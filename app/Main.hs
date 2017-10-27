{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Lens
import Control.Monad (forM_)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Complex (magnitude, realPart, imagPart, Complex(..))
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo (toFile, FileOptions(..), FileFormat(..))
import Data.Array.CArray.Base (unsafeForeignPtrToCArray, toForeignPtr, CArray(..), ixmapWithIndP)
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Fixed (mod')
import Math.FFT (dftRC)
import Numeric.Signal (analytic_signal)
import Data.Maybe (catMaybes)
import Sound.MIDI.File
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as SFV
import qualified Sound.MIDI.File.Event as ME
import qualified Sound.MIDI.Message.Channel as MC
import qualified Sound.MIDI.Message.Channel.Voice as MCV
import qualified Data.EventList.Relative.TimeBody as E

type Frequency = Double
type Amplitude = Double
type Step = Double

data NoteLetter = A
        | Ais
        | B
        | C
        | Cis
        | D
        | Dis
        | E
        | F
        | Fis
        | G
        | Gis
        deriving (Show, Eq)

data Note = Note NoteLetter Int deriving (Show, Eq)

getBaseFreq :: NoteLetter -> Frequency
getBaseFreq A = 27.50
getBaseFreq Ais = 29.14
getBaseFreq B = 30.87
getBaseFreq C = 16.35
getBaseFreq Cis = 17.32
getBaseFreq D = 18.35
getBaseFreq Dis = 19.45
getBaseFreq E = 20.60
getBaseFreq F = 21.83
getBaseFreq Fis = 23.12
getBaseFreq G = 24.50
getBaseFreq Gis = 25.96

getFreq :: Note -> Frequency
getFreq (Note letter n) = getBaseFreq letter * (fromIntegral n + 1)

getNote :: Frequency -> Maybe Note
getNote freq
    | logBase 2 (freq / getBaseFreq A) `mod'` 1 < 0.02  || logBase 2 (freq / getBaseFreq A) `mod'` 1 > 0.98 = Just (Note A (findN freq A))
    | logBase 2 (freq / getBaseFreq Ais) `mod'` 1 < 0.02  || logBase 2 (freq / getBaseFreq Ais) `mod'` 1 > 0.98 = Just (Note Ais (findN freq Ais))
    | logBase 2 (freq / getBaseFreq B) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq B) `mod'` 1 > 0.98 = Just (Note B (findN freq B))
    | logBase 2 (freq / getBaseFreq C) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq C) `mod'` 1 > 0.98 = Just (Note C (findN freq C))
    | logBase 2 (freq / getBaseFreq Cis) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq Cis) `mod'` 1 > 0.98 = Just (Note Cis (findN freq Cis))
    | logBase 2 (freq / getBaseFreq D) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq D) `mod'` 1 > 0.98 = Just (Note D (findN freq D))
    | logBase 2 (freq / getBaseFreq Dis) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq Dis) `mod'` 1 > 0.98 = Just (Note Dis (findN freq Dis))
    | logBase 2 (freq / getBaseFreq E) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq E) `mod'` 1 > 0.98 = Just (Note E (findN freq E))
    | logBase 2 (freq / getBaseFreq F) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq F) `mod'` 1 > 0.98 = Just (Note F (findN freq F))
    | logBase 2 (freq / getBaseFreq Fis) `mod'` 1 < 0.02  || logBase 2 (freq / getBaseFreq Fis) `mod'` 1 > 0.98 = Just (Note Fis (findN freq Fis))
    | logBase 2 (freq / getBaseFreq G) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq G) `mod'` 1 > 0.98 = Just (Note G (findN freq G))
    | logBase 2 (freq / getBaseFreq Gis) `mod'` 1 < 0.02 || logBase 2 (freq / getBaseFreq Gis) `mod'` 1 > 0.98 = Just (Note Gis (findN freq Gis))
    | otherwise = Nothing
    where findN f n = round $ logBase 2 (f / getBaseFreq n) :: Int

data PlotRects z x y = PlotRects {
    _plot_rect_title :: String,
    _plot_rect_style :: z -> FillStyle,
    _plot_rect_values :: [((x, x), (y, y), z)]
}

instance (PlotValue z, Real z) => Default (PlotRects z x y) where
    def = PlotRects {
        _plot_rect_title = "Default",
        _plot_rect_style = \amp -> solidFillStyle (opaque $ sRGB (realToFrac $ 1 - amp) 0 (realToFrac amp)),
        _plot_rect_values = []
    }

instance (PlotValue z, Real z, Fractional z, Floating z) => ToPlot (PlotRects z) where
    toPlot p = Plot {
        _plot_render = renderPlotRects p,
        _plot_legend = [(_plot_rect_title p, renderPlotLegendRects p)],
        _plot_all_points = (map (\((x,_), (_,_), _) -> x) pts ++ map (\((_, x), (_,_), _) -> x) pts, map (\((_,_), (y,_), _) -> y) pts ++ map (\((_,_), (_,y), _) -> y) pts)
    }
        where pts = _plot_rect_values p

buildRectPath :: PointMapFn x y -> ((x,x),(y,y), z) -> Path
buildRectPath pmap ((xLeft, xRight), (yBottom, yTop), _) =
    MoveTo (pt xLeft yBottom) (
    LineTo (pt xLeft yTop) (
    LineTo (pt xRight yTop) (
    LineTo (pt xRight yBottom) (
    LineTo (pt xLeft yBottom) End))))
    where pt x y = pmap (LValue x, LValue y)

renderPlotRects :: (Real z, Fractional z, Floating z) => PlotRects z x y -> PointMapFn x y -> BackendProgram ()
renderPlotRects p pmap =
    forM_ values (\point -> withFillStyle (_plot_rect_style p (getPercentage point)) $ alignFillPath (buildRectPath pmap point) >>= fillPath)
    where pmap' = mapXY pmap
          values = _plot_rect_values p
          ((_,_), (_,_), maxAmp) = maximumBy (\(_, _, z1) (_, _, z2) -> compare z1 z2) values
          getPercentage ((_, _), (_, _), amp) = amp / maxAmp -- arbitrary scaling

renderPlotLegendRects :: (Real z, Fractional z) => PlotRects z x y -> Rect -> BackendProgram ()
renderPlotLegendRects p r = withFillStyle (_plot_rect_style p 0.5) $ fillPath (rectPath r)

makeLenses ''PlotRects

-- samples = N = number of samples
-- n = sample number
hannWindow :: Integer -> Integer -> Double
hannWindow samples n = 0.5 * (1 - cos (2 * pi * fromIntegral n / (fromIntegral samples - 1)))

sinFunc :: Double -> Double -> Double -> Double -> Double
sinFunc samples cycle amplitude p = - amplitude * sin (2 * pi * cycle * p / samples)

cosFunc :: Double -> Double -> Double -> Double -> Double
cosFunc samples cycle amplitude p = amplitude * cos (2 * pi * cycle * p / samples)

performFFT :: VS.Vector Double -> Int -> IO (CArray Int Double, VS.Vector (Complex Double))
performFFT vector intSamples = do
    let (vec, x1, x2) = VS.unsafeToForeignPtr $ VS.take intSamples vector
    carray <- unsafeForeignPtrToCArray vec (x1, x2)
    let fft = dftRC carray
    let (len, fftptr) = toForeignPtr fft
    return (carray, VS.unsafeFromForeignPtr fftptr 0 len)

filterMaxima :: V.Vector Int -> V.Vector Double -> V.Vector Int
filterMaxima peaks fftabs 
    | V.null peaks = V.empty
    | otherwise = V.filter (> -1) $ V.map 
            (\i -> let section = V.map 
                            (\x -> (x, fftabs V.! x)) 
                            (V.filter (\x -> x < V.length fftabs && x >= i && x < i + 100) peaks)
                   in if V.null section then -1 else fst $ V.maximumBy
                        (comparing snd)
                        -- list of (index, magnitude)
                        section)
            (V.fromList [0,100..V.last peaks])

findMaxima :: V.Vector Double -> V.Vector Int
findMaxima fftabs = V.filter 
            (\x -> (diffs' V.! x) > 0 && (diffs' V.! (x + 1)) < 0) 
            (V.fromList [1..(V.length diffs' - 2)])
    where 
            diff f x = f x - f (x - 1) 
            diffs' = V.map (diff (fftabs V.!)) (V.fromList [1..(V.length fftabs - 1)])

findSignificantNotesForSection :: Int -> Int -> V.Vector (Complex Double) -> [(Frequency, Double)]
findSignificantNotesForSection sampleRate samples fftSection = fmap 
            (\cycle -> (fromIntegral (snd cycle) * fromIntegral sampleRate / fromIntegral samples, fftabs V.! snd cycle)) 
            maxima'
        where
            fftabs = V.map magnitude fftSection :: V.Vector Double
            peaks = findMaxima fftabs
            peaks' = filterMaxima peaks fftabs
            maxima = zip [0..V.length peaks'] (V.toList peaks') -- tricky: the cycles returned is the cycles+1
            maxima' = filter (\(_, value) -> fftabs V.! value > 10) maxima

findSignificantNotes :: Int -> Int -> [(Step, [Complex Double])] -> IO ()
findSignificantNotes sampleRate samples = mapM_ (\(_, fft) -> do
    let freqs = findSignificantNotesForSection sampleRate samples (V.fromList fft)
    let notes = fmap (\(f, _) -> getNote f) (filter (\(f, _) -> 7903 > f) freqs)
    print $ catMaybes notes
    let midi = Cons Serial (Ticks 60) [E.singleton 1 (ME.MIDIEvent (MC.Cons (MC.toChannel 1) (MC.Voice (MCV.NoteOn (MC.toPitch 60) (MC.toVelocity 60)))))]
    print midi)

    --let intList = take samples $ VS.toList vector :: [Double]
    --let y = intList
    --let x = [0..fromIntegral samples - 1] :: [Double]

--    toFile def{_fo_format=SVG} "sine.svg" $ do
--        layout_title .= "sine curve"
--        plot (line "" [zip x y])

--    (carray, fftvec) <- performFFT vector samples

--    let fftvals = VS.toList fftvec
--    let fftreals = VS.map realPart fftvec :: VS.Vector Double
--    let fftimg = VS.map imagPart fftvec :: VS.Vector Double
--    let fftabs = VS.map magnitude fftvec :: VS.Vector Double
--    let fftx = [0..10000] :: [Double]
--    toFile def{_fo_format=SVG} "real.svg" $ do
--        layout_title .= "real fft curve"
--        plot (points "abs" $ zip fftx fftabs)
--        plot (points "real" $ zip fftx fftreals)
--        plot (points "imaginary" $ zip fftx fftimg)
--    toFile def{_fo_format=SVG} "diff.svg" $ do
--        layout_title .= "First derivative"
--        plot (points "diff" $ zip fftx (VS.toList fftabs))

--    print $ V.map (\x -> (x, fftabs VS.! x)) peaks'
    --toFile def{_fo_format=SVG} "diff2.svg" $ do
    --    layout_title .= "Second derivative"
    --    plot (points "diff" $ zip fftx diffs'')

    --let sinAmplitudes = take 500 $ fmap (\(cycle, _) -> (fromIntegral cycle, fftimg   VS.! cycle / fromIntegral (samples `div` 2))) maxima :: [(Double, Double)]
    --let cosAmplitudes = take 500 $ fmap (\(cycle, _) -> (fromIntegral cycle, fftreals VS.! cycle / fromIntegral (samples `div` 2))) maxima :: [(Double, Double)]

    --print "sin amps"
    --print sinAmplitudes
    --print "cos amps"
    --print cosAmplitudes

    --toFile def{_fo_format=SVG} "sine2.svg" $ do
    --    layout_title .= "curve"
    --    plot (line "data" [zip x y])
    --    plot
    --        (line "fit"
    --        [zip
    --            x
    --            (fmap
    --                (\p -> foldr (\(cycle, amplitude) total -> total + sinFunc (fromIntegral samples) cycle amplitude p) 0 sinAmplitudes +
    --                    foldr (\(cycle, amplitude) total -> total + cosFunc (fromIntegral samples) cycle amplitude p) 0 cosAmplitudes) x)])

    --let signal = VS.take samples $ analytic_signal vector
    --let signalReal = VS.toList $ VS.map realPart signal
    --let signalImag = VS.toList $ VS.map imagPart signal
    ----print $ VS.length signal
    --toFile def{_fo_format=SVG} "signal.svg" $ do
    --    layout_title .= "Hibert transform derivative"
    --    plot (line "Hilbert transform" [zip x (VS.toList $ VS.map imagPart signal)])
    --    plot (line "Original signal" [zip x intList])
    --    plot (line "Analytic signal" [zip x $ VS.toList (VS.map magnitude signal)])

main :: IO ()
main = do
    (i, content) <- SF.readFile "piano.wav" :: IO (SF.Info, Maybe (SFV.Buffer Double))
    let sampleRate = SF.samplerate i
    let samples = SF.frames i * SF.channels i
    print samples
    print i
    let v = fmap SFV.fromBuffer content
    case v of
        Nothing     -> print "Nothing!"
        Just vector -> do
            -- plot histogram
            let (vec, x1, x2) = VS.unsafeToForeignPtr vector
            carray <- unsafeForeignPtrToCArray vec (x1, x2)
            let stepWidth = 5000 :: Integer
            print $ "Step width: " ++ show stepWidth
            let cutSamples = (samples `div` fromIntegral stepWidth) * fromIntegral stepWidth :: Int

            -- windows of size stepWidth
            let stft = fmap 
                    (stftSection carray cutSamples stepWidth) 
                    [0, stepWidth `div` 2..fromIntegral cutSamples] :: [(Step, [Complex Double])]

            findSignificantNotes sampleRate cutSamples stft

            -- (window start, fft for window)
            let plots' = concatMap
                    (spectrogramIntensity (fromIntegral cutSamples) sampleRate) 
                    stft :: [(Step, Frequency, Amplitude)]

            let stftData = generateStftData (fromIntegral stepWidth) plots'
            let stftData' = filter (\((_, _), (_, _), z) -> z > 1) stftData

--            let ((_,_), (_,_), maxAmp) = maximumBy (\(_, _, z1) (_, _, z2) -> compare z1 z2) stftData'
--            let getPercentage ((_, _), (_, _), amp) = amp ** 2 / maxAmp

            --print $ map getPercentage stftData'
            --print maxAmp
            --print $ length stft

            toFile def{_fo_format=PNG} "stft.png" $ do
                layout_title .= "curve"
                plot (liftEC $ do
                    plot_rect_title .= "title"
                    plot_rect_style .= \amp -> let RGB a b c = hsl ((1 - amp) * 240) 1 (amp * 0.6) in solidFillStyle (opaque $ sRGB a b c)
                    plot_rect_values .= stftData')
                --mapM_
                --    (\data_ ->
                --        plot (liftEC $ do
                --            plot_points_title .= "title"
                --            plot_points_values .= stuff))
                --    plots'

            return ()

-- convert to plottable set of values
generateStftData :: Step -> [(Step, Frequency, Amplitude)] -> [((Double, Double), (Double, Double), Amplitude)]
generateStftData windowSize = map (generateColumn windowSize)

-- column in spectrogram
generateColumn :: Step -> (Step, Frequency, Amplitude) -> ((Double, Double), (Double, Double), Amplitude)
generateColumn windowSize (step, freq, amp) = ((step, windowSize + step), (freq, freq + 1), amp)

spectrogramIntensity :: Double -> Int -> (Step, [Complex Double]) -> [(Step, Frequency, Amplitude)]
spectrogramIntensity totalSamples sampleRate (step, fft) = 
        let rate = fromIntegral sampleRate / totalSamples
        in filter (\(_, f,_ ) -> f < 4000) (map (\(cycle, x) -> (step, cycle * rate, magnitude x ** 2)) (zip [0..] fft))

ftSignals :: Double -> [Complex Double] -> [Double] -> [Double]
ftSignals totalSamples fft = fmap (\x -> foldr (\(cycle, amplitude) total -> total + sinFunc totalSamples cycle amplitude x) 0 sinAmplitudes +
                                foldr (\(cycle, amplitude) total -> total + cosFunc totalSamples cycle amplitude x) 0 cosAmplitudes)
    where significantSignals = filter (\x -> magnitude x > 5) fft
          cycles = zip [0..] significantSignals
          sinAmplitudes = fmap (\(cycle, signal) -> (fromIntegral cycle, realPart signal / (totalSamples / 2.0))) cycles :: [(Double, Double)]
          cosAmplitudes = fmap (\(cycle, signal) -> (fromIntegral cycle, imagPart signal / (totalSamples / 2.0))) cycles :: [(Double, Double)]

-- | Apply window function to the set of samples
windowedSection :: CArray Int Double -> Int -> Integer -> Integer -> CArray Int Double
windowedSection data_ totalSamples n stepWidth = ixmapWithIndP (0, totalSamples) id (\_ x i' ->
    if i' < fromIntegral n || i' >= fromIntegral (n + stepWidth)
    then 0
    else x * hannWindow (fromIntegral stepWidth) (fromIntegral i' - n)) data_

-- | n = start of step sample number
stftSection :: CArray Int Double -> Int -> Integer -> Integer -> (Step, [Complex Double])
stftSection !completeSample totalSamples stepWidth n = (fromIntegral n, VS.toList $ VS.unsafeFromForeignPtr fftptr 0 len)
            where fft = dftRC (windowedSection completeSample totalSamples n stepWidth)
                  (len, fftptr) = toForeignPtr fft
