{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Chart.Heatmap
import Control.Lens
import Control.Monad (forM_)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Colour.SRGB (sRGB, RGB(..))
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Complex (magnitude, realPart, imagPart, Complex(..))
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo (toFile, FileOptions(..), FileFormat(..))
import Data.Array.CArray.Base (unsafeForeignPtrToCArray, toForeignPtr, CArray(..), ixmapWithIndP)
import Data.Fixed (mod')
import Math.FFT (dftRC)
import Numeric.Signal (analytic_signal, auto_correlation, deriv, Filterable)
import Data.Maybe (catMaybes)
import Sound.MIDI.File
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as SFV
import qualified Sound.MIDI.File.Save as MS
import qualified Sound.MIDI.File.Event as ME
import qualified Sound.MIDI.Message.Channel as MC
import qualified Sound.MIDI.Message.Channel.Voice as MCV
import qualified Sound.MIDI.Message.Channel.Mode as MCM
import qualified Data.EventList.Relative.TimeBody as E

type Frequency = Double
type Amplitude = Double
type Step = Double

maxFrequency :: Double
maxFrequency = 7000

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

type Octave = Int

data Note = Note NoteLetter Octave deriving (Show, Eq)

getBaseFreq :: NoteLetter -> Frequency
getBaseFreq A   = 27.50
getBaseFreq Ais = 29.14
getBaseFreq B   = 30.87
getBaseFreq C   = 16.35
getBaseFreq Cis = 17.32
getBaseFreq D   = 18.35
getBaseFreq Dis = 19.45
getBaseFreq E   = 20.60
getBaseFreq F   = 21.83
getBaseFreq Fis = 23.12
getBaseFreq G   = 24.50
getBaseFreq Gis = 25.96

getFreq :: Note -> Frequency
getFreq (Note letter n) = getBaseFreq letter * (fromIntegral n + 1)

getNote :: Frequency -> Maybe Note
getNote freq
  | divisor A `mod'` 1 < 0.02 || divisor A `mod'` 1 > 0.98 = Just
    (Note A (findN freq A))
  | divisor Ais `mod'` 1 < 0.02 || divisor Ais `mod'` 1 > 0.98 = Just
    (Note Ais (findN freq Ais))
  | divisor B `mod'` 1 < 0.02 || divisor B `mod'` 1 > 0.98 = Just
    (Note B (findN freq B))
  | divisor C `mod'` 1 < 0.02 || divisor C `mod'` 1 > 0.98 = Just
    (Note C (findN freq C))
  | divisor Cis `mod'` 1 < 0.02 || divisor Cis `mod'` 1 > 0.98 = Just
    (Note Cis (findN freq Cis))
  | divisor D `mod'` 1 < 0.02 || divisor D `mod'` 1 > 0.98 = Just
    (Note D (findN freq D))
  | divisor Dis `mod'` 1 < 0.02 || divisor Dis `mod'` 1 > 0.98 = Just
    (Note Dis (findN freq Dis))
  | divisor E `mod'` 1 < 0.02 || divisor E `mod'` 1 > 0.98 = Just
    (Note E (findN freq E))
  | divisor F `mod'` 1 < 0.02 || divisor F `mod'` 1 > 0.98 = Just
    (Note F (findN freq F))
  | divisor Fis `mod'` 1 < 0.02 || divisor Fis `mod'` 1 > 0.98 = Just
    (Note Fis (findN freq Fis))
  | divisor G `mod'` 1 < 0.02 || divisor G `mod'` 1 > 0.98 = Just
    (Note G (findN freq G))
  | divisor Gis `mod'` 1 < 0.02 || divisor Gis `mod'` 1 > 0.98 = Just
    (Note Gis (findN freq Gis))
  | otherwise = Nothing
 where
  findN f n = round $ logBase 2 (f / getBaseFreq n) :: Int
  divisor note = logBase 2 (freq / getBaseFreq note)

-- samples = N = number of samples
-- n = sample number
hannWindow :: Integer -> Integer -> Double
hannWindow samples n =
  0.5 * (1 - cos (2 * pi * fromIntegral n / (fromIntegral samples - 1)))

sinFunc :: Double -> Double -> Double -> Double -> Double
sinFunc samples cycle amplitude p =
  -amplitude * sin (2 * pi * cycle * p / samples)

cosFunc :: Double -> Double -> Double -> Double -> Double
cosFunc samples cycle amplitude p =
  amplitude * cos (2 * pi * cycle * p / samples)

performFFT :: VS.Vector Double -> Int -> IO (VS.Vector (Complex Double))
performFFT vector intSamples = do
  let (vec, x1, x2) = VS.unsafeToForeignPtr $ VS.take intSamples vector
  carray <- unsafeForeignPtrToCArray vec (x1, x2)
  let fft           = dftRC carray
  let (len, fftptr) = toForeignPtr fft
  return (VS.unsafeFromForeignPtr fftptr 0 len)

-- |Filter an array of maxima by partioning fftabs into bins of 100 elements, then finding the
-- largest value in that bin.
-- peaks are the list of indexes of the maxima.
filterMaxima :: V.Vector Int -> V.Vector Double -> V.Vector Int
filterMaxima peaks fftabs
  | V.null peaks = V.empty
  | otherwise = V.filter (> -1) $ V.map
    (\i ->
      let
        section = V.map
          (\x -> (x, fftabs V.! x))
          (V.filter (\x -> x < V.length fftabs && x >= i && x < i + binSize)
                    peaks
          )
      in  if V.null section
            then -1
            else fst $ V.maximumBy (comparing snd)
            -- list of (index, magnitude)
                                                   section
    )
    (V.fromList [0, binSize .. V.last peaks])
  where binSize = 100

-- | Find the maxima from fftabs by finding all points where the left and right points
-- are less than the current point.
findMaxima :: V.Vector Double -> V.Vector Int
findMaxima fftabs = V.filter
  (\x -> (diffs' V.! x) > 0 && (diffs' V.! (x + 1)) < 0)
  (V.fromList [1 .. (V.length diffs' - 2)])
 where
  diff f x = f x - f (x - 1)
  diffs' = V.map (diff (fftabs V.!)) (V.fromList [1 .. (V.length fftabs - 1)])

-- | Find maxima by finding points where that point is the maximum in it's neighbourhood.
findMaxima2 :: Filterable a => Int -> V.Vector a -> V.Vector (Int, a)
findMaxima2 radius v =
  V.filter (\(i, x) -> x == V.maximum (neighbours i))
    $ V.imap (\i x -> (i, x)) v
  where neighbours x0 = V.ifilter (\i _ -> abs (x0 - i) <= radius) v

findSignificantNotesForSection
  :: Int -> Int -> V.Vector (Complex Double) -> [(Frequency, Double)]
findSignificantNotesForSection sampleRate samples fftSection = fmap
  (\cycle ->
    ( fromIntegral cycle * fromIntegral sampleRate / fromIntegral samples
    , fftabs V.! cycle
    )
  )
  maxima'
 where
  fftabs  = V.map magnitude fftSection :: V.Vector Double
  peaks   = findMaxima fftabs
  peaks'  = filterMaxima peaks fftabs
  maxima  = V.toList peaks' -- tricky: the cycles returned is the cycles+1
  maxima' = filter (\value -> fftabs V.! value > 20) maxima -- Find 'large' maxima

midiPitch :: Note -> Int
midiPitch (Note letter n) =
  round $ 12 * logBase 2 (((2 ^ n) * getBaseFreq letter) / 440) + 69

-- | Find notes that standout in each stft section.
findSignificantNotes
  :: Int -> Int -> [(Step, [Complex Double])] -> String -> IO ()
findSignificantNotes sampleRate samples stft filePrefix = do
  notes <-
    mapM
      (\(step, fft) -> do
        let freqs =
              findSignificantNotesForSection sampleRate samples (V.fromList fft)
        let notes = fmap (\(f, _) -> getNote f)
                         (filter (\(f, _) -> f < maxFrequency) freqs)
        return $ map (\note -> (step, note)) $ catMaybes notes
      )
      stft :: IO [[(Step, Note)]]

  print $ map (map (\(s, note) -> (s, note, midiPitch note))) notes

  let
    sections = map
      (foldl
        (\acc (step, note) -> E.append
          acc
          (E.singleton
            (toElapsedTime 0)
            (ME.MIDIEvent
              (MC.Cons
                (MC.toChannel 1)
                (MC.Voice
                  (MCV.NoteOn (MC.toPitch (midiPitch note)) (MC.toVelocity 20))
                )
              )
            )
          )
        )
        E.empty
      )
      notes

  let
    t =
      E.append
          (foldl (\acc section -> E.append acc (E.delay 100 section))
                 E.empty
                 sections
          )
        $ E.singleton
            (toElapsedTime 100)
            (ME.MIDIEvent (MC.Cons (MC.toChannel 1) (MC.Mode MCM.AllNotesOff)))

  let midi = Cons Serial (Ticks 120) [t] :: T

  MS.toFile (filePrefix ++ ".midi") midi

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

autoCorrelate :: Filterable a => VS.Vector a -> Int -> IO ()
autoCorrelate vector sampleRate = do
  let result = auto_correlation 1000 vector
  let y      = map realToFrac $ VS.toList result :: [Double]
  --let values = [zip (map (\x -> x / fromIntegral sampleRate) [1..]) y] :: [[(Double, Double)]]
  let values = [zip [1 ..] y] :: [[(Double, Double)]]
  toFile def { _fo_format = SVG } "autocorrelation.svg" $ do
    layout_title .= "auto correlation"
    plot (line "" values)

  let localMaxima = findMaxima2 10 (VS.convert result)
  print $ V.map (\(i, x) -> (i, realToFrac x)) localMaxima
  print $ V.imap
    (\j (i, _) -> if j == 0 then j else i - fst (localMaxima V.! (j - 1)))
    localMaxima
  print $ fromIntegral sampleRate / fromIntegral
    (V.maximum
      (V.imap
        (\j (i, _) -> if j == 0 then j else i - fst (localMaxima V.! (j - 1)))
        localMaxima
      )
    )

graphSignal :: Filterable a => VS.Vector a -> Int -> IO ()
graphSignal vector sampleRate = do
  let intList = map realToFrac $ VS.toList vector :: [Double]
  let y       = intList
  let x       = map (\x -> x / fromIntegral sampleRate) [1 .. 200] :: [Double]
  toFile def { _fo_format = SVG } "signal.svg" $ do
    layout_x_axis . laxis_title .= "Elapsed time (s)"
    layout_title .= "signal curve"
    plot (line "" [zip x y])

graphFFTSignal :: VS.Vector (Complex Double) -> Int -> Int -> IO ()
graphFFTSignal fftvec samples sampleRate = do
  let fftreals = VS.toList $ VS.map realPart fftvec :: [Double]
  let fftimg   = VS.toList $ VS.map imagPart fftvec :: [Double]
  let fftabs   = VS.toList $ VS.map magnitude fftvec :: [Double]
  let fftx =
        map (\x -> fromIntegral (x * sampleRate) / fromIntegral samples)
            [0 .. 1000] :: [Double]
  toFile def { _fo_format = SVG } "fft.svg" $ do
    layout_title .= "fft curve"
    layout_x_axis . laxis_title .= "Frequency (Hz)"
    plot (points "abs" $ zip fftx fftabs)
    plot (points "real" $ zip fftx fftreals)
    plot (points "imaginary" $ zip fftx fftimg)

data Options = Options {
    sourceFile :: String,
    stepWidth :: Int
}

args :: ParserInfo Options
args = info
  (Options <$> strOption (long "src" <> metavar "SOURCE") <*> option
    auto
    (long "step-width" <> metavar "STEP_WIDTH" <> value 10000 <> showDefault)
  )
  (fullDesc <> progDesc "Desc" <> header "test")

main :: IO ()
main = do
  opts         <- execParser args
  (i, content) <-
    SF.readFile (sourceFile opts) :: IO (SF.Info, Maybe (SFV.Buffer Double))
  let sampleRate = SF.samplerate i
  let samples    = SF.frames i * SF.channels i
  print samples
  print i
  let v = fmap SFV.fromBuffer content
  case v of
    Nothing     -> print "Nothing!"
    Just vector -> do
      graphSignal vector sampleRate

      -- Run FFT on whole dataset and plot the result
      fftvec <- performFFT vector samples
      graphFFTSignal fftvec samples sampleRate
      findSignificantNotes sampleRate samples [(0, VS.toList fftvec)] "fft"

      -- get auto correlation
      autoCorrelate vector sampleRate

      -- plot histogram
      let (vec, x1, x2) = VS.unsafeToForeignPtr vector
      carray <- unsafeForeignPtrToCArray vec (x1, x2)
      --let stepWidth = fromIntegral samples :: Integer
      let stepWidth = 5000 :: Integer
      print $ "Step width: " ++ show stepWidth
      let cutSamples =
            (samples `div` fromIntegral stepWidth) * fromIntegral stepWidth :: Int

      -- windows of size stepWidth
      let stft =
            fmap (stftSection carray cutSamples stepWidth)
                 [0, stepWidth `div` 2 .. fromIntegral cutSamples] :: [ ( Step
                , [Complex Double]
                )
              ]

      findSignificantNotes sampleRate cutSamples stft "stft"

      -- (window start, fft for window)
      let plots' =
            concatMap
              (spectrogramIntensity (fromIntegral cutSamples) sampleRate)
              stft :: [(Step, Frequency, Amplitude)]

      let stftData  = generateStftData (fromIntegral stepWidth) plots'
      let stftData' = filter (\((_, _), (_, _), z) -> z > 1) stftData
      toFile def { _fo_format = PNG } "stft.png" $ do
        layout_title .= "curve"
        plot
          (liftEC $ do
            plot_rect_title .= "title"
            plot_rect_style .= \amp ->
              let RGB a b c = hsl ((1 - amp) * 240) 1 (amp * 0.6)
              in  solidFillStyle (opaque $ sRGB a b c)
            plot_rect_values .= stftData'
          )

      return ()

-- convert to plottable set of values
generateStftData
  :: Step
  -> [(Step, Frequency, Amplitude)]
  -> [((Double, Double), (Double, Double), Amplitude)]
generateStftData windowSize = map (generateColumn windowSize)

-- column in spectrogram
generateColumn
  :: Step
  -> (Step, Frequency, Amplitude)
  -> ((Double, Double), (Double, Double), Amplitude)
generateColumn windowSize (step, freq, amp) =
  ((step, windowSize + step), (freq, freq + 1), amp)

spectrogramIntensity
  :: Double -> Int -> (Step, [Complex Double]) -> [(Step, Frequency, Amplitude)]
spectrogramIntensity totalSamples sampleRate (step, fft) =
  let rate = fromIntegral sampleRate / totalSamples
  in  filter
        (\(_, f, _) -> f < 4000)
        (map (\(cycle, x) -> (step, cycle * rate, magnitude (x + 0) ** 2))
             (zip [0 ..] fft)
        )

ftSignals :: Double -> [Complex Double] -> [Double] -> [Double]
ftSignals totalSamples fft = fmap
  (\x ->
    foldr
        (\(cycle, amplitude) total ->
          total + sinFunc totalSamples cycle amplitude x
        )
        0
        sinAmplitudes
      + foldr
          (\(cycle, amplitude) total ->
            total + cosFunc totalSamples cycle amplitude x
          )
          0
          cosAmplitudes
  )
 where
  significantSignals = filter (\x -> magnitude x > 5) fft
  cycles             = zip [0 ..] significantSignals
  sinAmplitudes =
    fmap
      (\(cycle, signal) ->
        (fromIntegral cycle, realPart signal / (totalSamples / 2.0))
      )
      cycles :: [(Double, Double)]
  cosAmplitudes =
    fmap
      (\(cycle, signal) ->
        (fromIntegral cycle, imagPart signal / (totalSamples / 2.0))
      )
      cycles :: [(Double, Double)]

-- | Apply window function to the set of samples
windowedSection
  :: CArray Int Double -> Int -> Integer -> Integer -> CArray Int Double
windowedSection data_ totalSamples n stepWidth = ixmapWithIndP
  (0, totalSamples)
  id
  (\_ x i' -> if i' < fromIntegral n || i' >= fromIntegral (n + stepWidth)
    then 0
    else x * hannWindow (fromIntegral stepWidth) (fromIntegral i' - n)
  )
  data_

-- | n = start of step sample number
stftSection
  :: CArray Int Double -> Int -> Integer -> Integer -> (Step, [Complex Double])
stftSection !completeSample totalSamples stepWidth n =
  (fromIntegral n, VS.toList $ VS.unsafeFromForeignPtr fftptr 0 len)
 where
  fft = dftRC (windowedSection completeSample totalSamples n stepWidth)
  (len, fftptr) = toForeignPtr fft
