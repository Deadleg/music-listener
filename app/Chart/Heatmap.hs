{-# LANGUAGE TemplateHaskell #-}

module Chart.Heatmap (
    PlotRects(..),
    plot_rect_title,
    plot_rect_style,
    plot_rect_values
) where

import Control.Lens (makeLenses)
import Control.Monad (forM_)
import Graphics.Rendering.Chart.Easy
import Data.Colour.SRGB (sRGB)
import Data.List (maximumBy)

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
    where --pmap' = mapXY pmap
          values = _plot_rect_values p
          ((_,_), (_,_), maxAmp) = maximumBy (\(_, _, z1) (_, _, z2) -> compare z1 z2) values
          getPercentage ((_, _), (_, _), amp) = amp / maxAmp -- arbitrary scaling

renderPlotLegendRects :: (Real z, Fractional z) => PlotRects z x y -> Rect -> BackendProgram ()
renderPlotLegendRects p r = withFillStyle (_plot_rect_style p 0.5) $ fillPath (rectPath r)

makeLenses ''PlotRects