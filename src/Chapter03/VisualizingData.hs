{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Chapter03.VisualizingData

  where

import           Data.HashMap.Strict                   (HashMap, toList)
import           Data.Statistics                       (counter)
import           Graphics.Rendering.Chart.Axis.Indexed (autoIndexAxis)
import           Graphics.Rendering.Chart.Easy

-- data for simple line chart
years :: [Int]
years = [1950, 1960, 1970, 1980, 1990, 2000, 2010]

gdp :: [Double]
gdp = [300.2, 543.3, 1075.9, 2862.5, 5979.6, 10289.7, 14958.3]


-- data for simple bar chart
movies :: [String]
movies = ["Annie Hall", "Ben-Hur", "Casablanca", "Gandhi", "West Side Story"]

numOscars :: [Int]
numOscars = [5, 11, 3, 8, 10]

barvs :: [(Int, [Int])]
barvs = zipWith (\x y -> (x, [y])) [0..] numOscars

barChart :: Renderable ()
barChart = toRenderable $ do
  layout_title .= "Bollywool Oscars"
  layout_x_axis . laxis_override .= \_ -> autoIndexAxis movies [0..]
  plot $ plotBars <$> bars movies barvs

-- data for histogram
grades :: Num a => [a]
grades = [83,95,91,87,70,0,85,82,100,67,73,77,0]

decile :: Integral a => a -> a
decile n = n `div` 10 * 10

decileGrades :: HashMap Int Int
decileGrades = counter decile grades

barvsDeciles :: [(Int, [Int])]
barvsDeciles = decileGrades
  & toList
  & map (\x -> (fst x, [snd x]))

bars' :: PlotBars Int Int
bars' = plot_bars_values .~ barvsDeciles
      $ plot_bars_style  .~ BarsClustered -- is default
      $ plot_bars_alignment .~ BarsCentered
      $ def

hiddenPlot :: PlotHidden Int Int
hiddenPlot = PlotHidden {_plot_hidden_x_values = [-3, 103]
                        , _plot_hidden_y_values = [0, 0] }

-- throwing a hidden plot into the mix to force a larger range for x-axis.
expandedLayout :: Layout Int Int
expandedLayout = layout_title .~ "Distribution of Exam 1 Grades"
                 $ layout_x_axis . laxis_title .~ "Deciles"
                 $ layout_y_axis . laxis_title .~ "# of Students"
                 $ layout_plots .~ [ plotBars bars', toPlot hiddenPlot ]
                 $ def

histPlot :: PlotHist Double Int
histPlot = plot_hist_title .~ "Histogram Title"
         $ plot_hist_bins .~ 10
         $ plot_hist_values .~ grades
         $ plot_hist_range .~ Just (0, 100)
         $ def

histLayout :: Layout Double Int
histLayout = layout_title .~ "Layout Title"
           $ layout_plots .~ [ histToPlot histPlot ]
           $ def

-- toRenderable layout

-- line charts
variance, biasSquared, totalError :: [Int]
variance     = [1,2,4,8,16,32,64,128,256]
biasSquared = [256,128,64,32,16,8,4,2,1]
totalError  = zipWith (+) variance biasSquared

-- using the 'Easy' module
easyLineChart :: Renderable ()
easyLineChart = toRenderable $ do
  layout_title .= "The Bias-Variance Tradeoff"
  layout_x_axis . laxis_title .= "model complexity"
  plot $ line "variance" [zip [0 :: Int ..] variance]
  plot $ line "bias"     [zip [0..] biasSquared]
  plot $ line "total error" [zip [0..] totalError]

lineLayout :: Layout Int Int
lineLayout = layout_title .~  "The Bias-Variance Tradeoff"
       $ layout_plots .~ map toPlot [biasLinePlot, varianceLinePlot, totalErrorPlot]
       $ def
  where
    varianceLinePlot = plot_lines_style . line_dashes .~ [1, 5] $
                       plot_lines_title .~ "variance" $
                       plot_lines_style . line_color  .~ opaque green $
                       plot_lines_values .~ [ zip [0..] variance] $
                       def

    biasLinePlot = plot_lines_style . line_dashes .~ [2] $
                   plot_lines_title .~ "bias^2" $
                   plot_lines_style . line_color  .~ opaque red $
                   plot_lines_values .~ [ zip [0..] biasSquared] $
                   def

    totalErrorPlot = plot_lines_style . line_dashes .~ [1, 2] $
                     plot_lines_title .~ "total error" $
                     plot_lines_style . line_color  .~ opaque blue $
                     plot_lines_values .~ [ zip [0..] totalError] $
                     def

-- scatter plots

friends, minutes :: Num a => [a]
friends = [ 70, 65, 72, 63, 71, 64, 60, 64, 67]
minutes = [175, 170, 205, 120, 220, 130, 105, 145, 190]

labels :: [Char]
labels = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']

-- scatterPlot :: Renderable ()
-- scatterPlot = toRenderable annotatedPoints
--   where
--     annotatedPoints =
--       layout_title .~ "Daily Minutes vs Number of Friends" $
--       layout_x_axis . laxis_title .~ "# of friends" $
--       layout_y_axis . laxis_title .~ "daily minutes spent on the site" $
--       layout_plots .~ [toPlot pointsPlot, toPlot texts] $
--       def
--     pointsPlot = plot_points_values .~ zip friends minutes $
--                  plot_points_style . point_radius .~ 3 $
--                  def
--     texts = plot_annotation_values .~ zip3 ((+0.2) <$> friends) minutes ((:[]) <$> labels) $
--             plot_annotation_hanchor .~ HTA_Left $
--             plot_annotation_vanchor .~ VTA_Top $
--             def

-- more scatter plots: unequal axes

test1Grades, test2Grades :: Num a => [a]
test1Grades = [ 99, 90, 85, 97, 80]
test2Grades = [100, 85, 60, 90, 70]

data AxisRange a = AxisRange a a deriving (Show, Eq)

-- scatter plot combining a point plot with a hidden plot to enlarge the
-- viewport somewhat, otherwise some points lay directly on the axis and ticks.
scatterplotUnequalAxes :: Renderable ()
scatterplotUnequalAxes = toRenderable unequalScatterLayout
  where
    AxisRange minX maxX = scaleRange 1.1 $ computeRange (test1Grades :: [Double])
    AxisRange minY maxY = scaleRange 1.1 $ computeRange (test2Grades :: [Double])
    hidden = PlotHidden { _plot_hidden_x_values = [minX, maxX]
                        , _plot_hidden_y_values = [minY, maxY] }
    pointPlot = plot_points_values .~ zip (test1Grades :: [Double]) (test2Grades :: [Double]) $
                plot_points_style . point_radius .~ 2 $
                def
    unequalScatterLayout = layout_title .~ "Axes Aren't Comparable" $
                           layout_x_axis . laxis_title .~ "test 1 grade" $
                           layout_y_axis . laxis_title .~ "test 2 grade" $
                           layout_plots .~ [toPlot pointPlot, toPlot hidden] $ def

-- | produce a range that scaled by `factor`.
scaleRange :: Fractional a => a -> AxisRange a -> AxisRange a
scaleRange factor (AxisRange minV maxV) =
  let middle = (maxV + minV) / 2
  in AxisRange (middle - (middle - minV)*factor) (middle + (maxV - middle)*factor)

computeRange :: Ord a => [a] -> AxisRange a
computeRange xs = AxisRange (minimum xs) (maximum xs)

-- | given two ranges, produce new set of ranges that have equal width
alignRanges :: (Fractional a, Ord a) => AxisRange a -> AxisRange a -> (AxisRange a, AxisRange a)
alignRanges x@(AxisRange minX maxX) y@(AxisRange minY maxY)=
  let
    xRange = maxX - minX
    yRange = maxY - minY
    range  = max xRange yRange
    newXrange = if range == xRange then x else scaleRange (range / xRange) x
    newYrange = if range == yRange then y else scaleRange (range / yRange) y
  in (newXrange, newYrange)

scatterplotEqualAxes :: Renderable ()
scatterplotEqualAxes = toRenderable layout
  where
    xRange = scaleRange 1.1 $ computeRange (test1Grades :: [Double])
    yRange = scaleRange 1.1 $ computeRange (test2Grades :: [Double])
    (AxisRange minX maxX, AxisRange minY maxY) = alignRanges xRange yRange
    hidden = PlotHidden { _plot_hidden_x_values = [minX, maxX]
                        , _plot_hidden_y_values = [minY, maxY] }
    pointPlot = plot_points_values .~ zip (test1Grades :: [Double]) (test2Grades :: [Double]) $
                plot_points_style . point_radius .~ 2 $
                def
    layout = layout_title .~ "Axes Are Comparable" $
             layout_x_axis . laxis_title .~ "test 1 grade" $
             layout_y_axis . laxis_title .~ "test 2 grade" $
             layout_plots .~ [toPlot pointPlot, toPlot hidden] $ def
