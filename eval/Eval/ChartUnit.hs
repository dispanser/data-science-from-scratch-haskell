-- {-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Eval.ChartUnit
  where

import Chart
import Lens.Micro ((.~))
import NumHask.Prelude
import Data.Generics.Product (field)

-- data for simple bar chart
movies :: IsString a => [a]
movies = ["Annie Hall", "Ben-Hur", "Casablanca", "Gandhi", "West Side Story"]

numOscars :: Num a => [a]
numOscars = [5, 11, 3, 8, 10]

barExample :: Chart b
barExample  = barChart def (BarData [numOscars] Nothing Nothing) <>
  hud hudOpts sixbyfour (fold (abs <$> rs))
  where
    labels' = movies -- fmap T.pack <$> take 10 $ (:[]) <$> ['a'..]

    rs = rectBars 0.1 numOscars
    hudOpts :: HudOptions
    hudOpts =
      field @"titles" .~ titles' $ -- #titles .~ [(def,"Bar Chart")] $
      field @"axes" .~ [ field @"tickStyle" .~ TickLabels labels' $ def
                       , defYAxis ] $
      def

titles' :: [(TitleOptions, Text)]
titles' =
  [ (def, "Number of Oscars")
  , ( field @"text" . field @"size" .~ 0.08 $
      field @"align" .~ AlignRight $
      field @"place" .~ PlaceBottom $
      def
    , "oscars for each movie")
  ]

sStandard :: Pair Double
sStandard = Pair 600 400

defaultSvgOptions :: SvgOptions
defaultSvgOptions = SvgOptions sStandard "" [] True

main :: IO ()
main = do
  fileSvg "bar_example.svg" defaultSvgOptions barExample

