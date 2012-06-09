-- module Main
module Histogram
    where

import AI.MathTmp

import Data.List
import Data.Monoid
import Debug.Trace
import qualified Data.Map as Map

import Numeric

import System.Random
import Control.Monad.Random

import qualified Graphics.Gnuplot.Advanced as Plot
-- import qualified Graphics.Gnuplot.Terminal.X11 as X11
-- import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as Color

-------------------------------------------------------------------------------

data Histogram = Histogram Double Double [(Double,Int)]
    deriving Show

-------------------------------------------------------------------------------
-- create the histogram data

   -- |creates the histogram datatype
histogram :: [Double] -> Histogram
histogram xs = histogramNumBins (round $ sqrt $ fromIntegral $ length xs) xs

histogramBinSize :: Double -> [Double] -> Histogram
histogramBinSize size xs = Histogram m s $ fillhist size $ histbin size $ bin size xs
    where
        m = mean xs
        s = stddev xs

histogramNumBins :: Int -> [Double] -> Histogram
histogramNumBins n xs = -- trace (show size) $ Histogram 0 0 []
    histogramBinSize size xs
    where
        size = (fromIntegral $ firstdigit diff) *((10) ** (fromIntegral $ exponent10 diff))
        diff = ((maximum xs)-(minimum xs))/(fromIntegral n)

        firstdigit dbl = floor $ dbl/((10) ** (fromIntegral $ exponent10 dbl))
        exponent10 dbl = floor $ log10 dbl
        log10 x = (log x) / (log 10)

-- helpers

   -- | histbin does all the binning for the histogram
histbin :: Double -> [Double] -> [(Double,Int)]
histbin size xs = Map.toList $ Map.fromList [ (head l, length l) | l <- group (sort xs) ]

   -- | histbin bins all the numbers in the histogram, but it ignores any columns with zero elements.
   -- fillhist adds those zero element columns
fillhist :: Double -> [(Double,Int)] -> [(Double,Int)]
fillhist size ((a,b):[]) = [(roundFloat a,b)]
fillhist size ((a,b):xs) = 
    if abs (next-a')<0.0001
       then (roundFloat a,b):(fillhist size xs)
       else (roundFloat a,b):(fillhist size $ (next,0):xs)
    where
        a' = fst $ head xs
        b' = snd $ head xs
        next = roundFloat (a+size)

   -- | bin "rounds" every number into the closest number below it that is divisible by size
-- bin :: (Num a, RealFrac a) => a -> [a] -> [a]
bin :: Double -> [Double] -> [Double]
bin size xs = map (\x -> size*(fromIntegral $ floor (x/size))) xs

roundFloat :: Double -> Double
roundFloat num = read $ showFFloat (Just 3) num ""

-------------------------------------------------------------------------------
-- IO

hist2svg :: String -> Histogram -> IO ()
hist2svg file histdata = do
    sequence_ 
        [ Plot.plot (SVG.cons $ file) $ histgen histdata
        ]
        
-- histgen :: Histogram -> Frame.T (Graph2D.T Double Double)
histgen (Histogram m s xs) =
    Frame.cons (
        Opts.title "I'm a histogram and I'm okay.  I show you data and I hide the pain." $
        Histogram.clusteredGap 0 $
--       Opts.boxwidthRelative 5 $
        OptsStyle.fillBorderLineType (-1) $
        OptsStyle.fillSolid $
        Histogram.clustered $
        Opts.xTicks2d xlabels $
        Opts.deflt) $
    mconcat $ concat 
        -- this is the bar chart
        [ map (\(title,dat) ->
            fmap (Graph2D.lineSpec (LineSpec.title title LineSpec.deflt)) $
            Plot2D.list Graph2D.histograms dat) $
                ("", map (fromIntegral . snd) xs) :
                []
                
        -- this is the gaussian
        , map (\(title,dat) ->
            fmap (Graph2D.lineSpec (
--                 OptsStyle.custom "smooth" "bezier" $
                LineSpec.title "" $ 
                LineSpec.lineWidth 3 $
                LineSpec.deflt
                )) $
            Plot2D.list Graph2D.listLines dat) $ 
--                 ("", map snd xs):
                ("", map (\(x,y) -> normalscale*(normalpdf m s x)) xs):
--                 ("", replicate 20 20):
                []
        ]
    where
        xlabels = zip (map (show . fst) xs) [0..]
        normalscale = (fromIntegral $ maximum $ map snd xs)/(normalpdf m s m)

normalcoord m s (x,y) = normalpdf m s x

normalpdf :: Double -> Double -> Double -> Double
normalpdf m s x = (1/(s*(sqrt $ 2*pi)))*(exp $ -(x-m)^2/(2*s^2))

--     Frame.cons (
--         Opts.title "Boosting Error Progression" $
--         Opts.xLabel "Number of boosting iterations" $
--         Opts.yLabel "Error" $
--         Opts.deflt) $

-------------------------------------------------------------------------------
-- testing

randomList :: (RandomGen g) => (Double,Double) -> Int -> Rand g [Double]
randomList r n = sequence $ replicate n $ getRandomR r

-- main=test
test = do
    let m=10
    let bin=10
    let num=100
    
    xs <- evalRandIO $ randomList (0,m) num
    ys <- evalRandIO $ randomList (0,m) num
    let zs = [ x*y | (x,y) <- zip xs ys]
    let hist=histogram zs
    putStrLn $ show $ hist
    putStrLn $ show $ func hist
--     putStrLn $ show $ fillhist bin $ histogram bin zs

    hist2svg "test.svg" hist
        where
            func (Histogram m s xs) = map (\(x,y) -> floor $ 100*(normalpdf m s x)) xs