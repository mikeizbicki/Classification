module Main where

import AI.DataLoader
import AI.MathTmp
import Histogram

import Control.Monad
import Data.List
import Data.Monoid
import System.Directory
import System.IO
import Text.ParserCombinators.Parsec

import qualified Data.Map as Map

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as Color
    
csv2boostErr :: (Num t) => String -> [[String]] -> [(String, t, [Double])]
csv2boostErr title xs = 
    [ ("",     1, matmap (quantile 0.1) $ doubledata xs)
    , (title', 2, matmap mean           $ doubledata xs)
    , ("",     1, matmap (quantile 0.9) $ doubledata xs)
    ]
    where 
        title' = 
            last $
            words $
            map (\x -> if x=='/'
                          then ' '
                          else x)
                title

-- csv2boostHist :: (Num t) => String -> [[String]] -> [(String, t, [Double])]
csv2boostHist title xs = 
    map boostingperf $ doubledata xs
    
boostingperf xs = (head $ tail $ reverse xs) - (head $ tail $ tail $ tail xs)
    
-- helpers

matmap :: ([a]->b) -> [[a]] -> [b]
matmap f xs = reverse $ matmap' f xs
    
matmap' :: ([a]->b) -> [[a]] -> [b]
matmap' f xs = 
    if (length $ head xs)>1
       then (f $ map head xs):(matmap' f $ map tail xs)
       else (f $ map head xs):[]
    
doubledata :: [[String]] -> [[Double]]
doubledata xs = 
    map ( map (read::String->Double)
        . tail
        . tail
        . tail
        )
        xs
        
-- types of plots

boostErr :: [(String,Double,[Double])] -> Frame.T (Graph2D.T Int Double)
boostErr xs =
    Frame.cons (
        Opts.title "Boosting Error Progression" $
        Opts.xLabel "Number of boosting iterations" $
        Opts.yLabel "Error" $
        Opts.deflt) $
    mconcat $
    mkErrLines Color.red xs
          
-- mkErrLines :: Color.T -> [(String,Double,[a])] -> [Plot2D.T (Graph2D.T Int Double)]
mkErrLines color xs =
    map (\(title,width,dat) ->
        fmap (Graph2D.lineSpec (
            LineSpec.title title $ 
            LineSpec.lineWidth width $
            LineSpec.lineColor color $
            LineSpec.deflt
            )) $
        Plot2D.list Graph2D.listLines dat) $ xs
        
-- boostErrComp :: [[(Color.T,(String,Double,[Double]))]] -> Frame.T (Graph2D.T Int Double)
boostErrComp xs =
    Frame.cons (
        Opts.title "Boosting Error Progression" $
        Opts.xLabel "Number of boosting iterations" $
        Opts.yLabel "Error" $
        Opts.deflt) $
    mconcat $ concat 
    [ [head $ tail $ mkErrLines color ys] | (color,ys) <- xs ]
        
-- plotting IO
          
loadResults plottingFunc filename = liftM (liftM $ plottingFunc filename) $ loadCSV filename

plotFile file = do
    putStrLn $ "Plotting results for: "++file
    eitherBoostErr <- loadResults csv2boostErr file
    eitherBoostHist <- loadResults csv2boostHist file
    sequence_
        [ Plot.plot (SVG.cons $ file++".boostErr.svg") $ boostErr $ right eitherBoostErr
--         , Plot.plot (SVG.cons $ file++".boostHist.svg") $ boostHist $ right eitherBoostHist
        ]    
    hist2svg (file++".boostHistogram.svg") $ histogram $ right eitherBoostHist
right (Right xs) = xs
          
algCompare fs = do
    putStrLn $ "Plotting comparison for: "++(show fs)
    eitherBoostErr <- sequence [loadResults csv2boostErr f | f<-fs]
    sequence_
        [ Plot.plot (SVG.cons $ "../results/comp."++{-(show fs)++-}".boostErr.svg") $ boostErrComp $ 
            zip [ Color.red, Color.green, Color.blue, Color.black ]
                [ right ebe | ebe <- eitherBoostErr ]
        ]
        
plotAllFiles tmpdir resdir = do
    setCurrentDirectory tmpdir
    files <- getDirectoryContents resdir
    let resfiles = map (resdir++) $ filter (\str-> isInfixOf ".csv" str && (not $ isInfixOf ".png" str) && (not $ isInfixOf ".svg" str)) files
    algCompare resfiles
    sequence_ $ map plotFile $ resfiles

-- main

main :: IO ()
main = do
    plotAllFiles tmpdir resdir
    where
        tmpdir = "/home/user/proj/haskell-classification/tmp/"
        resdir = "/home/user/proj/haskell-classification/results/"
