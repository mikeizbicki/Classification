module Main where

import AI.DataLoader
import AI.MathTmp
import Graphics.Histogram

import Control.Monad
import Data.List
import Data.Monoid
import Debug.Trace
import System.Directory
import System.IO
import Text.ParserCombinators.Parsec

import qualified Data.Map as Map

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PostScript as PS

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
    
-- csv2stats :: (Num t) => String -> [[String]] -> [(String, t, [Double])]
csv2stats title xs = 
    [ last $ matmap mean $ doubledata xs
    , last $ matmap stddev $ doubledata xs
    ]

csv2boostErr :: (Num t) => String -> [[String]] -> [(String, t, [Double])]
csv2boostErr title xs = trace title $
    [ ("",     1, matmap (quantile 0.75) $ doubledata xs)
    , (title', 2, matmap mean           $ doubledata xs)
    , ("",     1, matmap (quantile 0.25) $ doubledata xs)
    ]
    where 
        title' = 
            swapL '\\' ' ' $
            last $
            words $
            swapL '/' ' ' $ 
            swapL ' ' '\\' title
            
swapL :: Char -> Char -> String -> String
swapL x y ls = map (\c -> if c==x
                             then y
                             else c) ls

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
        Opts.title "" $
        Opts.xLabel "Number of boosting iterations" $
        Opts.yLabel "Error" $
--         Opts.yRange2d (0,0.35) $
        Opts.deflt) $
    mconcat $
    mkErrLines Color.red [(head $ tail xs)]
          
-- mkErrLines :: Color.T -> [(String,Double,[a])] -> [Plot2D.T (Graph2D.T Int Double)]
mkErrLines color xs =
    map (\(title,width,dat) ->
        fmap (Graph2D.lineSpec (
            LineSpec.title title $ 
            LineSpec.lineWidth width $
            LineSpec.lineColor color $
            LineSpec.lineStyle 1 $
            LineSpec.deflt
            )) $
        Plot2D.list Graph2D.listLines dat) $ xs
        
-- boostErrComp :: [[(Color.T,(String,Double,[Double]))]] -> Frame.T (Graph2D.T Int Double)
boostErrComp xs =
    Frame.cons (
        Opts.title "" $
        Opts.xLabel "Number of boosting iterations" $
        Opts.yLabel "Error" $
--         Opts.yRange2d (0,0.3) $
        Opts.deflt) $
    mconcat $ concat 
    [ [head $ tail $ mkErrLines color ys] | (color,ys) <- xs ]
        
-------------------------------------------------------------------------------
-- plotting IO

loadResults :: String -> (String -> [[String]] -> r) -> String -> IO (Either ParseError r)

-- loadResults {-rowfilter-} plottingFunc filename = liftM (liftM $ plottingFunc $ nocsv filename) $ loadCSV filename
loadResults rowfilter plottingFunc filename = do
    eitherCSV <- loadCSV filename
    return $ do
        csv <- eitherCSV
        return $ plottingFunc (nocsv $ filename) $ filter (\xs -> xs!!2==rowfilter) csv
   
nocsv str = trace str $
    if (take 4 $ reverse str)=="vsc."
        then reverse $ drop 4 $ reverse str
        else str

plotFile file = do
    putStrLn $ "Plotting results for: "++file
    eitherBoostErr <- loadResults "performance" csv2boostErr file
    eitherBoostMargin <- loadResults "aveMargin" csv2boostErr file
    eitherBoostHist <- loadResults "performance" csv2boostHist file
    sequence_
        [ Plot.plot (PS.cons $ file++".boostErr.ps") $ boostErr $ right eitherBoostErr
        , Plot.plot (PS.cons $ file++".boostMargin.ps") $ boostErr $ right eitherBoostMargin
--         , Plot.plot (PS.cons $ file++".boostHist.ps") $ boostHist $ right eitherBoostHist
        ]    
    plot (file++".boostHistogram.ps") $ histogram binSturges $ right eitherBoostHist
right (Right xs) = xs
          
algCompare resdir fs = do
    putStrLn $ "Plotting comparison for: "++(show fs)
    eitherBoostErr <- sequence [loadResults "performance" csv2boostErr f | f<-fs]
    sequence_
        [ Plot.plot (PS.cons $ resdir++"/comp."++{-(show fs)++-}".boostErr.ps") $ boostErrComp $ 
            zip [ Color.red, Color.orange, Color.green, Color.blue, Color.purple, Color.black ]
                [ right ebe | ebe <- eitherBoostErr ]
        ]
        
plotAllFiles tmpdir resdir = do
    setCurrentDirectory tmpdir
    files <- getDirectoryContents resdir
    let resfiles = sort $ map (resdir++) $ filter okfile files
    algCompare resdir resfiles
    sequence_ $ map plotFile $ resfiles

okfile str = isInfixOf ".csv" str && 
             (not $ isInfixOf ".png" str) && 
             (not $ isInfixOf ".ps" str) && 
             (not $ isInfixOf ".svg" str)

--------------------

statsAllFiles tmpdir resdir = do
    setCurrentDirectory tmpdir
    hout <- openFile (resdir++"/stats.txt") WriteMode
    files <- getDirectoryContents resdir
    let resfiles = sort $ map (resdir++) $ filter okfile files
    sequence_ $ map (statsFile hout) $ resfiles
    hClose hout

statsFile hout file = do
    hPutStr hout $ "Statistics for: "++file++"  ;  "
    eitherStats <- loadResults "performance" csv2stats file
    let stats=right eitherStats
    hPutStr hout $ "mean="++(show $ stats !! 0)++"  ;  "
    hPutStr hout $ "stddev="++(show $ stats !! 1)++"  ;  "
    hPutStrLn hout ""


-- main

main :: IO ()
main = do
    plotAllFiles tmpdir resdir
    statsAllFiles tmpdir resdir
    where
        tmpdir = "/home/user/proj/haskell-classification/tmp/"
        resdir = "/home/user/proj/haskell-classification/results/"
--         resdir="/home/user/proj/haskell-classification/results/results-good/ringnorm-NB-0.01/"