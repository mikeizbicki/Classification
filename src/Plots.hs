module Main where

import AI.DataLoader

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
    
-- csv2boostErr :: [[String]] -> [Double]
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
    
-- csv2boostHist :: [[String]] -> [Double]
csv2boostHist title xs = 
    [ ("", 1, matmap (quantile 0.1) $ doubledata xs)
    , ("", 2, matmap mean           $ doubledata xs)
    , ("", 1, matmap (quantile 0.9) $ doubledata xs)
    ]

csv2boostHist' title xs = 
--     last2nd $ head $ doubledata xs
    histogram $ map (\x-> floor $ x*100) $ map (\ys -> (last2nd ys)-(head ys)) $ doubledata xs
    where 
        last2nd ys = head $ tail $ reverse ys
        
histogram :: Ord a => [a] -> Map.Map a Int
histogram xs = Map.fromList [ (head l, length l) | l <- group (sort xs) ]

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
        
-- Math functions copied from Math.Statistics because the whole thing wouldn't compile

mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x
-- mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x
    
-- |Arbitrary quantile q of an unsorted list.  The quantile /q/ of /N/
-- |data points is the point whose (zero-based) index in the sorted
-- |data set is closest to /q(N-1)/.
quantile :: (Fractional b, Ord b) => Double -> [b] -> b
quantile q = quantileAsc q . sort

-- |As 'quantile' specialized for sorted data
quantileAsc :: (Fractional b, Ord b) => Double -> [b] -> b
quantileAsc _ [] = error "quantile on empty list"
quantileAsc q xs
    | q < 0 || q > 1 = error "quantile out of range"
    | otherwise = xs !! (quantIndex (length xs) q)
    where quantIndex :: Int -> Double -> Int
          quantIndex len q = case round $ q * (fromIntegral len - 1) of
                               idx | idx < 0    -> error "Quantile index too small"
                                   | idx >= len -> error "Quantile index too large"
                                   | otherwise  -> idx

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
        
        
boostHist :: [(String,Double,[Double])] -> Frame.T (Graph2D.T Int Double)
boostHist xs =
    Frame.cons (
        Opts.title "Boosting Error Progression" $
        Opts.xLabel "Number of boosting iterations" $
        Opts.yLabel "Error" $
        Opts.deflt) $
    mconcat $
    map (\(title,width,dat) ->
        fmap (Graph2D.lineSpec (
            LineSpec.title title $ 
            LineSpec.lineWidth width $
            LineSpec.deflt
            )) $
        Plot2D.list Graph2D.listLines dat) $ xs

-- plotting IO
          
loadResults plottingFunc filename = liftM (liftM $ plottingFunc filename) $ loadCSV filename

plotFile file = do
    putStrLn $ "Plotting results for: "++file
    eitherBoostErr <- loadResults csv2boostErr file
    eitherBoostHist <- loadResults csv2boostHist file
    sequence_
        [ Plot.plot (SVG.cons $ file++".boostErr.svg") $ boostErr $ right eitherBoostErr
        , Plot.plot (SVG.cons $ file++".boostHist.svg") $ boostHist $ right eitherBoostHist
        ]    
{-    liftM sequence_ $ do
        ds <- eitherdata
        return 
            [ Plot.plot (PNG.cons $ resdir++"list.png") $ plotLine ds
            ]-}
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
