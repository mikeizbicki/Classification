module Main where

import Control.Monad
import Data.List
import Data.Monoid
import System.Directory
import System.IO
import Text.ParserCombinators.Parsec

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

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

-- importing data functions

-- loadResults :: String -> IO (Either ParseError ResultsData)
loadResults plottingFunc filename = do
    hin <- openFile filename ReadMode
    str <- hGetContents hin
    let ds = liftM plottingFunc $ parseCSV str
    return ds
    
-- csv2boostErr :: [[String]] -> [Double]
csv2boostErr xs = 
    [ ("", 1, matmap (quantile 0.1) $ doubledata xs)
    , ("", 2, matmap mean           $ doubledata xs)
    , ("", 1, matmap (quantile 0.9) $ doubledata xs)
    ]
    
-- csv2boostHist :: [[String]] -> [Double]
csv2boostHist xs = 
    [ ("", 1, matmap (quantile 0.1) $ doubledata xs)
    , ("", 2, matmap mean           $ doubledata xs)
    , ("", 1, matmap (quantile 0.9) $ doubledata xs)
    ]

csv2boostHist' xs = 
    [ ("", 1, matmap (quantile 0.1) $ doubledata xs)
    , ("", 2, matmap mean           $ doubledata xs)
    , ("", 1, matmap (quantile 0.9) $ doubledata xs)
    ]

test = loadResults csv2boostHist' "/home/user/proj/haskell-classification/results/german.data-AdaBoost-0.1.csv"

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
    
-- CSV parser from "Real World Haskell," p. 391
    
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown--parseResults)" input
    
csvFile = endBy line eol
line = sepBy cell (char ',' <|> char ' ')
cell = do
    spaces
    quotedCell <|> many (noneOf " ,\n\r")

quotedCell = do
    char '"'
    content <- many quotedChar
    char '"' <?> "Quote at end of cell"
    return content
    
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"

-- types of plots

boostErr :: [(String,Double,[Double])] -> Frame.T (Graph2D.T Int Double)
boostErr xs =
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
          
plotFile file = do
    putStrLn $ "Plotting results for: "++file
    eitherBoostErr <- loadResults csv2boostErr file
    eitherBoostHist <- loadResults csv2boostHist file
    sequence_
        [ Plot.plot (PNG.cons $ file++".boostErr.png") $ boostErr $ right eitherBoostErr
        , Plot.plot (PNG.cons $ file++".boostHist.png") $ boostHist $ right eitherBoostHist
        ]    
{-    liftM sequence_ $ do
        ds <- eitherdata
        return 
            [ Plot.plot (PNG.cons $ resdir++"list.png") $ plotLine ds
            ]-}
right (Right xs) = xs
          
plotAllFiles tmpdir resdir = do
    setCurrentDirectory tmpdir
    files <- getDirectoryContents resdir
    let resfiles = map (resdir++) $ filter (isInfixOf ".csv") files
    sequence_ $ map plotFile $ resfiles

-- main

main :: IO ()
main = do
    plotAllFiles tmpdir resdir
    where
        tmpdir = "/home/user/proj/haskell-classification/tmp/"
        resdir = "/home/user/proj/haskell-classification/results/"
