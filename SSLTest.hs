import Classification
import DecisionStump
import NaiveBayes
import AdaBoost
import ASSEMBLE
import Ensemble

import Control.Monad
import Control.Monad.Writer

import Database.HDBC
import Debug.Trace
import System.IO
import System.Random
import Text.ParserCombinators.Parsec
   
import qualified Data.Map as Map
   
-- IO functions

loadData :: String -> IO (Either ParseError TrainingData)
loadData filename = do
    hin <- openFile filename ReadMode
    str <- hGetContents hin
    let ds = liftM (map (\dp -> (last dp, map cell2sql $ init dp))) $ parseCSV str
    return ds
        where cell2sql x = case (reads x::[(Double,String)]) of
                                [] -> toSql (x::String)
                                xs -> toSql $ fst $ head xs
    
-- CSV parser from "Real World Haskell," p. 391
    
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
    
csvFile = endBy line eol
line = sepBy cell (char ',' <|> char ' ')
cell = quotedCell <|> many (noneOf " ,\n\r")

quotedCell = do
    char '"'
    content <- many quotedChar
    char '"' <?> "Quote at end of cell"
    return content
    
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
    
-- test functions
kFolds :: Int -> [a] -> [[a]]
kFolds k xs = kFoldsItr 0 xs
    where
          kFoldsItr itr []   = []
          kFoldsItr itr rest = {-trace (show itr ++ "-" ++ show n) $-} [take n rest]++(kFoldsItr (itr+1) (drop n rest))
              where n = ceiling $ (fromIntegral $ length xs) / (fromIntegral k)

s2ss:: StdGen -> Double -> [(Bool,DataPoint)] -> ([(Bool,DataPoint)],[DataPoint]) -> ([(Bool,DataPoint)],[DataPoint])
s2ss rgen factor []     (ls,us) = (ls,us)
s2ss rgen factor (x:xs) (ls,us) = if r<factor
                                     then s2ss rgen' factor xs (x:ls,us)
                                     else s2ss rgen' factor xs (ls,(snd x):us)
    where (r,rgen') = randomR (0,1) rgen

--

test = do
--     rgen <- newStdGen
    let rgen = mkStdGen 200
    dm <- loadData "testdata/german.data"
--     dm <- loadData "testdata/haberman.data"
--     dm <- loadData "testdata/ionosphere.data"
    let x=do
        ds <- dm
        let bds = toBinaryData "1" ds
        let (ls,us) = s2ss rgen (0.1) bds ([],[])
        
        let bnbc = NaiveBayes.classify (NaiveBayes.train bds)
        
--         let (ada,out) = (runWriter $ ASSEMBLE.train DecisionStump.train DecisionStump.classify ls us)
        let (ada,out) = (runWriter $ ASSEMBLE.train NaiveBayes.train NaiveBayes.classify ls [])
        let adac= Ensemble.classify ada
        
        let dsc = DecisionStump.classify (DecisionStump.train bds)
        
--         return $ ([""],eval dsc bds)
        let evalres=eval adac bds
        return $ trace ("length ls/us = "++(show $ length ls)++"/"++(show $ length us)) $ (out,evalres)
    pExec x

pExec :: (Show a) => Either b ([String],PerformanceDesc a) -> IO ()
pExec (Right (xs,pd)) = do
    putStrLn $ concat $ map (\x -> "\n"++x) xs
    putStrLn $ "result="++show pd++"  --  "++(show $ errorRate pd)