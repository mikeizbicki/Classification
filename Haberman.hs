import Classification
import DecisionStump
import NaiveBayes
import AdaBoost

import Control.Monad
import Control.Monad.Writer

import Database.HDBC
import Debug.Trace
import System.IO
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



test = do
    dm <- loadData "testdata/german.data"
--     dm <- loadData "testdata/haberman.data"
--     dm <- loadData "testdata/ionosphere.data"
    let x=do
        ds <- dm
        let bds = toBinaryData "1" ds
--         let bds =  DecisionStump.sqldata
        
        let bnbc = NaiveBayes.classify (NaiveBayes.train bds)
        
        let (ada,out) = (runWriter $ AdaBoost.train DecisionStump.train DecisionStump.classify bds)
--         let (ada,out) = (runWriter $ AdaBoost.train NaiveBayes.train NaiveBayes.classify bds)
        let adac= AdaBoost.classify ada
        
        let dsc = DecisionStump.classify (DecisionStump.train bds)
        
--         return $ ([""],eval dsc bds)
        return $ (out,eval adac bds)
    pExec x

pExec :: (Show a) => Either b ([String],PerformanceDesc a) -> IO ()
pExec (Right (xs,pd)) = do
    putStrLn $ concat $ map (\x -> "\n"++x) xs
    putStrLn $ "result="++show pd