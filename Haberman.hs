import Classification
import NaiveBayes
import AdaBoost

import Control.Monad
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
    let ds = liftM (map (\dp -> (last dp, map (\x -> toSql x{-$ (read x::Double)-}) $ init dp))) $ parseCSV str
    return ds
    
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

-- foldeval c ds = map (eval c) $ kFolds ds

test = do
    dm <- loadData "testdata/german.data"
--     dm <- loadData "testdata/haberman.data"
    putStrLn $ show $ do
        ds <- dm
        let bds = toBinaryData "1" ds
        let bnb = {-toBinaryClassifier "1" $-} NaiveBayes.classify -- (NaiveBayes.train ds)
        let bnbc = NaiveBayes.classify (NaiveBayes.train bds)
        let ada = (AdaBoost.train NaiveBayes.train bnb bds)
        let adac= AdaBoost.classify ada
        return $ {-ada -} {-errorRate $-} eval bnbc bds
    