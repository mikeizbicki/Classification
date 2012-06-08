module AI.DataLoader
    where

import Control.Monad
import Database.HDBC
import System.IO
import Text.ParserCombinators.Parsec

import AI.Classification

-- IO functions

loadData :: String -> IO (Either ParseError TrainingData)
loadData filename = do
    csv <- loadCSV filename
    return $ liftM csv2data csv

loadCSV :: String -> IO (Either ParseError [[String]])
loadCSV filename = do
    hin <- openFile filename ReadMode
    str <- hGetContents hin
    let ds = parseCSV str
    return ds

csv2data :: [[String]] -> TrainingData
csv2data csv = map (\dp -> (last dp, map cell2sql $ init dp)) csv
        where 
              cell2sql x = toSql $ case (reads x::[(Double,String)]) of
                                        []     -> toSql (x::String)
                                        (x:xs) -> toSql $ fst x
    
-- CSV parser from "Real World Haskell," p. 391
-- modified to allow multiple spaces between cells
    
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
    
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
   
-- test csv parsing

csv_test = do
    dm <- loadData "../testdata/ringnorm.data"
    putStrLn $ func dm
    where
          func (Left x) = show x
          func (Right x) = show $ take 10 x
