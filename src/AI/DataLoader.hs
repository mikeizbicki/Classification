module AI.DataLoader
    where

import Control.Monad
import Debug.Trace
import System.IO
import Text.ParserCombinators.Parsec

import AI.Classification

-------------------------------------------------------------------------------

-- IO functions

loadData :: String -> Maybe String -> Maybe [String->DataItem] -> IO (Either ParseError (TrainingData String))
loadData filename missingStr fs = do
    csv <- loadCSV filename
    return $ liftM (csv2data missingStr fs) csv

loadCSV :: String -> IO (Either ParseError [[String]])
loadCSV filename = do
    hin <- openFile filename ReadMode
    str <- hGetContents hin
    let ds = parseCSV str
    return ds

---------------

csv2data :: Maybe String -> Maybe [String -> DataItem] -> [[String]] -> TrainingData String
csv2data missingStr Nothing   csv = csv2dataAuto missingStr csv
csv2data missingStr (Just fs) csv = csv2dataForce missingStr fs csv

csv2dataAuto :: Maybe String -> [[String]] -> TrainingData String
csv2dataAuto missingStr csv = map (\dp -> (last dp, map cell2sql $ init dp)) csv
    where 
        cell2sql x = 
            if Just x==missingStr
               then Missing
               else case (reads x::[(Double,String)]) of
                        []     -> toDataItem (x::String)
                        (x:xs) -> toDataItem $ fst x

csv2dataForce :: Maybe String -> [String -> DataItem] -> [[String]] -> TrainingData String
csv2dataForce missingStr fs csv = 
    [(last line,
        [ if Just cell==missingStr
             then Missing
             else f cell
        | (f,cell) <- zip fs $ init line
        ])
    | line <- csv
    ]

-- | Converts a list into CSV format for writing
list2csv :: (Show a) => [a] -> String
list2csv xs = foldl addcommas "" $ map addquotes xs
    where
        addcommas x y = 
            if x==""
               then y
               else x++","++y
        addquotes x = "\""++show x++"\""
-- list2csv xs = init $ tail $ show xs

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
    dm <- loadData "../testdata/ringnorm.data" Nothing Nothing
    putStrLn $ func dm
    where
          func (Left x) = show x
          func (Right x) = show $ take 10 x
