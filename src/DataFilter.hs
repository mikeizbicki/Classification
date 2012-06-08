import Control.Monad
import Database.HDBC
import System.IO
import Text.ParserCombinators.Parsec

-- loadData :: String -> IO (Either ParseError TrainingData)
loadData filename = do
    hin <- openFile filename ReadMode
    str <- hGetContents hin
    let ds = parseCSV str
    return ds
        where 
--               cell2sql x = toSql (read x::Double) 
              cell2sql x = toSql $ case (reads x::[(Double,String)]) of
                                        []     -> toSql (x::String)
                                        (x:xs) -> toSql $ fst x
    
-- CSV parser from "Real World Haskell," p. 391
    
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

filterData inFile outFile allow = do
    ds <- loadData inFile
    hout <- openFile outFile WriteMode
    let filteredData = liftM (filter $ \ds -> (last ds) `elem` allow) ds
    writeData hout filteredData
    hClose hout
    
writeData hout (Right ds) =
    sequence_ $ map (hPutStrLn hout . list2str) ds
    
list2str xs = 
    reverse $ tail $ reverse $ tail $ show xs