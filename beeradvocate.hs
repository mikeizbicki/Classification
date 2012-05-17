import Control.Monad.ListT
import Control.Exception as E
import Data.Char
import Data.List
import Network.HTTP
import Text.HTML.TagSoup

import IO
import System.Directory

-- generic functions

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

downloadFiles [] = putStrLn "Done."
downloadFiles (x:xs) = do
    let (filename,url) = x
    putStrLn filename
    openURL url >>= writeFile filename
    downloadFiles xs

getLink (TagOpen tag attr) = snd $ head $ attr

replace a b []     = []
replace a b (x:xs) = if a==x 
                        then b:(replace a b xs)
                        else x:(replace a b xs)
                        
split char str =  case dropWhile {-partain:Char.-}(==char) str of
                       "" -> []
                       s' -> w : split char s''
                            where (w, s'') =
                                    break {-partain:Char.-}(==char) s'


-- process searches

searchList = [("searches/"++show start++".html"
              ,"http://beeradvocate.com/beer/reviews?view=all&order=breweryA&start="++show start
              )
             | start<-map (50*) [0..388]
             ]

downloadSearches = downloadFiles searchList

parseSearch searchFile = do
    tags <- fmap parseTags $ readFile searchFile
    let beerList = map getLink $ map (!!5) $ take 50 $ drop 5 $ sections (~== "<tr>") $ head $ sections (~== "<table width=100% cellpadding=2 cellspacing=0 border=0>") tags
    return beerList


beerSearches :: [(String,String)] -> IO [String]
beerSearches [] = do 
    return []
beerSearches (x:xs) = do
    let (filename,url) = x
    putStrLn ("parsing"++filename)
    links <- parseSearch filename
    morelinks <- beerSearches xs
    return ((nub $ map (head . split '?') links)++morelinks)
    
writeBeerList = do
    list <- beerSearches $ take 300 searchList
    h <- openFile "beerlist" WriteMode
    writeList h $ sort $ nub list
    hClose h
    where
        writeList h [] = hPutStrLn h ""
        writeList h (x:xs) = writeList h xs >> hPutStrLn h x
{-            do
            hPutStrLn h x
            writeList h xs-}

downloadBeers = do
    h <- openFile "beerlist" ReadMode
    list <- hGetContents h
    let beerList=map (\x -> ("beers/"++(replace '/' '-' x)++".html","http://beeradvocate.com"++x)) $ words list
    downloadFiles beerList


-- process each beer file
beerFile beerFile = do
    tags <- fmap parseTags $ readFile beerFile
    let beerList = head $ sections (~== "<span style=\"color: #999999\">") tags
    putStrLn ("File="++beerFile)
    let ahrefIndex = case elemIndex (TagText "last \187") beerList of
                          Just x  -> x-1
                          Nothing -> error "elemIndex = Nothing!?"
--     putStrLn (show $ ahrefIndex)
    let lastPage = floor $ (toRational (read ({-head $ words $-} last $ split '=' $ getLink $ beerList !! ahrefIndex) :: Int)) / 10
    let dirName = head $ split '.' beerFile
    let htmlTmp = replace '-' '/' $ (split '/' dirName) !! 1
    let dirName = head $ split ('.') beerFile
    createDirectory dirName
    downloadFiles [ (dirName++"/search_"++show x++".html","http://beeradvocate.com"++htmlTmp++"?sort=topr&start="++show x)
                  | x <- map (*10) [0..lastPage]
                  ]
    putStrLn $ show $ htmlTmp
    
beerFiles []     = putStrLn "beerFiles done."
beerFiles (x:xs) = do
    putStrLn ("Files left="++(show $ length xs))
    E.catch (beerFile x) handler
    beerFiles xs
        where handler :: E.SomeException -> IO ()
              handler e = putStrLn ("Exception caught! " ++ show e)
    
beer = do
    list <- getDirectoryContents "beers"
    let newList = drop 2 $ filter (elem '.') $ map ("beers/"++) list
    beerFiles newList
--     putStrLn $ show newList 


test = do
    links <- parseSearch $ fst $ head searchList
    putStrLn $ show links