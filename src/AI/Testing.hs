module AI.Testing
    where

import AI.Classification
import AI.DataLoader
import AI.Ensemble

import Control.Monad
import Control.Monad.Writer

import Data.List
import Database.HDBC
import Debug.Trace
import System.IO
import System.Random
   
import qualified Data.Map as Map
import qualified Data.Set as S

data TestConfig = TestConfig
    { factor :: Double
    , extreme :: Bool
    , seed :: Int
    , resultsDir :: String
    , dataDir :: String
    , dataFile :: String
    , trueClass :: String
    } deriving Show
    
defTest = TestConfig
    { factor = 0.8
    , extreme = False
    , seed = 0
    , resultsDir = ""
    , dataDir = ""
    , dataFile = ""
    , trueClass = ""
    }
          

randSplit:: StdGen -> Double -> [a] -> ([a],[a])
randSplit rgen factor xs = randSplitWalk 0 (length xs - 1) xs is ([],[])
    where is=randList2 rgen (floor $ factor*(fromIntegral $ length xs)) (length xs-1) S.empty

randSplitWalk :: Int -> Int -> [a] -> [Int] -> ([a],[a]) -> ([a],[a])
randSplitWalk itr stop xs     []     (s1,s2) = (s1,xs++s2)
randSplitWalk itr stop (x:xs) (y:ys) (s1,s2) = 
    if itr>stop
       then (s1,s2)
       else if itr==y
               then randSplitWalk (itr+1) stop xs ys     (x:s1,s2)
               else randSplitWalk (itr+1) stop xs (y:ys) (s1,x:s2)

randList2 :: StdGen -> Int -> Int -> S.Set Int -> [Int]
randList2 rgen total m set = 
    if S.size set == total
       then S.toList set
       else randList2 rgen' total m (S.insert r set)
           where (r,rgen') = randomR (0, m) rgen

s2ss:: StdGen -> Double -> [(Bool,a)] -> ([(Bool,a)],[a])
s2ss rgen factor xs = (l1, map snd l2)
    where (l1,l2)=randSplit rgen factor xs

--

-- s2ssX=s2ss
s2ssX:: StdGen -> Int -> [(Bool,a)] -> ([(Bool,a)],[a])
s2ssX rgen count ds = (fst xs,map snd $ snd xs)
    where 
        is=chooseIndices rgen count ds
        xs=splitIndices ds is

splitIndices ::[(Bool,a)] -> [Int] -> ([(Bool,a)],[(Bool,a)])
splitIndices ds is = 
    splitIndicesWalker ds (sort is) 0 [] []
    
splitIndicesWalker (d:[]) []     count s1 s2 = (s1,d:s2)
splitIndicesWalker (d:[]) (i:[]) count s1 s2 = (d:s1,s2)
splitIndicesWalker (d:ds) []     count s1 s2 = (s1,(d:ds)++s2)
splitIndicesWalker (d:ds) (i:is) count s1 s2 = 
    if i==count
       then splitIndicesWalker ds (is) (count+1) (d:s1) s2
       else splitIndicesWalker ds (i:is) (count+1) s1 (d:s2)

itest = [(True,1),(True,20),(True,3),(True,4),(False,10),(False,11),(False,-12),(False,13)]

chooseIndices :: StdGen -> Int -> [(Bool,a)] -> [Int]
chooseIndices rgen count ds = chooseIndicesItr rgen count ds S.empty S.empty

chooseIndicesItr :: StdGen -> Int -> [(Bool,a)] -> S.Set Int -> S.Set Int -> [Int]
chooseIndicesItr rgen count ds ts fs = 
    if S.size ts==count && S.size fs==count
       then (S.toList ts)++(S.toList fs)
       else if (fst $ ds !! r)
               then if (S.size ts<count)
                       then chooseIndicesItr rgen' count ds (S.insert r ts) fs 
                       else chooseIndicesItr rgen' count ds ts fs
               else if (S.size fs<count)
                       then chooseIndicesItr rgen' count ds ts (S.insert r fs)
                       else chooseIndicesItr rgen' count ds ts fs
    where  
        (r,rgen')=randomR (0,(length ds)-1) rgen

--

sup2semi :: STrainer model -> SSTrainer model
sup2semi trainer = \t -> \c -> \ls -> \us -> trainer t c ls

-- runTest testdatafile alg count seed = do
--     putStrLn $ "TEST = "++(show testdatafile)++"-"++(fst alg)++"-"++(show count)++"-"++(show seed)
--     let outfile = "../results/"++(fst testdatafile)++"-"++(fst alg)++"-"++(show count)++{-"-"++(show seed)++-}".csv"
-- --                  putStrLn $ "TEST = "++(show testdatafile)++"-"++(fst alg)++"-"++(show factor)++"-"++(show seed)
-- --                  let outfile = "../results/"++(fst testdatafile)++"-"++(fst alg)++"-"++(show factor)++{-"-"++(show seed)++-}".csv"
--     hout <- openFile outfile AppendMode
--     dm <- loadData $ "../testdata/"++(fst testdatafile)
--     printTest hout $ do
--         ds <- dm
--         let bds = toBinaryData (snd testdatafile) ds
--         return $  performTestExtreme seed alg count bds
-- --                     return $  performTest seed alg factor bds
--     hClose hout

-- runTest :: String -> (String,SSTrainer (NaiveBayes.NBayes Bool)) -> TestConfig -> IO ()
-- runTest :: (String,String) -> (String,SSTrainer (NaiveBayes.NBayes Bool)) -> Int -> Int -> IO ()

runTest testdata alg factor seed = do
    putStrLn $ "TEST = "++(show testdata)++"-"++(fst alg)++"-"++(show factor)++"-"++(show seed)
    let outfile = (resultsDir testdata)++"/"++(dataFile testdata)++"-"++(fst alg)++"-"++(show factor)++{-"-"++(show seed)++-}".csv"
--     putStrLn $ "TEST = "++(show testdatafile)++"-"++(fst alg)++"-"++(show count)++"-"++(show seed)
--     let outfile = "../results/"++(fst testdatafile)++"-"++(fst alg)++"-"++(show count)++{-"-"++(show seed)++-}".csv"
    hout <- openFile outfile AppendMode
    dm <- loadData $ (dataDir testdata)++"/"++(dataFile testdata)
    printTest hout $ do
        ds <- dm
        let bds = toBinaryData (trueClass testdata) ds
--         return $  performTestExtreme seed alg count bds
        return $  performTest seed alg factor bds
    hClose hout

-- performTest :: Int -> (String,BoolEnsemble (NaiveBayes.NBayes Bool)) -> Double -> [(Bool,DataPoint)] -> (String,String)
performTest seed (strTrainer,trainer) factor bds = (out,res)
    where
          rgen = mkStdGen seed
          res = strTrainer++","++(show seed)++","++(show factor)++","++(list2csv $ perfTrace ens train_data)
          out = concat $ map (\x -> "\n"++x) aitrace
          (ens,aitrace) = (runWriter $ trainer ls (take 500 us))
          (train_data,test_data) = randSplit rgen (0.8) bds
          (ls,us) = s2ss rgen (factor) train_data


-- performTest :: Int -> (String,SSTrainer (NaiveBayes.NBayes Bool)) -> Double -> [(Bool,DataPoint)] -> (String,String)
-- performTest seed (strTrainer,trainer) factor bds = (out,res)
--     where
--           rgen = mkStdGen seed
--           res = strTrainer++","++(show seed)++","++(show factor)++","++(list2csv $ perfTrace ens test_data)
--           out = concat $ map (\x -> "\n"++x) aitrace
--           (ens,aitrace) = (runWriter $ trainer ls (take 1000 us))
--           (train_data,test_data) = randSplit rgen (0.8) bds
--           (ls,us) = s2ss rgen (factor) train_data

performTestExtreme seed (strTrainer,trainer) count bds = (out,res)
    where
          rgen = mkStdGen seed
          res = strTrainer++","++(show seed)++","++(show count)++","++(list2csv $ perfTrace ens test_data)
          out = concat $ map (\x -> "\n"++x) aitrace
          (ens,aitrace) = (runWriter $ trainer ls (take 300 us))
          (train_data,test_data) = randSplit rgen (0.8) bds
          (ls,us) = s2ssX rgen (count) train_data


printTest :: (Show a) => Handle -> Either a (String,String) -> IO ()
printTest hout (Left err) = putStrLn $ show err
printTest hout (Right (std,file)) = do
    putStrLn std
    putStrLn "Writing next line to CSV"
    putStrLn file
    hPutStrLn hout file
    hFlush hout
    putStrLn "Done."
   
list2csv :: (Show a) => [a] -> String
list2csv xs = init $ tail $ show xs

-- perfTrace :: Ensemble model -> [(Bool,[SqlValue])] -> [Double]
-- perfTrace (Ensemble es) ds = [ show $ middle (es !! i) | i <- [0..length es]] 
-- perfTrace (Ensemble es) ds = [ eval (classify $ Ensemble $ drop i es) ds | i <- [0..length es]] 
perfTrace (Ensemble es) ds = [ errorRate $ eval (classify $ Ensemble $ drop i es) ds | i <- [0..length es]] 

-- middle (a,b,c) = b
