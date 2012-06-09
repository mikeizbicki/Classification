import AI.Classification
import AI.DataLoader
import AI.Ensemble

import qualified AI.Supervised.DecisionStump as DecisionStump
import qualified AI.Supervised.NaiveBayes as NaiveBayes
import qualified AI.Supervised.AdaBoost as AdaBoost
import qualified AI.Supervised.MarginBoost as MarginBoost

import qualified AI.SemiSupervised.ASSEMBLE as ASSEMBLE
import qualified AI.SemiSupervised.SemiMarginBoost as SemiMarginBoost
import qualified AI.SemiSupervised.SemiBoost as SemiBoost
import qualified AI.SemiSupervised.RegBoost as RegBoost
import qualified AI.SemiSupervised.RegularizedBoost as RegularizedBoost

import Control.Monad
import Control.Monad.Writer

import Database.HDBC
import Debug.Trace
import System.IO
import System.Random
   
import qualified Data.Map as Map
import qualified Data.Set as S
          
-- test functions
-- kFolds :: Int -> [a] -> [[a]]
-- kFolds k xs = kFoldsItr 0 xs
--     where
--           kFoldsItr itr []   = []
--           kFoldsItr itr rest = {-trace (show itr ++ "-" ++ show n) $-} [take n rest]++(kFoldsItr (itr+1) (drop n rest))
--               where n = ceiling $ (fromIntegral $ length xs) / (fromIntegral k)

{-randSplit:: StdGen -> Double -> [a] -> ([a],[a]) -> ([a],[a])
randSplit rgen factor []     (ls,us) = (ls,us)
randSplit rgen factor (x:xs) (ls,us) = if r<factor
                                          then randSplit rgen' factor xs (x:ls,us)
                                          else randSplit rgen' factor xs (ls,x:us)
    where (r,rgen') = randomR (0,1) rgen-}
          
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

sup2semi :: STrainer model -> SSTrainer model
sup2semi trainer = \t -> \c -> \ls -> \us -> trainer t c ls
   --

main=test
test = do
    let x = [ do
                 putStrLn $ "TEST = "++(show testdatafile)++"-"++(fst alg)++"-"++(show factor)++"-"++(show seed)
                 let outfile = "../results/"++(fst testdatafile)++"-"++(fst alg)++"-"++(show factor)++{-"-"++(show seed)++-}".csv"
                 hout <- openFile outfile AppendMode
                 dm <- loadData $ "../testdata/"++(fst testdatafile)
                 seq dm $ printTest hout $ do
                    ds <- dm
                    let bds = toBinaryData (snd testdatafile) ds
                    return $  performTest seed alg factor bds
                 hClose hout
                 
            | alg <- 
--                 ("RegularizedBoost-NB",RegularizedBoost.train NaiveBayes.train NaiveBayes.classify) :
--                 ("SemiMarginBoost-NB",SemiMarginBoost.train NaiveBayes.train NaiveBayes.classify) :
--                 ("ASSEMBLE-NB",ASSEMBLE.train NaiveBayes.train NaiveBayes.classify):
--                 ("SemiBoost-NB",SemiBoost.train NaiveBayes.train NaiveBayes.classify):
                ("AdaBoost-NB",sup2semi AdaBoost.train NaiveBayes.train NaiveBayes.classify):
--                 ("MarginBoost-NB",sup2semi MarginBoost.train NaiveBayes.train NaiveBayes.classify):
                
--                 ("RegularizedBoost-DS",RegularizedBoost.train DecisionStump.train DecisionStump.classify) :
--                 ("SemiMarginBoost-DS",SemiMarginBoost.train DecisionStump.train DecisionStump.classify) :
--                 ("MarginBoost-DS",sup2semi MarginBoost.train DecisionStump.train DecisionStump.classify):
--                 ("ASSEMBLE-DS",ASSEMBLE.train DecisionStump.train DecisionStump.classify):
--                 ("SemiBoost-DS",SemiBoost.train DecisionStump.train DecisionStump.classify):
--                 ("AdaBoost-DS",sup2semi AdaBoost.train DecisionStump.train DecisionStump.classify):
                []
--             , factor <- [ 0.2, 0.1, 0.05 ]
            , factor <- [ 0.2 ]
--             , seed <- [1..10]
            , seed <- [1..200]
            , testdatafile <- 
--                 ("kr-vs-kp.data","won"):
--                 ("tic-tac-toe.data","positive"):
                ("haberman.data","1") :
--                 ("optdigits.data","2") :
--                 ("german.data","1") :
--                 ("ringnorm.data","1") :
--                 ("twonorm.data","1") :
--                 ("ionosphere.data","g"):
                []
            ]
    sequence x

-- performTest :: Int -> (String,SSTrainer (NaiveBayes.NBayes Bool)) -> Double -> [(Bool,DataPoint)] -> (String,String)
performTest seed (strTrainer,trainer) factor bds = (out,res)
    where
          rgen = mkStdGen seed
          res = strTrainer++","++(show seed)++","++(show factor)++","++(list2csv $ perfTrace ens test_data)
          out = concat $ map (\x -> "\n"++x) aitrace
          (ens,aitrace) = (runWriter $ trainer ls (take 1000 us))
          (train_data,test_data) = randSplit rgen (0.8) bds
          (ls,us) = s2ss rgen (factor) train_data

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