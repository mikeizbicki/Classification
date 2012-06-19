module AI.Testing
    where

import AI.Classification
import AI.DataLoader
import AI.Ensemble
import AI.RandomUtils

import Control.Monad
import Control.Monad.Writer

import Database.HDBC
import Debug.Trace
import System.IO
import System.Random

-------------------------------------------------------------------------------

data TestConfig model = TestConfig
    { ldr :: Double -- ^ labeled data rate
    , tdr :: Double -- ^ training data rate.  Set equal to number of 1-(1/number of folds).
    , seed :: Int
    , resultsDir :: String
    , dataDir :: String
    , dataFile :: String
    , trueClass :: String
    } deriving Show
    
defTest = TestConfig
    { ldr = 0.2
    , tdr = 0.8
    , seed = 0
    , resultsDir = ""
    , dataDir = ""
    , dataFile = ""
    , trueClass = ""
    }

-- conf2filename :: TestConfig -> String
-- conf2filename conf = (resultsDir conf)++"/"++(dataFile conf)++"-"++(fst alg)++"-"++(show $ factor conf)++{-"-"++(show seed)++-}".csv"

-- performTest :: Int -> (String,BoolEnsemble (NaiveBayes.NBayes Bool)) -> Double -> [(Bool,DataPoint)] -> (String,String)
performTest conf (strTrainer,trainer) bds = (out,res)
    where
        rgen = mkStdGen $ seed conf
        res = strTrainer++","++(show $ seed conf)++","++(show $ ldr conf)++","++(list2csv $ perfTrace ens test_data)
        out = concat $ map (\x -> "\n"++x) aitrace
        (ens,aitrace) = (runWriter $ trainer ls (us))
        (train_data,test_data) = randSplit rgen (tdr conf) bds
        (ls,us) = s2ss rgen (ldr conf) train_data

runTest conf alg = do
    putStrLn $ "TEST = "++(show conf)++"-"++(fst alg)++"-"++(show $ ldr conf)++"-"++(show $ seed conf)
    let outfile = (resultsDir conf)++"/"++(dataFile conf)++"-"++(fst alg)++"-"++(show $ ldr conf)++{-"-"++(show seed)++-}".csv"
    hout <- openFile outfile AppendMode
    dm <- loadData $ (dataDir conf)++"/"++(dataFile conf)
    test2file hout $ do
        ds <- dm
        let bds = toBinaryData (trueClass conf) ds
        return $ performTest conf alg bds
    hClose hout

test2file :: (Show a) => Handle -> Either a (String,String) -> IO ()
test2file hout (Left err) = putStrLn $ show err
test2file hout (Right (std,file)) = do
    putStrLn std
    putStrLn "Writing next line to CSV"
    putStrLn file
    hPutStrLn hout file
    hFlush hout
    putStrLn "Done."

-- perfTrace :: Ensemble model -> [(Bool,[SqlValue])] -> [Double]
-- perfTrace (Ensemble es) ds = [ show $ middle (es !! i) | i <- [0..length es]] 
-- perfTrace (Ensemble es) ds = [ eval (classify $ Ensemble $ drop i es) ds | i <- [0..length es]] 
perfTrace (Ensemble es) ds = [ errorRate $ eval (classify $ Ensemble $ drop i es) ds | i <- [0..length es]] 
