{-# LANGUAGE FlexibleInstances,FlexibleContexts #-}

module AI.Testing
    where

import AI.Classification
import AI.DataLoader
import AI.RandomUtils
-- import AI.Classification.SemiBoost
import AI.Classification.Boosting

import Control.Monad
import Control.Monad.Writer

import qualified Data.Map as Map
import Data.Maybe

import Debug.Trace
import System.IO
import System.Random

-------------------------------------------------------------------------------

data TestConfig = TestConfig
    { ldr :: DataRate -- ^ labeled data rate
    , tdr :: DataRate -- ^ training data rate.  Set equal to number of 1-(1/number of folds).
    , seed :: Int
    , inductive :: Bool
    , resultsDir :: String
    , dataDir :: String
    , datafile :: DatafileDesc
    , testAlg :: EnsembleContainer
    } deriving Show
    
defTest = TestConfig
    { ldr = Relative 0.2
    , tdr = Relative 0.8
    , seed = 0
    , inductive = True
    , resultsDir = "."
    , dataDir = "."
    , datafile = defDatafileDesc
    , testAlg = error "defTest: must specify which algorithm to test"
    }

getDataRateFactor :: DataRate -> Int -> Double
getDataRateFactor (Absolute amount) n = (fromIntegral amount)/(fromIntegral n)
getDataRateFactor (Relative factor) _ = factor

-------------------------------------------------------------------------------

conf2filename :: (ClassifyModel model labelType) => TestConfig -> model -> String
conf2filename conf model = (resultsDir conf)++"/"++(datafileName $ datafile conf)++"-"++(modelName model)++"-ldr="++(show $ ldr conf)++"-tdr="++(show $ tdr conf)++{-"-"++(show seed)++-}".csv"

performTest :: TestConfig -> TrainingData Bool -> Map.Map String [String]
performTest conf bds = Map.insert "performance" (reverse $ map show $ perfTrace ens finaltestdata) aitrace
    where
        rgen = mkStdGen $ seed conf
        (ens,aitrace) = (runAI (seed conf) $ train (testAlg conf) ls (us))
        (train_data,test_data) = randSplit rgen (getDataRateFactor (tdr conf) (length bds)) bds
        (ls,us) = s2ss rgen (getDataRateFactor (ldr conf) (length train_data)) train_data
        
        finaltestdata =
            if (inductive conf)
               then test_data
               else train_data
               
runTest :: TestConfig -> IO ()
runTest conf = do
    putStrLn $ "TEST = "++(show conf)++"-"++(modelName $ testAlg conf)++"-"++(show $ ldr conf)++"-"++(show $ seed conf)
    dm <- loadData $ applyDirPrefix (dataDir conf) (datafile conf) 
    test2file conf $ do
        ds <- dm
        let bds = toBinaryData (datafileTrueClass $ datafile conf) ds
        return $ performTest conf bds

runTests :: [TestConfig] -> IO ()
runTests ts = do
    sequence_ [ runTest t | t <- ts ]


test2file :: (Show a) => TestConfig -> Either a (Map.Map String [String]) -> IO ()
test2file conf (Left err) = putStrLn $ show err
test2file conf (Right logai) = do
    let outfile = trace ("outfile="++(conf2filename conf $ testAlg conf)) $ conf2filename conf $ testAlg conf
    hout <- openFile outfile AppendMode
    putStrLn $ concat $ map ("\n"++) $ Map.findWithDefault [] "print" logai 
    putStrLn "Writing next line to CSV"
    putStrLn $ csvLine "performance"
    hPutStrLn hout $ csvLine "performance"
    hPutStrLn hout $ csvLine "aveMargin"
    hFlush hout
    putStrLn "Done."
    hClose hout
    
    where
        csvLine str = (show $ testAlg conf)++","++(show $ seed conf)++","++str++","++(list2csv $ reverse $ Map.findWithDefault ["csvLine error"] str logai)

-- perfTrace (Ensemble params es) ds = [ errorRate $ genConfusionMatrix (classify $ Ensemble params $ drop i es) ds | i <- [0..length es]] 
perfTrace (EC (Ensemble params es)) ds = [ errorRate $ genConfusionMatrix (classify $ Ensemble params $ drop i es) ds | i <- [0..length es]] 
