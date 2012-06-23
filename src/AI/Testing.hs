{-# LANGUAGE FlexibleInstances,FlexibleContexts #-}

module AI.Testing
    where

import AI.Classification
import AI.DataLoader
import AI.Ensemble
import AI.RandomUtils
import AI.ASSEMBLE2

import Control.Monad
import Control.Monad.Writer

import Debug.Trace
import System.IO
import System.Random

-------------------------------------------------------------------------------

data DatafileDesc = DatafileDesc
    { datafileName :: String
    , datafileTrueClass :: String
    , datafileMissingStr :: Maybe String
    , datafileForce :: Maybe [String -> DataItem]
    }
    deriving Show

defDatafileDesc=DatafileDesc
    { datafileName = ""
    , datafileTrueClass = ""
    , datafileMissingStr = Nothing
    , datafileForce = Nothing
    }

data TestConfig = TestConfig
    { ldr :: DataRate -- ^ labeled data rate
    , tdr :: DataRate -- ^ training data rate.  Set equal to number of 1-(1/number of folds).
    , seed :: Int
    , inductive :: Bool
    , resultsDir :: String
    , dataDir :: String
    , datafile :: DatafileDesc
    } deriving Show
    
instance Show (String->DataItem) where
    show xs = ""
    
defTest = TestConfig
    { ldr = Relative 0.2
    , tdr = Relative 0.8
    , seed = 0
    , inductive = True
    , resultsDir = "."
    , dataDir = "."
    , datafile = defDatafileDesc
    }

getDataRateFactor :: DataRate -> Int -> Double
getDataRateFactor (Absolute amount) n = (fromIntegral amount)/(fromIntegral n)
getDataRateFactor (Relative factor) _ = factor

-------------------------------------------------------------------------------

conf2filename :: (ClassifyModel model labelType) => TestConfig -> model -> String
conf2filename conf model = (resultsDir conf)++"/"++(datafileName $ datafile conf)++"-"++(modelName model)++"-ldr="++(show $ ldr conf)++"-tdr="++(show $ tdr conf)++{-"-"++(show seed)++-}".csv"

-- performTest :: Int -> (String,BoolEnsemble (NaiveBayes.NBayes Bool)) -> Double -> [(Bool,DataPoint)] -> (String,String)
{-performTest conf (strTrainer,trainer) bds = (out,res)-}
-- performTest :: (ClassifyModel model Bool) => TestConfig model -> model -> TrainingData Bool -> (String,String)
-- performTest :: (ClassifyModel (Ensemble model1) label0) => TestConfig model -> Ensemble model1 -> TrainingData Bool -> (String,String)
performTest :: (ClassifyModel model Bool) => TestConfig -> Ensemble model -> TrainingData Bool -> (String,String)
performTest conf model bds = (out,res)
    where
        rgen = mkStdGen $ seed conf
        res = "\""++(modelName model)++"\",\""++(show $ seed conf)++"\",\""++(show $ ldr conf)++"\","++(list2csv $ perfTrace ens finaltestdata)
        out = concat $ map (\x -> "\n"++x) aitrace
        (ens,aitrace) = (runAI (seed conf) $ train model ls (us))
        (train_data,test_data) = randSplit rgen (getDataRateFactor (tdr conf) (length bds)) bds
        (ls,us) = s2ss rgen (getDataRateFactor (ldr conf) (length train_data)) train_data
        
        finaltestdata =
            if (inductive conf)
               then test_data
               else train_data
               
runTest :: (ClassifyModel model Bool) => TestConfig -> Ensemble model -> IO ()
runTest conf alg = do
    putStrLn $ "TEST = "++(show conf)++"-"++(modelName alg)++"-"++(show $ ldr conf)++"-"++(show $ seed conf)
    let outfile = conf2filename conf alg
    hout <- openFile outfile AppendMode
    dm <- loadData ((dataDir conf)++"/"++(datafileName $ datafile conf)) (datafileMissingStr $ datafile conf) (datafileForce $ datafile conf)
    test2file hout $ do
        ds <- dm
        let bds = toBinaryData (datafileTrueClass $ datafile conf) ds
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
perfTrace (Ensemble params es) ds = [ errorRate $ genConfusionMatrix (classify $ Ensemble params $ drop i es) ds | i <- [0..length es]] 
