import AI.Classification

import AI.Supervised.AdaBoost as AdaBoost
import AI.Supervised.NaiveBayes as NaiveBayes

import Criterion.Config
import Criterion.Main
import Database.HDBC

import Control.Monad.Random
import Control.Monad.Writer
-- import System.Random

myConfig = defaultConfig
            { 
--               cfgPerformGC = ljust True
--             , cfgSamples = ljust 3
--             , cfgReport = ljust "perf.txt"
             cfgSummaryFile = ljust "perf.csv"
--             , cfgResamples = ljust 0
--             , cfgVerbosity = ljust Quiet
            }

main = defaultMainWith myConfig (return ())
    [ bench ("BinaryNaiveBayes ex="++show ex++" dim="++show dim) $ runTest ex dim
    | ex <- map (*10) [1..5]
    , dim <- map (*5) [1..5]
    ]
    
runTest ex dim = do
    bds <- evalRandIO $ createData ex dim []
    putStrLn $ show $ NaiveBayes.train bds
{-    let bnb = {-toBinaryClassifier "1" $-} NaiveBayes.classify -- (NaiveBayes.train ds)
    let bnbc = NaiveBayes.classify (NaiveBayes.train bds)
    let (ada,out) = (runWriter $ AdaBoost.trainM NaiveBayes.train bnb bds)
    let adac= AdaBoost.classify ada
    return $ (out,eval bnbc bds)-}
        
createData :: (RandomGen g) => Int -> Int -> [(Bool,DataPoint)] -> Rand g [(Bool,DataPoint)]
createData 0  dim xs = return xs
createData ex dim xs = do
    dp <- createDataPoint dim []
    r <- getRandomR (0,100)
    let label=(r::Double)<50
    createData (ex-1) dim ((label,dp):xs)
    
createDataPoint :: (RandomGen g) => Int -> DataPoint -> Rand g DataPoint
createDataPoint 0 xs = return xs
createDataPoint n xs = do
    r <- getRandomR (1,100)
    createDataPoint (n-1) ((toSql (r::Double)):xs)
