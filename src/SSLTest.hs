module Main
    where

import AI.Classification
import AI.DataLoader
import AI.Ensemble
import AI.Testing

import Control.Parallel
import Control.Parallel.Strategies

import qualified AI.Supervised.DecisionStump as DecisionStump
import qualified AI.Supervised.NaiveBayes as NaiveBayes
import qualified AI.Supervised.KNN as KNN
import AI.Boosting

-- test = parMap (putStrLn
          
main = do
    let boostParams = BoostParams 
            { iterations = 50
            , sampleRate = 500
            , trainer = DecisionStump.train
--             , trainer = NaiveBayes.train
--             , trainer = KNN.train 3
            }
        
    sequence 
        [ runTest defTest 
            { dataFile=fst testdatafile
            , trueClass=snd testdatafile
            , ldr=0.1
            , tdr=0.05
            , seed=seed
            , dataDir ="../testdata"
            , resultsDir="../results"
            }
            alg
        | testdatafile <- 
--             ("tic-tac-toe.data","positive"):
--             ("haberman.data","1") :
--             ("optdigits.data","2") :
--             ("german.data","1") :
--             ("kr-vs-kp.data","won"):
            ("ringnorm.data","1") :
--             ("twonorm.data","1") :
--             ("ionosphere.data","g"):
            []
        , alg <- 
--             ("regAssembleBoost2d",AI.Boosting.train regAssembleBoost2 boostParams):
--             ("regAssembleBoost1e",AI.Boosting.train regAssembleBoost1 boostParams):
            ("ssmBoost",AI.Boosting.train ssmBoost boostParams):
            ("assembleBoost",AI.Boosting.train assembleBoost boostParams):
--             ("semiBoost-DS",AI.Boosting.train semiBoost boostParams):
--             ("adaBoost",AI.Boosting.train adaBoost boostParams):
            ("marginBoost",AI.Boosting.train marginBoost boostParams):
            []
        , ldr <- [ 0.01 ]
        , seed <- [1..50]
        ]
