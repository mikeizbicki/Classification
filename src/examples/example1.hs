{- 

This example partially reproduces the experiments in the paper "A study of AdaBoost with Naive Bayesian Classifiers: Weakness and Improvement"

The paper can be downloaded at: http://www.cs.odu.edu/~pflynn/survey/classifiers/1467-8640.pdf

-}

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
          
data DatafileDesc = DatafileDesc
    { datafileName :: String
    , datafileTrueClass :: String
    , datafileMissingStr :: Maybe String
    , datafileForce :: Maybe [String -> DataItem]
    }
          
main = do
    let boostParams = BoostParams 
            { iterations = 100
            , sampleRate = 500
            , obeyStopCriteria = False
            , trainer = NaiveBayes.train
            }
        
    sequence 
        [ runTest defTest 
            { dataFile=datafileName testdatafile
            , trueClass=datafileTrueClass testdatafile
            , dataFileMissingStr=datafileMissingStr testdatafile
            , dataFileForce=datafileForce testdatafile
            , ldr=1
            , tdr=0.66
            , inductive = False
            , seed=seed
            , dataDir ="/home/user/proj/haskell-classification/src/examples/example1-testdata/"
            , resultsDir="/home/user/proj/haskell-classification/src/examples/example1-results/"
            }
            alg
        | testdatafile <- 
--             DatafileDesc { datafileName="tic-tac-toe.data"
--                          , datafileTrueClass="positive"
--                          , datafileMissingStr=Nothing
--                          , datafileForce=Nothing
--                          }:
--             DatafileDesc { datafileName="kr-vs-kp.data"
--                          , datafileTrueClass="won"
--                          , datafileMissingStr=Nothing
--                          , datafileForce=Nothing
--                          }:
            DatafileDesc { datafileName="breast-cancer-wisconsin.data"
                         , datafileTrueClass="2"
                         , datafileMissingStr=Just "?"
                         , datafileForce=Nothing
                         }:
            []
        , alg <- 
--             ("adaBoost",AI.Boosting.train adaBoost boostParams):
            ("marginBoost-nostop",AI.Boosting.train marginBoost boostParams):
            []
        , seed <- [1..10]
        ]
