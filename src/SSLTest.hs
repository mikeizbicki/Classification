module Main
    where

import AI.Classification
import AI.DataLoader
import AI.Ensemble
import AI.Testing

import Control.Parallel
import Control.Parallel.Strategies

import AI.Supervised.DecisionTree
-- import qualified AI.Supervised.DecisionStump as DecisionStump
import AI.Supervised.NaiveBayes
import AI.Supervised.KNN
-- import qualified AI.Supervised.DecisionTree as DecisionTree
-- import qualified AI.Supervised.DecisionStump as DecisionStump
-- import qualified AI.Supervised.NaiveBayes as NaiveBayes
-- import qualified AI.Supervised.KNN as KNN
-- import qualified AI.Boosting as Boosting
-- import qualified AI.ASSEMBLE as ASSEMBLE
import AI.ASSEMBLE2 
          
main = do
    let boostParams = BoostParams 
            { iterations = 50
            , sampleRate = Auto
            , obeyStopCriteria = True
            , modelConf = (defDTree {maxDepth=4}) {-:: DTree Bool-}
--             , trainer = DecisionStump.train
--             , trainer = NaiveBayes.train
--             , trainer = KNN.train 3
            }
    let assemble = Ensemble boostParams [] :: Ensemble (DTree Bool)
        
    sequence 
        [ runTest defTest 
            { datafile=testdatafile
            , ldr=Absolute 200
            , tdr=tdr -- Relative 0.8
            , inductive = True
            , seed=seed
            , dataDir ="../testdata"
            , resultsDir="../results"
            }
            alg
        | testdatafile <- 
--             ("tic-tac-toe.data","positive"):
--             defDatafileDesc { datafileName="haberman.data",datafileTrueClass="1"} :
--             ("optdigits.data","2") :
--             ("german.data","1") :
--             ("kr-vs-kp.data","won"):
--             ("ringnorm.data","1") :
--             ("twonorm.data","1") :
--             ("ionosphere.data","g"):
            defDatafileDesc {datafileName="pima-indians-diabetes.data",datafileTrueClass="1"}:
            []
        , alg <- 
--             ("regAssembleBoost2",AI.Boosting.train regAssembleBoost2 boostParams):
--             ("regAssembleBoost1",AI.Boosting.train regAssembleBoost1 boostParams):
--             ("ssmBoost",AI.Boosting.train ssmBoost boostParams):
--             ("assembleBoost",AI.Boosting.train assembleBoost boostParams):
--             ("semiBoost-DS",AI.Boosting.train semiBoost boostParams):
--             ("adaBoost-ds",Boosting.train Boosting.adaBoost boostParams):
--             ("ASSEMBLE",ASSEMBLE.train boostParams):
--             ("ASSEMBLE2",assemble):
            assemble:
            []
        , tdr <- map Absolute [200,400,600]
        , seed <- [101..300]
        ]
