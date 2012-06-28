module Main
    where

import AI.Classification
import AI.DataLoader
import AI.Testing

import AI.Classification.DecisionTree
import AI.Classification.NaiveBayes
import AI.Classification.KNN
-- import AI.Classification.SemiBoost
import AI.Classification.Boosting
import AI.Classification.Boosting.AdaBoost
import AI.Classification.Boosting.ASSEMBLE
import AI.Classification.Boosting.LogitBoost
import AI.Classification.Boosting.LogitASSEMBLE
import AI.Classification.Boosting.EntropyBoost
import AI.Classification.Boosting.MutualInformationBoost
import AI.Classification.Boosting.RegularizedBoost

main = do
    let boostParams = BoostParams 
            { iterations = 20
            , sampleRate = Auto -- Relative 0.1
            , obeyStopCriteria = True
            , modelConf = (defDTree {maxDepth=4})
--             , modelConf = (defKNN {k=3})
--             , modelConf = (defNBayes)
            , boostAlg = ASSEMBLE
            }
        
    runTests
        [ defTest 
            { datafile=testdatafile
            , ldr=Relative 0.2
            , tdr=Relative 0.8
            , inductive = False
            , seed=seed
            , dataDir ="../testdata"
            , resultsDir="../results"
            , testAlg = alg
            }
        | testdatafile <- 
--             ("tic-tac-toe.data","positive"):
--             ("german.data","1") :
--             ("kr-vs-kp.data","won"):
--             ("ringnorm.data","1") :
--             ("twonorm.data","1") :
--             ("ionosphere.data","g"):
--             defDatafileDesc { datafileName="haberman.data",datafileTrueClass="1"} :
--             defDatafileDesc {datafileName="pima-indians-diabetes.data",datafileTrueClass="1"}:
            defDatafileDesc {datafileName="bupa.data",datafileTrueClass="1"}:
--             defDatafileDesc {datafileName="optdigits.data",datafileTrueClass="2"}:
--             DatafileDesc { datafileName="breast-cancer-wisconsin.data"
--                          , datafileTrueClass="2"
--                          , datafileMissingStr=Just "?"
--                          , datafileForce=Nothing
--                          }:

            []
        , alg <- 
--             (mkBoost $ boostParams { boostAlg=AdaBoost }):
--             (mkBoost $ boostParams { boostAlg=ASSEMBLE }):
--             (mkBoost $ boostParams { boostAlg=LogitBoost }):
--             (mkBoost $ boostParams { boostAlg=LogitASSEMBLE }):
            
--             (mkBoost $ boostParams { boostAlg=SemiBoost }):
            (mkBoost $ boostParams { boostAlg=RegularizedBoost }):
--             (mkBoost $ boostParams { boostAlg=EntropyBoost }):
--             (mkBoost $ boostParams { boostAlg=MutualInformationBoost }):
            []
--         , tdr <- map Absolute [{-200,400,-}600]
        , seed <- [1..20]
        ]
