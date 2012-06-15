module Main
    where

import AI.Classification
import AI.DataLoader
import AI.Ensemble
import AI.Testing

import qualified AI.Supervised.DecisionStump as DecisionStump
import qualified AI.Supervised.NaiveBayes as NaiveBayes
import AI.Boosting --as Boosting
import AI.SemiSupervised.SemiMarginBoost as SemiMarginBoost
          
main = do
    sequence 
        [ runTest defTest { dataFile=fst testdatafile
                          , trueClass=snd testdatafile
                          , dataDir ="../testdata"
                          , resultsDir="../results"
                          }
            alg factor seed
        | testdatafile <- 
--             ("tic-tac-toe.data","positive"):
--             ("haberman.data","1") :
--             ("optdigits.data","2") :
--             ("german.data","1") :
            ("kr-vs-kp.data","won"):
--             ("ringnorm.data","1") :
--             ("twonorm.data","1") :
--             ("ionosphere.data","g"):
            []
        , alg <- 
            ("Boosting-NB",AI.Boosting.train ssmBoost
                BoostParams { AI.Boosting.iterations = 10
                            , AI.Boosting.sampleRate = 100
                            , AI.Boosting.trainer = NaiveBayes.train
                            }
                 ):
            []
--             , factor <- [ 0.2, 0.1, 0.05 ]
        , factor <- [ 0.1 ]
--         , count <- [4]
--             , seed <- [1..10]
        , seed <- [2000]
        ]

