{-# LANGUAGE FlexibleContexts #-}

module Main
    where

import System.IO
import Test.HUnit

import AI.Classification
import AI.Ensemble
import AI.Supervised.DecisionStump as DecisionStump
import AI.Supervised.NaiveBayes as NaiveBayes
import AI.Supervised.KNN as KNN

-------------------------------------------------------------------------------
-- code for performing tests

evalAI hout task = do
    hPutStrLn hout $ show task

testTrainer hout trainer dataset = TestCase $ do
    evalAI hout $ trainer dataset

-- testClassifier :: Handle -> (TrainingData String -> NBayes String) -> TrainingData String -> DataPoint -> Test
testClassifier hout trainer dataset testdata = TestCase $ do
    evalAI hout $ (classify (trainer dataset :: NBayes String) testdata :: String)

-------------------------------------------------------------------------------
-- test datasets; mostly hard edge cases

dataset_continuous = 
    [("male",map (toDataItem::Double->DataItem) [6.0,180,12])
    ,("male",map (toDataItem::Double->DataItem) [5.92,190,11])
    ,("male",map (toDataItem::Double->DataItem) [5.58,170,12])
    ,("male",map (toDataItem::Double->DataItem) [5.92,165,10])
    ,("female",map (toDataItem::Double->DataItem) [5.5,130,7])
    ,("female",map (toDataItem::Double->DataItem) [5.5,150,8])
    ,("female",map (toDataItem::Double->DataItem) [5.42,130,7])
    ,("female",map (toDataItem::Double->DataItem) [5.75,150,9])
    ]
    
dataset_discrete = 
    [("male",map toDataItem ["fat"])
    ,("male",map toDataItem ["fat"])
    ,("male",map toDataItem ["fat"])
    ,("male",map toDataItem ["thin"])
    ,("female",map toDataItem ["thin"])
    ,("female",map toDataItem ["thin"])
    ,("female",map toDataItem ["thin"])
    ,("female",map toDataItem ["fat"])
    ]
    
dataset_mixed = [ (l1,d1++d2) | ((l1,d1),(l2,d2)) <- zip dataset_discrete dataset_continuous]

classify_mixed_normal = [Discrete "thin",Continuous 6, Continuous 180, Continuous 8]
classify_mixed_missingdiscrete = [Missing, Continuous 6, Continuous 180, Continuous 8]
classify_mixed_missingcontinuous = [Discrete "thin", Continuous 6, Continuous 180, Missing]
classify_mixed_missingall = [Missing, Missing, Missing, Missing]

dataset_repeats = concat $ replicate 100 dataset_mixed
    
dataset_empty = []

dataset_one = take 1 dataset_mixed

dataset_two = take 2 dataset_mixed

dataset_missing1 = (label,Missing:rest):(tail dataset_mixed)
    where
        label = fst $ head dataset_mixed
        rest = tail $ snd $ head dataset_mixed
        
dataset_missingrow = dataset_mixed++[(label, replicate len Missing)]
    where
        label = fst $ head dataset_mixed
        len = length $ snd $ head dataset_mixed
                
-------------------------------------------------------------------------------
-- main procedure
    
tests_trainer hout alg_label trainer = TestList 
    [ TestLabel ("train:"++alg_label++"-"++dataset_label) $ testTrainer hout trainer dataset
    | (dataset_label,dataset) <- 
        ("dataset_continuous",dataset_continuous):
        ("dataset_discrete",dataset_discrete):
        ("dataset_mixed",dataset_mixed):
        ("dataset_repeats",dataset_repeats):
        ("dataset_empty",dataset_empty):
        ("dataset_one",dataset_one):
        ("dataset_two",dataset_two):
        ("dataset_missing1",dataset_missing1):
        ("dataset_missingrow",dataset_missingrow):
        []
    ]
    
tests_classifier hout alg_label trainer = TestList
    [ TestLabel ("classify:"++alg_label++"-"++dataset_label++"-"++testdata_label) $ testClassifier hout trainer dataset testdata
    | (testdata_label,testdata) <-
        ("classify_mixed_normal",classify_mixed_normal):
        ("classify_mixed_missingdiscrete",classify_mixed_missingdiscrete):
        ("classify_mixed_missingcontinuous",classify_mixed_missingcontinuous):
        ("classify_mixed_missingall",classify_mixed_missingall):
        []
    , (dataset_label,dataset) <- 
        ("dataset_mixed",dataset_mixed):
        []
    ]
    
tests_alg hout alg_label trainer = TestList
    [ tests_trainer hout alg_label trainer
    , tests_classifier hout alg_label trainer
    ]
    
tests hout = TestList $
    (tests_alg hout "NaiveBayes"      NaiveBayes.train):
--     (tests_singleAlg hout "DecisionStump"   DecisionStump.train):
--     (tests_singleAlg hout "3-NN"            $ KNN.train 3):
--     (tests_singleAlg hout "1-NN"            $ KNN.train 1):
    []
    
main = do
    hout <- openFile "tests.out" WriteMode
    runTestTT $ tests hout
    hClose hout