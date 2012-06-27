{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, NoMonomorphismRestriction #-}

module Main
    where

import System.IO
import Test.HUnit

import AI.Classification
import AI.Classification.DecisionTree
import AI.Classification.NaiveBayes
import AI.Classification.KNN

-------------------------------------------------------------------------------
-- code for performing tests

evalAI hout task = do
    hPutStrLn hout $ show task

testTrainer :: (ClassifyModel model String, Show model) => Handle -> model -> TrainingData String -> Test
testTrainer hout model dataset = TestCase $ do
    let (trained,dbgstr) = runAI 0 $ train model dataset []
    hPutStrLn hout $ show trained

testClassifier :: (ClassifyModel model String) => Handle -> model -> TrainingData String -> DataPoint -> Test
testClassifier hout model dataset testdata = TestCase $ do
    let (trained,dbgstr) = runAI 0 $ train model dataset []
    hPutStrLn hout $ show {-$ classify trained-} testdata

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

dataset_discreteallsame = 
    [("male",map toDataItem ["human"])
    ,("male",map toDataItem ["human"])
    ,("male",map toDataItem ["human"])
    ,("male",map toDataItem ["human"])
    ,("female",map toDataItem ["human"])
    ,("female",map toDataItem ["human"])
    ,("female",map toDataItem ["human"])
    ,("female",map toDataItem ["human"])
    ]

dataset_continuousallsame = 
    [("male",map (toDataItem::Double->DataItem) [5])
    ,("male",map (toDataItem::Double->DataItem) [5])
    ,("male",map (toDataItem::Double->DataItem) [5])
    ,("male",map (toDataItem::Double->DataItem) [5])
    ,("female",map (toDataItem::Double->DataItem) [5])
    ,("female",map (toDataItem::Double->DataItem) [5])
    ,("female",map (toDataItem::Double->DataItem) [5])
    ,("female",map (toDataItem::Double->DataItem) [5])
    ]

dataset_allsame = [ (l1,d1++d2) | ((l1,d1),(l2,d2)) <- zip dataset_discreteallsame dataset_continuousallsame]
dataset_mixedallsame = [ (l1,d1++d2) | ((l1,d1),(l2,d2)) <- zip dataset_mixed dataset_allsame]

---

dataset_discreteperfect = 
    [("male",map toDataItem ["male"])
    ,("male",map toDataItem ["male"])
    ,("male",map toDataItem ["male"])
    ,("male",map toDataItem ["male"])
    ,("female",map toDataItem ["female"])
    ,("female",map toDataItem ["female"])
    ,("female",map toDataItem ["female"])
    ,("female",map toDataItem ["female"])
    ]

classify_discreteperfect_normal = [Discrete "male"]
classify_discreteperfect_newdiscrete = [Discrete "hermaphrodite"]

----

-------------------------------------------------------------------------------
-- main procedure
    
tests_trainer :: (ClassifyModel model String, Show model) => Handle -> model -> Test
tests_trainer hout model = TestList 
    [ TestLabel ("train:"++(modelName model)++"-"++dataset_label) $ testTrainer hout model dataset
    | (dataset_label,dataset) <- 
        ("dataset_continuous",dataset_continuous):
        ("dataset_discrete",dataset_discrete):
        ("dataset_mixed",dataset_mixed):
        ("dataset_repeats",dataset_repeats):
        ("dataset_allsame",dataset_allsame):
        ("dataset_continuousallsame",dataset_continuousallsame):
        ("dataset_discreteallsame",dataset_discreteallsame):
        ("dataset_mixedallsame",dataset_mixedallsame):
--         ("dataset_empty",dataset_empty):
--         ("dataset_one",dataset_one):
        ("dataset_two",dataset_two):
        ("dataset_missing1",dataset_missing1):
        ("dataset_missingrow",dataset_missingrow):
        []
    ]
    
tests_classifier :: (ClassifyModel model String, Show model) => Handle -> model -> Test
tests_classifier hout model = TestList $
    [ TestLabel ("classify:"++(modelName model)++"-"++dataset_label++"-"++testdata_label) $ testClassifier hout model dataset testdata
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
    ++
    [ TestLabel ("classify:"++(modelName model)++"-"++dataset_label++"-"++testdata_label) $ testClassifier hout model dataset testdata
    | (testdata_label,testdata) <-
        ("classify_discreteperfect_normal",classify_discreteperfect_normal):
        ("classify_discreteperfect_newdiscrete",classify_discreteperfect_newdiscrete):
        []
    , (dataset_label,dataset) <- 
        ("dataset_discreteperfect",dataset_discreteperfect):
        []
    ]
    
tests_alg :: (ClassifyModel model String, Show model) => Handle -> model -> Test
tests_alg hout model = TestList
    [ tests_trainer hout model
    , tests_classifier hout model
    ]

tests :: Handle -> Test
tests hout = TestList $
    (tests_alg hout $ defDTree {maxDepth=1}):
    (tests_alg hout $ defNBayes):
    (tests_alg hout $ defKNN { k=3 }):
    (tests_alg hout $ defKNN { k=1 }):
    []
    
main = do
    hout <- openFile "tests.out" WriteMode
    runTestTT $ tests hout
    hClose hout