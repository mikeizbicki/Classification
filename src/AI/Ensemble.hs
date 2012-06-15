{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module AI.Ensemble
    where

import Database.HDBC
import Debug.Trace

import AI.Classification

-- data types

class ClassifyModel model label where 
    classify :: model -> DataPoint -> label

data Ensemble model = Ensemble ![(Double,model)]

instance (Show b) => Show (Ensemble b) where
    show (Ensemble xs) = show $ map (\(a,b)->(a)) xs

instance (ClassifyModel basemodel Bool) => ClassifyModel (Ensemble basemodel) Bool where
--     classify :: (Ensemble basemodel) -> DataPoint -> Bool
    classify ens dp = num2bool $ weightedClassify ens dp    

type SSTrainer model = Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)
type STrainer model  = Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (Ensemble model)

type BoolEnsemble model = [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)

-- classification

weightedClassify :: (ClassifyModel a Bool) => (Ensemble a) -> DataPoint -> Double
weightedClassify (Ensemble xs) dp = (sum $ [alpha * (bool2num $ classify model dp) | (alpha,model) <- xs])
                                    -- (sum $ map (\(alpha,c) -> alpha * (bool2num $ c dp) ) xs)
--                                   / (sum $ [ abs alpha | (alpha,model,c) <- xs ])


-- similarity functions

similarity :: DataPoint -> DataPoint -> Double
similarity dp1 dp2 = {-trace (" << "++show diff)-} res
    where res = exp $ (-1 * (sqrt $ diff) / (2*sigma^2))
          sigma = 1
          diff = sum [ sqlminus x1 x2 | (x1,x2) <- zip dp1 dp2] :: Double
          
          
sqlminus :: SqlValue -> SqlValue -> Double
sqlminus (SqlDouble x1) (SqlDouble x2) = (x1 - x2)^2
sqlminus (SqlString x1) (SqlString x2) = 
    if (x1==x2)
       then 0
       else 1
