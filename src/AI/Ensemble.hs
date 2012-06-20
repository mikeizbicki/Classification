{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}

module AI.Ensemble
    where

import Debug.Trace
import Data.List.Extras
import AI.Classification

-------------------------------------------------------------------------------
-- data types

class ClassifyModel model label where 
    probClassify :: model -> DataPoint -> [(label,Prob)]
    
    classify :: model -> DataPoint -> label
    classify model dp = fst $ argmaxBy compare snd $ probClassify model dp
    
data Ensemble model = Ensemble ![(Double,model)]

instance (Show b) => Show (Ensemble b) where
    show (Ensemble xs) = show $ map (\(a,b)->(a)) xs

instance (ClassifyModel basemodel Bool) => ClassifyModel (Ensemble basemodel) Bool where
    classify ens dp = num2bool $ weightedClassify ens dp    
    probClassify = error "Ensemble.probClassify not yet implemented"

-- data Trainable = forall model label . ClassifyModel model label => MkTrainable model label


-- classification

weightedClassify :: (ClassifyModel a Bool) => (Ensemble a) -> DataPoint -> Double
weightedClassify (Ensemble xs) dp = (sum $ [alpha * (bool2num $ classify model dp) | (alpha,model) <- xs])


-- similarity functions

similarity :: DataPoint -> DataPoint -> Double
similarity dp1 dp2 = {-trace (" << "++show diff)-} res
    where res = exp $ (-1 * (sqrt $ diff) / (2*sigma^2))
          sigma = 5
          diff = sum [ dataminus x1 x2 | (x1,x2) <- zip dp1 dp2] :: Double
          
          
dataminus :: DataItem -> DataItem -> Double
dataminus (Continuous x1) (Continuous x2) = (x1 - x2)^2
dataminus (Discrete x1) (Discrete x2) = 
    if (x1==x2)
       then 0
       else 1
