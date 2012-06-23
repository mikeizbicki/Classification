{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}

module AI.Ensemble
    where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

import Debug.Trace
import Data.List.Extras
import Data.Monoid
import AI.Classification

-------------------------------------------------------------------------------
-- data types

-- class ClassifyModelConf modelConf where
    

class ClassifyModel model label | model -> label where
    modelName :: model -> String
    
    train :: model -> TrainingData label -> UnlabeledData -> LogAI model

    probClassify :: model -> DataPoint -> [(label,Prob)]
    
    classify :: model -> DataPoint -> label
    classify model dp = fst $ argmaxBy compare snd $ probClassify model dp

-- data Classifier label = forall model . (ClassifyModel model label)=>Classifier model

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
