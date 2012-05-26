module AI.Ensemble
    where

import Database.HDBC
import Debug.Trace

import AI.Classification

-- data types

data Ensemble model = Ensemble ![(Double,[SqlValue]->Bool)]

instance (Show b) => Show (Ensemble b) where
    show (Ensemble xs) = show $ map (\(a,b)->(a)) xs


-- classification

weightedClassify :: (Ensemble a) -> DataPoint -> Double
weightedClassify (Ensemble xs) dp = sum $ map (\(alpha,c) -> alpha * (bool2num $ c dp) ) xs

classify :: (Ensemble a) -> DataPoint -> Bool
classify ens dp = num2bool $ weightedClassify ens dp

-- similarity functions

similarity :: DataPoint -> DataPoint -> Double
similarity dp1 dp2 = 1 -- {-trace (" << "++show res)-} res
    where res = exp $ diff^2 / sigma^2
          sigma = 1
          diff = sum [ (fromSql x1)-(fromSql x2) | (x1,x2) <- zip dp1' dp2'] :: Double
          dp1' = filter real dp1
          dp2' = filter real dp2
          real sqlval = case sqlval of
                             (SqlDouble x) -> True
                             (SqlString x) -> error "similarity: SqlString" --False