module AI.Ensemble
    where

import Database.HDBC
import Debug.Trace

import AI.Classification

-- data types

data Ensemble model = Ensemble ![(Double,model,BoolClassifier model)]

instance (Show b) => Show (Ensemble b) where
    show (Ensemble xs) = show $ map (\(a,b,c)->(a)) xs

type SSTrainer model = Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)
type STrainer model  = Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (Ensemble model)

-- classification

weightedClassify :: (Ensemble a) -> DataPoint -> Double
weightedClassify (Ensemble xs) dp = (sum $ [alpha * (bool2num $ c model dp) | (alpha,model,c) <- xs])
                                    -- (sum $ map (\(alpha,c) -> alpha * (bool2num $ c dp) ) xs)
--                                   / (sum $ [ abs alpha | (alpha,model,c) <- xs ])

classify :: (Ensemble a) -> DataPoint -> Bool
classify ens dp = num2bool $ weightedClassify ens dp



-- similarity functions

similarity :: DataPoint -> DataPoint -> Double
similarity dp1 dp2 = {-trace (" << "++show diff)-} res
    where res = exp $ (-1 * (abs $ diff) / sigma^2)
          sigma = 1
          diff = sum [ sqlminus x1 x2 | (x1,x2) <- zip dp1 dp2] :: Double
          
          
sqlminus :: SqlValue -> SqlValue -> Double
sqlminus (SqlDouble x1) (SqlDouble x2) = x1 - x2
sqlminus (SqlString x1) (SqlString x2) = 
    if (x1==x2)
       then 0
       else 1

                             
-- similarity :: DataPoint -> DataPoint -> Double
-- similarity dp1 dp2 = {-1 ---} {-trace (" << "++show diff)-} res
--     where res = exp $ (abs $ diff) / sigma^2
--           sigma = 1
--           diff = sum [ (fromSql x1)-(fromSql x2) | (x1,x2) <- zip dp1' dp2'] :: Double
--           dp1' = filter real dp1
--           dp2' = filter real dp2
--           real sqlval = case sqlval of
--                              (SqlDouble x) -> True
--                              (SqlString x) -> error "similarity: SqlString" --False