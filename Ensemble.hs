module Ensemble
    where

import Database.HDBC

import Classification

-- data types

data Ensemble model = Ensemble ![(Double,[SqlValue]->Bool)]

instance (Show b) => Show (Ensemble b) where
    show (Ensemble xs) = show $ map (\(a,b)->(a)) xs


-- classification

classify :: (Ensemble a) -> DataPoint -> Bool
classify (Ensemble xs) dp = num2bool $ sum list
    where
          list = map (\(alpha,c) -> alpha * (bool2num $ c dp) ) xs

