{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module AI.Supervised.KNN
    ( train
    , classify
    , probClassify
    )
    where

import Data.List
import Data.List.Extras
import Data.Trees.KdTree

import AI.Classification
import AI.Ensemble

-------------------------------------------------------------------------------
-- Data types

data KNN label = KNN 
    { kdtree :: KdTree (label,DataPoint)
    , k :: Int
    }
    deriving Show

instance Point (label,DataPoint) where
    dimension (l,p) = length p
    coord n (l,p) = fromDataItem (p !! n)
    
    dist2 a b = sum . map diff2 $ [0..dimension a - 1]
        where diff2 i = (coord i a - coord i b)^2
-------------------------------------------------------------------------------
-- Training

train :: (Ord label) => Int -> [(label,DataPoint)] -> KNN label
train k xs = KNN 
    { kdtree=fromList xs
    , k=k
    }

-------------------------------------------------------------------------------
-- classification

instance (Ord a) => ClassifyModel (KNN a) a where
    
    probClassify knn dp = map reduce $ groupBy sameLabel neighbors
        where
            reduce xs = (fst $ head xs,(fromIntegral $ length xs)/(fromIntegral $ k knn))
            sameLabel (l1,d1) (l2,d2) = l1==l2
            neighbors = kNearestNeighbors (kdtree knn) (k knn) (undefined,dp)

----

dataset = [("male",[6.0,180,12])
          ,("male",[5.92,190,11])
          ,("male",[5.58,170,12])
          ,("male",[5.92,165,10])
          ,("male",[5.5,130,7])
          ,("female",[5.5,150,8])
          ,("female",[5.42,130,7])
          ,("female",[5.75,150,9])
          ]

dataset' = [("male",[6.0,180,12])
          ]

sqldata = map (\(label,xs) -> (label,map Continuous xs)) dataset
bdataset = toBinaryData "male" sqldata

trained = train 3 sqldata 
test = map Continuous [6.0,150,9]
-- sample = [toSql (5::Double), toSql (130::Double), toSql (8::Double)]