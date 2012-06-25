{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module AI.Supervised.KNN
    ( train
    , classify
    , probClassify
    , KNN (..)
    , defKNN
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

defKNN = KNN undefined 1

instance Point (label,DataPoint) where
    dimension (l,p) = length p
    
    coord n (l,p) = 
        case p !! n of
             Continuous x -> x
             otherwise    -> 0
    
    dist2 pt1@(l1,dp1) (l2,dp2) = sum . map diff2 $ [0..dimension pt1 - 1]
        where 
            diff2 i = (coordsub (dp1!!i) (dp2!!i))^2
              
            coordsub (Discrete a) (Discrete b) = 
                if a==b
                   then 0
                   else 1
            coordsub (Continuous a) (Continuous b) = a-b
            coordsub _ Missing = 0
            coordsub Missing _ = 0
            
-------------------------------------------------------------------------------
-- ClassifyModel definition

instance (Ord label) => ClassifyModel (KNN label) label where
    modelName knn = "KNN"++(show $ k knn)

    train knn td ud = return $ trainKNN (k knn) td

    probClassify = probClassifyKNN
            
-------------------------------------------------------------------------------
-- Training

trainKNN :: (Ord label) => Int -> [(label,DataPoint)] -> KNN label
trainKNN k xs = KNN 
    { kdtree=fromList xs
    , k=k
    }

-------------------------------------------------------------------------------
-- classification

probClassifyKNN knn dp = map reduce $ groupBy sameLabel neighbors
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

trained = train defKNN sqldata [] :: LogAI (KNN String)
test = map Continuous [6.0,150,9]
-- sample = [toSql (5::Double), toSql (130::Double), toSql (8::Double)]