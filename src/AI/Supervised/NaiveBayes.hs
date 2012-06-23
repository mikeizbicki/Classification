{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}

module AI.Supervised.NaiveBayes
    ( train
    , classify
    , probClassify
    , NBayes
    , defNBayes
    )
    where

import Data.List
import Data.List.Extras.Argmax
import qualified Data.Map as Map
import Debug.Trace

import AI.Classification
import AI.Ensemble

-------------------------------------------------------------------------------
-- Data types

data NBayes a = NBayes !(Map.Map a Int)
                       ![NBayesComponent a] 
    deriving Show

data NBayesComponent a = NBayesComponent !(Map.Map a NBayesDist)
    deriving Show
    
data NBayesDist = Gaussian !Double -- M
                           !Double -- S
                           !Double -- k
                | PDF !(Map.Map String Int)
    deriving Show

defNBayes = NBayes undefined undefined

-------------------------------------------------------------------------------
-- ClassifyModel definition

instance (Ord label) => ClassifyModel (NBayes label) label where
    modelName _ = "NBayes"

    train nb td ud = return $ trainNB td

    probClassify = probClassifyNB

-------------------------------------------------------------------------------
-- Training

trainNB :: (Ord a) => [(a,[DataItem])] -> NBayes a
trainNB [] = error "NaiveBayes.train: empty training set"
trainNB xs = foldl' trainItr emptyBayes xs
    where emptyBayes = NBayes Map.empty $ replicate (length $ snd $ head xs) $ NBayesComponent $ Map.empty

trainItr :: (Ord a) => NBayes a -> (a,[DataItem]) -> NBayes a
trainItr (NBayes labelCount oldBayesL) (label,sqlList) = 
    NBayes (Map.insertWith (+) label 1 labelCount) $ map (\(oldComp,op) -> trainComp op oldComp) $ zip oldBayesL (map (\x -> (label,x)) sqlList)

trainComp :: (Ord a) => (a,DataItem) -> NBayesComponent a -> NBayesComponent a
trainComp (label,sql) (NBayesComponent m) = 
    case (Map.lookup label m) of
        Nothing   -> case (initDist sql) of
                          Nothing   -> NBayesComponent m
                          Just dist -> NBayesComponent (Map.insert label dist m)
        Just dist -> NBayesComponent (Map.insert label (trainDist sql dist) m)

-- trainComp :: (Ord a) => (a,DataItem) -> NBayesComponent a -> NBayesComponent a
-- trainComp (label,sql) (NBayesComponent m) = NBayesComponent (Map.insert label newDist m)
--     where newDist = case (Map.lookup label m) of
--                          Nothing   -> initDist sql
--                          Just dist -> trainDist sql dist


trainDist :: DataItem -> NBayesDist -> NBayesDist
trainDist Missing nb = nb
trainDist sql (PDF m) = PDF $ Map.insertWith (+) (fromDataItem sql) 1 m
trainDist sql (Gaussian m s k) = Gaussian m' s' k'
    where x  = fromDataItem sql
          k' = k+1
          m' = m+(x-m)/k'
          s' = s+(x-m)*(x-m')

initDist sql = 
    case sql of
        Missing -> Nothing 
        Continuous x -> Just $ Gaussian (fromDataItem sql) 0 1
        Discrete x ->  Just $ PDF $ Map.singleton (fromDataItem sql) 1
                    
-------------------------------------------------------------------------------
-- classification

probClassifyNB (NBayes labelC compL) sqlL = 
    [ (label,prob label)
    | label <- keyList labelC
    ]
    where 
        totalCount = Map.fold (+) 0 labelC
        prob label = (probClass label)*(probDataGivenClass label)
        probClass label = (fromIntegral $ (Map.!) labelC label) / (fromIntegral totalCount)
        probDataGivenClass label = foldl' (*) 1 $ map (probComp label) $ zip sqlL compL
        keyList m = map fst $ Map.toAscList m


probComp :: (Ord a) => a -> (DataItem,NBayesComponent a) -> Prob
probComp label (sql,(NBayesComponent m)) = probDist label sql $ (Map.!) m label 

probDist :: a -> DataItem -> NBayesDist -> Prob
probDist _ Missing _ = 1
probDist label sql (PDF x) = labelMod / (fromIntegral totalCount)
    where totalCount = Map.fold (+) 0 x
          labelCount = Map.findWithDefault 0 (fromDataItem sql) x
          labelMod   = if labelCount==0
                          then 0.000001
                          else fromIntegral labelCount
                          
probDist label sql (Gaussian m s k) = 1/(sqrt $ 2*pi*var) * (exp $ -(x-m)^2/(2*var))
    where x   = fromDataItem sql
          var = s/(k-1)

----

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

trained = train defNBayes dataset_continuous [] :: LogAI (NBayes String)

testsample = [toDataItem (5::Double), toDataItem (130::Double), toDataItem (8::Double)]