{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FlexibleContexts #-}

module AI.Supervised.DecisionStump
    ( train
    , classify
    , probClassify
    )
    where

import AI.Classification
import AI.Ensemble

import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- data types

data DTree a = DTree { splitAttr :: Int
                     , splitCond :: DataItem
                     , children :: Map.Map DataItem (Map.Map a Int)
                     , tmpSplits :: [[(a,DataPoint)]]
                     }
    deriving Show

type SplitPoint = (Int,DataItem)

-------------------------------------------------------------------------------
-- training

train :: (Ord a) => [(a,DataPoint)] -> DTree a
train xs = old { tmpSplits=[]
               , children=calcChildren $ tmpSplits old
               }
    where 
        old=chooseSplit xs
          
        calcChildren xs = Map.fromList $ map (\x -> ((snd $ head x)!!(splitAttr old),calcChild x)) xs
        
        calcChild :: (Ord a) => [(a,DataPoint)] -> Map.Map a Int
        calcChild xs = foldl' (Map.unionWith (+) ) Map.empty $ map (\(l,dp)->Map.singleton l 1) xs

chooseSplit :: (Ord a) => [(a,DataPoint)] -> DTree a
chooseSplit xs = argmax (\x -> infoGain xs $ tmpSplits x) $ [splitData xs n | n <- [0..(length $ snd $ head xs)-1]]

splitData :: (Ord a) => [(a,DataPoint)] -> Int -> DTree a
splitData xs attrI = 
    case (snd $ head xs)!!attrI of
        Continuous a -> splitDataDouble xs attrI
        Discrete a -> splitDataString xs attrI

splitDataDouble :: (Ord a) => [(a,DataPoint)] -> Int -> DTree a
splitDataDouble xs attrI = DTree { splitAttr = attrI
                                 , splitCond = (snd $ head $ splits!!1) !!attrI
                                 , children = Map.empty
                                 , tmpSplits = splits
                                 }
    where splits = argmax (\x -> infoGain xs x) $ splitList
          splitList = map (\x->[concat $ fst x, concat $ snd x]) splitPair
          splitPair = [splitAt n grouped | n <- [1..(length grouped)-1]]
          grouped = groupBy sameAttrI $ sortBy dpOrd xs
          sameAttrI (l1,dp1) (l2,dp2) = (dp1!!attrI)==(dp2!!attrI)
          dpOrd (l1,dp1) (l2,dp2) = compare (dp1!!attrI) (dp2!!attrI)

splitDataString :: [(a,DataPoint)] -> Int -> DTree a
splitDataString xs attrI = DTree { splitAttr = attrI
                                 , splitCond = Discrete ""
                                 , children = Map.empty
                                 , tmpSplits = splits
                                 }
    where splits = Map.elems $ splitDataMap xs attrI Map.empty
          xs_trans = map (\(a,dps)->(a, map (fromDataItem::DataItem->String) dps)) xs

splitDataMap :: [(a,DataPoint)] -> Int -> Map.Map DataItem [(a,DataPoint)] -> Map.Map DataItem [(a,DataPoint)]
splitDataMap []     attrI m = m
splitDataMap (x:xs) attrI m = splitDataMap xs attrI $ Map.insertWith (++) k [x] m
    where k = (snd x)!!attrI

-- generic

infoGain :: (Ord a) => [(a,DataPoint)] -> [[(a,DataPoint)]] -> Double
infoGain xs ys = (info xs) - (sum $ map weightedInfo ys)
    where weightedInfo zs = (fromIntegral $ length zs)/(fromIntegral $ length xs)*(info zs)
              
info :: (Ord a) => [(a,DataPoint)] -> Double
info xs = infoInt $ Map.elems $ labelCount xs Map.empty

labelCount :: (Ord a) => [(a,DataPoint)] -> Map.Map a Int -> Map.Map a Int
labelCount []     m = m
labelCount (x:xs) m = labelCount xs $ Map.insertWith (+) (fst x) 1 m

infoInt :: [Int] -> Double
infoInt xs = sum $ map (\x -> -(x/tot)*(lg $ x/tot)) xs'
    where xs' = map (\x -> 0.000001+fromIntegral x) xs
          tot = sum xs'

lg :: Double -> Double -- base 2 logarithm
lg x = log x / log 2

-------------------------------------------------------------------------------
-- classification

instance (Ord label) => ClassifyModel (DTree label) label where
    probClassify dt sample = 
        case attr of
            (Continuous x) -> 
                if attr<splitCond dt
                    then predict $ snd $ head $ Map.toList $ children dt
                    else predict $ snd $ last $ Map.toList $ children dt
            (Discrete x) -> 
                predict $ snd $ head $ filter (\x->(fst x)==attr) $ Map.toList $ children dt
        where 
            attr = sample!!(splitAttr dt)
            predict m = map (\(l,x) -> (l,(fromIntegral x)/(fromIntegral $ tot))) $ Map.toList m
                where tot = sum $ map (\(l,c)->c) $ Map.toList m

-- test

sqldata = [(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ]
          
t=train sqldata :: DTree Bool
example = [toDataItem (2::Double),toDataItem "(1::Int)"]