{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FlexibleContexts #-}

{-

Decision stumps are decision trees with only a single level.  They were introduced by the paper:

Iba, Wayne; and Langley, Pat (1992); Induction of One-Level Decision Trees, in ML92: Proceedings of the Ninth International Conference on Machine Learning, Aberdeen, Scotland, 1–3 July 1992, San Francisco, CA: Morgan Kaufmann, pp. 233–240

which can be downloaded at http://lyonesse.stanford.edu/~langley/papers/stump.ml92.pdf
   
-}

module AI.Supervised.DecisionStump
{-    ( train
    , classify
    , probClassify
    , DStump
    )-}
    where

import AI.Classification
import AI.Ensemble

import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- data types

data DStump labelType = DStump 
    { splitAttr :: Int
    , splitCond :: DataItem
    , children :: Map.Map DataItem (Map.Map labelType Int)
    , tmpSplits :: [TrainingData labelType]
    }
    deriving Show

type SplitPoint = (Int,DataItem)

-------------------------------------------------------------------------------
-- training

train :: (Ord labelType) => TrainingData labelType -> DStump labelType
train xs = --trace "train" $ 
    old { tmpSplits=[]
        , children=calcChildren $ tmpSplits old
        }
    where 
        old=chooseSplit xs
          
        calcChildren xs = Map.fromList $ map (\x -> ((snd $ head x)!!(splitAttr old),calcChild x)) xs
        
        calcChild :: (Ord labelType) => TrainingData labelType -> Map.Map labelType Int
        calcChild xs = foldl' (Map.unionWith (+) ) Map.empty $ map (\(l,dp)->Map.singleton l 1) xs

chooseSplit :: (Ord labelType) => TrainingData labelType -> DStump labelType
chooseSplit ([]) = error "chooseSplit only 0 arg; you must have >= 2 datapoints in your training data"
chooseSplit (x:[]) = error "chooseSplit only 1 arg; you must have >= 2 datapoints in your training data"
chooseSplit xs = argmax (\x -> infoGain xs $ tmpSplits x) $ [splitData xs n | n <- [0..(length $ snd $ head xs)-1]]

splitData :: (Ord labelType) => TrainingData labelType -> Int -> DStump labelType
splitData td attrI = 
    case getAttrType td attrI of
        Continuous a -> splitDataDouble td attrI
        Discrete a -> splitDataString td attrI

splitDataDouble :: (Ord labelType) => TrainingData labelType -> Int -> DStump labelType
splitDataDouble xs attrI = DStump { splitAttr = attrI
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

splitDataString :: TrainingData labelType -> Int -> DStump labelType
splitDataString xs attrI = DStump { splitAttr = attrI
                                 , splitCond = Discrete ""
                                 , children = Map.empty
                                 , tmpSplits = splits
                                 }
    where splits = Map.elems $ splitDataMap xs attrI Map.empty
          xs_trans = map (\(a,dps)->(a, map (fromDataItem::DataItem->String) dps)) xs

splitDataMap :: TrainingData labelType -> Int -> Map.Map DataItem (TrainingData labelType) -> Map.Map DataItem (TrainingData labelType)
splitDataMap []     attrI m = m
splitDataMap (x:xs) attrI m = splitDataMap xs attrI $ Map.insertWith (++) k [x] m
    where k = (snd x)!!attrI

-------------------------
-- information gain utils

infoGain :: (Ord labelType) => TrainingData labelType -> [TrainingData labelType] -> Double
infoGain xs ys = (info xs) - (sum $ map weightedInfo ys)
    where weightedInfo zs = (fromIntegral $ length zs)/(fromIntegral $ length xs)*(info zs)
              
info :: (Ord labelType) => TrainingData labelType -> Double
info xs = infoInt $ Map.elems $ labelCount xs Map.empty

labelCount :: (Ord labelType) => TrainingData labelType -> Map.Map labelType Int -> Map.Map labelType Int
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

instance (Ord label, Show label) => ClassifyModel (DStump label) label where
    probClassify dt sample = -- trace ("classify: dt="++show dt++"; sample="++show sample) $ 
        labelCount2labelProb $ fetchLabelCount dt attr
        where 
            attr = sample!!(splitAttr dt)

labelCount2labelProb :: Map.Map labelType Int -> [(labelType,Prob)]
labelCount2labelProb m = map (\(l,x) -> (l,(fromIntegral x)/(fromIntegral $ tot))) $ Map.toList m
    where tot = sum $ map (\(l,c)->c) $ Map.toList m

fetchLabelCount :: (Ord labelType) => DStump labelType -> DataItem -> Map.Map labelType Int
fetchLabelCount dt Missing = defaultLabelCount dt
fetchLabelCount dt (Continuous attr) = 
    if attr<(fromDataItem $ splitCond dt :: Double)
        then snd $ head $ Map.toList $ children dt
        else snd $ last $ Map.toList $ children dt
fetchLabelCount dt (Discrete attr) = 
    case Map.lookup (Discrete attr) $ children dt of
         Nothing -> defaultLabelCount dt
         Just x -> x

defaultLabelCount :: (Ord labelType) => DStump labelType -> Map.Map labelType Int
defaultLabelCount dt = 
    Map.foldl'
        (Map.unionWith (+))
        (Map.empty)
        (children dt)

-- test

sqldata = [(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ]
          
t=train sqldata :: DStump Bool
example = [toDataItem (2::Double),toDataItem "(1::Int)"]

dt=DStump 
    { splitAttr = 3
    , splitCond = Discrete ""
    , children = Map.fromList 
        [(Discrete "A40", Map.fromList 
            [ (False,95)
            , (True,69)
            ])
        ,(Discrete "A41", Map.fromList 
            [(False,20)
            ,(True,7)])
        ,(Discrete "A410",Map.fromList 
            [(True,6)])
        ,(Discrete "A42",Map.fromList 
            [(False,29)
            ,(True,41)])
        ,(Discrete "A43",Map.fromList 
            [(False,45)
            ,(True,86)])
        ,(Discrete "A44",Map.fromList 
            [(True,1)])
        ,(Discrete "A45",Map.fromList 
            [(False,13)
            ,(True,8)])
        ,(Discrete "A46",Map.fromList 
            [(False,21)
            ,(True,1)])
        ,(Discrete "A49",Map.fromList 
            [(False,34)
            ,(True,24)])
        ]
    , tmpSplits = []
    }
    
samplebreak=[Discrete "A12",Continuous 6.0,Discrete "A34",Discrete "A48",Continuous 932.0,Discrete "A65",Discrete "A74",Continuous 1.0,Discrete "A92",Discrete "A101",Continuous 3.0,Discrete "A122",Continuous 39.0,Discrete "A143",Discrete "A152",Continuous 2.0,Discrete "A172",Continuous 1.0,Discrete "A191",Discrete "A201"]