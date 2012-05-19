module DecisionStump
    where

import Classification

import Data.List
import Data.List.Extras
import Database.HDBC
import Debug.Trace

import qualified Data.Map as Map

-- data types

data DTree a = DTree { splitAttr :: Int
                     , splitCond :: SqlValue
                     , children :: Map.Map SqlValue (Map.Map a Int)
                     , tmpSplits :: [[(a,DataPoint)]]
                     }
    deriving Show

type SplitPoint = (Int,SqlValue)

-- training

train :: (Ord a) => [(a,DataPoint)] -> DTree a
train xs = old { tmpSplits=[]
               , children=calcChildren $ tmpSplits old
               }

-- train xs = calcChildren $ tmpSplits old 
    where old=chooseSplit xs
          
          calcChildren xs = Map.fromList $ map (\x -> ((snd $ head x)!!(splitAttr old),calcChild x)) xs
          
          calcChild :: (Ord a) => [(a,DataPoint)] -> Map.Map a Int
--           calcChild xs = Map.empty
          calcChild xs = foldl' (Map.unionWith (+) ) Map.empty $ map (\(l,dp)->Map.singleton l 1) xs
--           calcChildren :: (Ord a) => [[(a,DataPoint)]] -> Map.Map SqlValue (Map.Map a Int)

-- trainItr :: (Eq a) => [(a,DataPoint)] -> DTree a
-- trainItr [] = error "DecisionTree.train: why?"
-- trainItr xs = if allSameClass xs
--                  then DLeaf $ fst $ head xs
--                  else Fail
--     where allSameClass [x]    = True
--           allSameClass (x:xs) = if (fst x) == (fst $ head $ xs)
--                                    then allSameClass xs
--                                    else False

chooseSplit :: (Ord a) => [(a,DataPoint)] -> DTree a
chooseSplit xs = argmax (\x -> infoGain xs $ tmpSplits x) $ [splitData xs n | n <- [0..(length $ snd $ head xs)-1]]

splitData :: (Ord a) => [(a,DataPoint)] -> Int -> DTree a
splitData xs attrI = case (snd $ head xs)!!attrI of
                          SqlDouble a -> splitDataDouble xs attrI
                          SqlString a -> splitDataString xs attrI

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
                                 , splitCond = toSql ""
                                 , children = Map.empty
                                 , tmpSplits = splits
                                 }
    where splits = Map.elems $ splitDataMap xs attrI Map.empty
          xs_trans = map (\(a,dps)->(a, map (fromSql::SqlValue->String) dps)) xs

splitDataMap :: [(a,DataPoint)] -> Int -> Map.Map SqlValue [(a,DataPoint)] -> Map.Map SqlValue [(a,DataPoint)]
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

-- classification

classify :: (Ord a) => DTree a -> [SqlValue] -> a
classify dt sqlL = fst $ argmaxBy compare snd $ probList dt sqlL

probList :: (Ord a) => DTree a -> [SqlValue] -> [(a,Prob)]
probList dt sample = case attr of
                          (SqlDouble x) -> if attr<splitCond dt
                                              then predict $ snd $ head $ Map.toList $ children dt
                                              else predict $ snd $ last $ Map.toList $ children dt
                          (SqlString x) -> predict $ snd $ head $ filter (\x->(fst x)==attr) $ Map.toList $ children dt
    where attr = sample!!(splitAttr dt)
          predict m = map (\(l,x) -> (l,(fromIntegral x)/(fromIntegral $ tot))) $ Map.toList m
              where tot = sum $ map (\(l,c)->c) $ Map.toList m

-- test

sqldata = [(True,[toSql (3::Double),toSql "(1::Int)"])
          ,(True,[toSql (2::Double),toSql "(1::Int)"])
          ,(True,[toSql (1::Double),toSql "(1::Int)"])
          ,(False,[toSql (1::Double),toSql "(2::Int)"])
          ,(False,[toSql (2::Double),toSql "(1::Int)"])
          ,(False,[toSql (1::Double),toSql "(2::Int)"])
          ]
          
t=train sqldata
sample = [toSql (1.9::Double),toSql "(1::Int)"]
{-          
sqldata = [(True,[toSql (1::Int),toSql (2::Int)])
          ,(True,[toSql (1::Int),toSql (1::Int)])
          ,(False,[toSql (1::Int),toSql (2::Int)])
          ,(False,[toSql (2::Int),toSql (1::Int)])
          ,(False,[toSql (2::Int),toSql (2::Int)])
          ]-}