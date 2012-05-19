module DecisionStump
    where

import Classification

import Data.List
import Data.List.Extras
import Database.HDBC
import Debug.Trace

import qualified Data.Map as Map

-- data types

data DTree a = DTree SplitPoint (DTree a) (DTree a) | DLeaf a | Fail
    deriving Show

type SplitPoint = (Int,SqlValue)

-- training

trainItr :: (Eq a) => [(a,DataPoint)] -> DTree a
trainItr [] = error "DecisionTree.train: why?"
trainItr xs = if allSameClass xs
                 then DLeaf $ fst $ head xs
                 else Fail
    where allSameClass [x]    = True
          allSameClass (x:xs) = if (fst x) == (fst $ head $ xs)
                                   then allSameClass xs
                                   else False

-- chooseSplit :: [(a,DataPoint)] -> SplitPoint
-- chooseSplit

instance Ord SqlValue where
    compare (SqlDouble a) (SqlDouble b) = compare a b
    compare (SqlString a) (SqlString b) = compare a b
    compare (SqlDouble a) (SqlString b) = LT
    compare (SqlString a) (SqlDouble b) = GT


-- splitDataDouble :: [(a,DataPoint)] -> Int -> [[(a,DataPoint)]]
splitDataDouble xs attrI = argmax  (\x -> infoGain True xs x) $ splitList
    where splitList = map (\x->[fst x,snd x]) splitPair
          splitPair = [splitAt n $ sortBy dpOrd xs | n <- [1..length xs]]
          dpOrd (l1,dp1) (l2,dp2) = compare (dp1!!attrI) (dp2!!attrI)

splitDataString :: [(a,DataPoint)] -> Int -> [[(a,DataPoint)]]
splitDataString xs attrI = Map.elems $ splitDataMap xs attrI Map.empty
    where xs_trans = map (\(a,dps)->(a, map (fromSql::SqlValue->String) dps)) xs

splitDataMap :: [(a,DataPoint)] -> Int -> Map.Map SqlValue [(a,DataPoint)] -> Map.Map SqlValue [(a,DataPoint)]
splitDataMap []     attrI m = m
splitDataMap (x:xs) attrI m = splitDataMap xs attrI $ Map.insertWith (++) k [x] m
    where k = (snd x)!!attrI

splitDataBin :: [(a,DataPoint)] -> (Int,SqlValue) -> [[(a,DataPoint)]]
splitDataBin xs (attrI,attrV) = [filter func xs, filter (not . func) xs]
    where func (l,dp) = (dp!!attrI)==attrV

infoGain :: (Eq a) => a -> [(a,DataPoint)] -> [[(a,DataPoint)]] -> Double
infoGain label xs ys = (info xp xn) - (sum $ trace (show $ map weightedInfo ys) $ map weightedInfo ys)
    where (xp,xn) = posCount xs (0,0)
          
          weightedInfo zs = (fromIntegral $ length zs)/(fromIntegral $ length xs)*(info zp zn)
              where (zp,zn) = posCount zs (0,0)
              
          posCount []     (p,n) = (p,n)
          posCount (z:zs) (p,n) = if (fst $ z)==label
                                     then posCount zs (p+1,n)
                                     else posCount zs (p,n+1)

info p' n' = -(p/(p+n)) * (lg $ p/(p+n)) - n/(p+n) * (lg $ n/(p+n))
    where p = 0.000001+fromIntegral p'
          n = 0.000001+fromIntegral n'

lg x = log x / log 2

-- classification


-- test

sqldata = [(True,[toSql (3::Double),toSql "(1::Int)"])
          ,(True,[toSql (2::Double),toSql "(1::Int)"])
          ,(False,[toSql (1::Double),toSql "(2::Int)"])
          ,(False,[toSql (4::Double),toSql "(2::Int)"])
          ,(False,[toSql (5::Double),toSql "(2::Int)"])
          ]
{-          
sqldata = [(True,[toSql (1::Int),toSql (2::Int)])
          ,(True,[toSql (1::Int),toSql (1::Int)])
          ,(False,[toSql (1::Int),toSql (2::Int)])
          ,(False,[toSql (2::Int),toSql (1::Int)])
          ,(False,[toSql (2::Int),toSql (2::Int)])
          ]-}