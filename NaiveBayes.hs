module NaiveBayes
    where

import Data.List.Extras.Argmax
import qualified Data.Map as Map
import Database.HDBC
import Debug.Trace

import Classification

-- Data types

data NBayes a = NBayes (Map.Map a Int)
                     [NBayesComponent a] 
    deriving Show

data NBayesComponent a = NBayesComponent (Map.Map a NBayesDist)
    deriving Show
    
data NBayesDist = Gaussian Double -- M
                           Double -- S
                           Double -- k
                | PDF (Map.Map String Int)
--                 | PDF (Map.Map SqlValue Int)
    deriving Show

-- Training

train :: (Ord a) => [(a,[SqlValue])] -> NBayes a
train xs = foldl trainItr emptyBayes xs
    where emptyBayes = NBayes Map.empty $ replicate (length $ snd $ head xs) $ NBayesComponent $ Map.empty

trainItr :: (Ord a) => NBayes a -> (a,[SqlValue]) -> NBayes a
trainItr (NBayes labelCount oldBayesL) (label,sqlList) = NBayes (Map.insertWith (+) label 1 labelCount) $ map (\(oldComp,op) -> trainComp op oldComp) $ zip oldBayesL (map (\x -> (label,x)) sqlList)

trainComp :: (Ord a) => (a,SqlValue) -> NBayesComponent a -> NBayesComponent a
trainComp (label,sql) (NBayesComponent m) = NBayesComponent (Map.insert label newDist m)
    where newDist = case (Map.lookup label m) of
                         Nothing   -> initDist sql
                         Just dist -> trainDist sql dist

trainDist :: SqlValue -> NBayesDist -> NBayesDist
trainDist sql (PDF m) = PDF $ Map.insertWith (+) (fromSql sql) 1 m
trainDist sql (Gaussian m s k) = Gaussian m' s' k'
    where x  = fromSql sql
          k' = k+1
          m' = m+(x-m)/k'
          s' = s+(x-m)*(x-m')

initDist sql = case safeFromSql sql of
                    Right x -> seq (x::Double) $ Gaussian (fromSql sql) 0 1
                    Left _  -> -- PDF Map.singleton sql 1
                               case safeFromSql sql of
                                    Right x -> seq (x::String) $ PDF $ Map.singleton (fromSql sql) 1
                                    Left _  -> error "initDist: sqlVar was neither double nor string"

-- classification

classify :: (Ord a) => NBayes a -> [SqlValue] -> a
classify nb sqlL = fst $ argmaxBy compare snd $ probList nb sqlL

probList :: (Ord a) => NBayes a -> [SqlValue] -> [(a,Prob)]
probList (NBayes labelC compL) sqlL = [ (label,prob label)
                                      | label <- keyList labelC
                                      ]
    where totalCount = Map.fold (+) 0 labelC
          prob label = (probClass label)*(probDataGivenClass label)
          probClass label = (fromIntegral $ (Map.!) labelC label) / (fromIntegral totalCount)
          probDataGivenClass label = foldl (*) 1 $ map (probComp label) $ zip sqlL compL
          keyList m = map fst $ Map.toAscList m

probComp :: (Ord a) => a -> (SqlValue,NBayesComponent a) -> Prob
probComp label (sql,(NBayesComponent m)) = probDist label sql $ (Map.!) m label 

probDist :: a -> SqlValue -> NBayesDist -> Prob
probDist label sql (PDF x) = labelMod / (fromIntegral totalCount)
    where totalCount = Map.fold (+) 0 x
          labelCount = Map.findWithDefault 0 (fromSql sql) x
          labelMod   = if labelCount==0
                          then 0.00000000000000000000001
                          else fromIntegral labelCount
probDist label sql (Gaussian m s k) = 1/(sqrt $ 2*pi*var) * (exp $ -(x-m)^2/(2*var))
    where x   = fromSql sql
          var = s/(k-1)

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

sqldata = map (\(label,xs) -> (label,map (toSql::Double->SqlValue) xs)) dataset
bdataset = toBinaryData "male" sqldata

trained = train sqldata

-- sample = [toSql (5::Double), toSql (130::Double), toSql (8::Double)]