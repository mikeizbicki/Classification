module AdaBoost
    where

import Data.List
import Database.HDBC
import Debug.Trace
import System.Random

import Classification
import NaiveBayes

data AdaBoost model = AdaBoost [(Double,model,BoolClassifier model)]

instance (Show b) => Show (AdaBoost b) where
    show (AdaBoost xs) = show $ map (\(a,b,c)->(a)) xs

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> AdaBoost model
train trainer classifier ds = AdaBoost.trainItr 10 trainer classifier (AdaBoost []) $ zip ws ds
    where 
          w = 1/(fromIntegral $ length ds)
          ws = replicate (length ds) w

-- trainItr n trainer classifier (AdaBoost xs) wds = err
trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> AdaBoost model -> [(Double,(Bool,DataPoint))] -> AdaBoost model
trainItr n trainer classifier (AdaBoost xs) wds = if n==0
                                                     then (AdaBoost xs)
                                                     else trace (show (take 10 $ map (\(w,(l,d))->w) newwds)++"\n\n\n") $
                                                          AdaBoost.trainItr (n-1)
                                                                            trainer
                                                                            classifier
                                                                            newAdaBoost
                                                                            newwds
    where 
          rgen = mkStdGen n
          newmodel = trainer $ sample rgen (50) wds
--           newmodel = trainer $ map (\(w,(l,d))->(l,d)) filterwds
          mean = (foldl (+) 0 $ map (\(w,(l,d))->w) wds) / (fromIntegral $ length wds)
--           filterwds = filter (\(w,(l,d)) -> w>mean) wds
--           filterwds = filter (\(w,(l,d)) -> (l== AdaBoost.classify (AdaBoost xs) d)) wds
          filterwds = sample rgen 100 wds
          
          newAdaBoost = AdaBoost $ (alpha,newmodel,classifier):xs
          alpha = 0.5 * (log $ (1-err)/err)
--           err = weightedError newmodel classifier wds
          err = errorRate $ eval (classifier newmodel) $ map (\(w,(l,d))->(l,d)) wds
          
          newwds = --trace ("----"++(show newwds_normconst)++"----") $ 
                   map (\(w,(l,d)) -> (w/newwds_normconst,(l,d))) newwds_unnorm
          newwds_normconst = foldl (+) 0 $ map (\(w,(l,d))->w) newwds_unnorm
          newwds_unnorm = map newweights wds
          newweights (w,(l,d)) = (w',(l,d))
              where w' = w * exp ( (-0.5) * alpha * (xor l $ AdaBoost.classify (AdaBoost xs) d))
                    xor a b = if a==b
                                 then 1
                                 else -1

sample :: StdGen -> Int -> [(Double,a)] -> [a]
sample rgen n [] = []
sample rgen 0 xs = []
sample rgen n xs = 
                    if r < (fst $ head xs)
                      then (snd $ head xs):(sample rgen' (n-1) $ tail xs)
                      else (sample rgen' n $ tail xs)
    where (r,rgen') = randomR (0,total/fromIntegral n) rgen
          total = sum $ map (\(w,b)->w) xs

-- train :: BoolClassifier -> [(Bool,DataPoint)] -> AdaBoost
-- train classifier ds = AdaBoost.trainItr 2 classifier (AdaBoost []) $ zip ws ds
--     where 
--           w = 1/(fromIntegral $ length ds)
--           ws = replicate (length ds) w
--           
{-trainItr :: Int -> BoolClassifier -> AdaBoost -> [(Double,(Bool,DataPoint))] -> AdaBoost
trainItr n classifier (AdaBoost xs) wds = if n==0
                                             then AdaBoost xs
                                             else AdaBoost.trainItr (n-1) classifier (AdaBoost ((alpha,classifier):xs)) wds'
    where
          wds'_unnorm = map (\(w,(l,d)) -> (trace ((show l) ++ (show $ classifier d)) $ w*(exp $ alpha * (xor l $ classifier d)),(l,d))) wds
          wds' = trace (show wds'_unnorm) $ map (\(w,(l,d))->(w/sumwds,(l,d))) wds'_unnorm
          sumwds=(foldl (+) 0 $ map (\(w,(l,d))->w) wds'_unnorm)
          xor a b = if a==b
                       then 1
                       else -1
          alpha = (0.5) * log ( (1-err)/err)
          err = weightedError classifier wds
-}
weightedError :: a -> BoolClassifier a-> [(Double,(Bool,DataPoint))] -> Double
weightedError model classify ds = foldl (+) 0 $ map errorFunc ds
    where errorFunc (w,(l,d)) = if l/=(classify model d)
                                   then {-trace (show w)-} w
                                   else 0
-- 
-- classify :: AdaBoost Bool -> DataPoint -> Bool
-- classify :: Classifier Bool (AdaBoost Bool)
-- classify (AdaBoost xs) dp = 0 < (sum $ map (\(w,l)->{-w*-}sign l) $ map (\(w,m,c)->(w,c m dp)) xs)
--classify (AdaBoost xs) dp = 0 < (sum $ map (\(w,l)->w*sign l) $ map (\(w,c) -> (w,c dp)) xs)

   
-- classify :: AdaBoost Bool -> DataPoint -> m
classify (AdaBoost xs) dp = 0< ({-trace (show list) $-} sum list )
    where
          list = map (\(alpha,m,c) -> alpha * sign (c m dp)) xs
          labelgroup (w1,l1) (w2,l2) = l1==l2
          sign l = if l
                      then 1
                      else -1
-- -- use groupBy function
-- 
-- boost         = AdaBoost.train (toBinaryClassifier "male" $ NaiveBayes.classify $ NaiveBayes.train sqldata)
--                                (toBinaryData "male" sqldata)