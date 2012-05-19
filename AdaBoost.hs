module AdaBoost
    where

import Control.Monad.Writer
import Data.List
import Database.HDBC
import Debug.Trace
import System.Random

import Classification

-- data types

data AdaBoost model = AdaBoost ![(Double,model,BoolClassifier model)]

instance (Show b) => Show (AdaBoost b) where
    show (AdaBoost xs) = show $ map (\(a,b,c)->(a)) xs

-- training

trainM :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (AdaBoost model)
trainM trainer classifier ds = do 
    logAI "trainM"
    AdaBoost.trainItrM 50 trainer classifier (AdaBoost []) $ zip ws ds
    where 
          w = 1/(fromIntegral $ length ds)
          ws = replicate (length ds) w

trainItrM :: Int 
          -> Trainer Bool model 
          -> BoolClassifier model 
          -> AdaBoost model 
          -> [(Double,(Bool,DataPoint))] 
          -> LogAI (AdaBoost model)
          
trainItrM  n trainer classifier (AdaBoost xs) wds = do
    logAI $ "trainItrM: "++show n++
            "  --  "++(show $ eval (classifier newmodel) $ map (\(w,(l,d))->(l,d)) wds)++
            "  --  "++(show err)++
            "  --  "++(show errUnw)
    if n==0
       then return $ AdaBoost xs
       else trainItrM (n-1) trainer classifier newAdaBoost newwds
    where 
          newAdaBoost = AdaBoost $ (alpha,newmodel,classifier):xs
          
--           newmodel = trainer $ map (\(d,a)->a) wds
          newmodel = trainer $ sample rgen (200) wds
          rgen = mkStdGen n
          
          alpha = 0.5 * (log $ (1-err)/(1+err))
--           alpha = 0.5 * (log $ (1-err)/(err))
          err = weightedError newmodel classifier wds
          errUnw = errorRate $ eval (classifier newmodel) $ map (\(w,(l,d))->(l,d)) wds
          
          newwds = map (\(w,(l,d)) -> (w/newwds_normconst,(l,d))) newwds_unnorm
          newwds_normconst = foldl (+) 0 $ map (\(w,(l,d))->w) newwds_unnorm
          newwds_unnorm = map newweights wds
          newweights (w,(l,d)) = (w',(l,d))
              where w' = w * exp ( (-0.5) * alpha * (xor l $ AdaBoost.classify (AdaBoost xs) d))
                    xor a b = if a==b
                                 then 1
                                 else -1


weightedError :: a -> BoolClassifier a-> [(Double,(Bool,DataPoint))] -> Double
weightedError model classify ds = foldl (+) 0 $ map errorFunc ds
    where errorFunc (w,(l,d)) = if l/=(classify model d)
                                   then w
                                   else 0

sample :: (Show a) => StdGen -> Int -> [(Double,a)] -> [a]
sample rgen n [] = []
sample rgen 0 xs = []
sample rgen n xs = if r < (fst $ head xs)
                      then (snd $ head xs):(sample rgen' (n-1) $ (((fst $ head xs)-r,snd $ head xs)):(tail xs))
                      else (sample rgen' n $ tail xs)
    where (r,rgen') = randomR (0,total/fromIntegral n) rgen
          total = sum $ map (\(w,b)->w) xs

-- classification

classify :: (AdaBoost a) -> DataPoint -> Bool
classify (AdaBoost xs) dp = 0 < sum list
    where
          list = {-take 1 $-} map (\(alpha,m,c) -> alpha * sign (c m dp)) xs
          labelgroup (w1,l1) (w2,l2) = l1==l2
          sign l = if l
                      then 1
                      else -1