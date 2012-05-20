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
          
trainItrM n trainer classifier (AdaBoost xs) wds = do
    logAI $ "trainItrM: "++show n++
            "  --  "++(show $ eval h_m $ map (\(w,(l,d))->(l,d)) wds)++
            "  --  "++(show err_m)++
            "  --  "++(show $ errorRate $ eval (classifier h_m_model) $ map (\(w,(l,d))->(l,d)) wds_m1)
    if n==0
       then return $ AdaBoost xs
       else trainItrM (n-1) trainer classifier (AdaBoost $ (alpha_m,h_m_model,classifier):xs) wds_m1_norm
    where 
          h_m_model = trainer $ sample (mkStdGen n) 500 wds
          h_m = classifier h_m_model
          
          err_m = (sum $ map (\(w,dp)->w) $ filter (\(w,(l,d))->l==h_m d) wds) -- / (sum $ map (\(w,dp)->w) wds)
          
          alpha_m = 0.5 * log ((1-err_m)/err_m)
          
          wds_m1 :: [(Double,(Bool,DataPoint))]
          wds_m1 = map (\(w,(l,d))->(w*exp (-alpha_m * (indicator l $ h_m d)),(l,d))) wds
          wds_m1_tot = sum $ map (\(w,dp)->w) wds_m1
          wds_m1_norm = map (\(w,dp)->(w/wds_m1_tot,dp)) wds_m1
          
          indicator a b = if a==b
                             then 1
                             else -1

-- trainItrM  n trainer classifier (AdaBoost xs) wds = do
--     logAI $ "trainItrM: "++show n++
--             "  --  "++(show $ eval (classifier newmodel) $ map (\(w,(l,d))->(l,d)) wds)++
--             "  --  "++(show err)++
--             "  --  "++(show errUnw)
--     if n==0
--        then return $ AdaBoost xs
--        else trainItrM (n-1) trainer classifier newAdaBoost newwds
--     where 
--           newAdaBoost = AdaBoost $ (alpha,newmodel,classifier):xs
--           
-- --           newmodel = trainer $ map (\(d,a)->a) wds
--           newmodel = trainer $ sample rgen (2000) wds
--           rgen = mkStdGen n
--           
--           alpha = 0.5 * (log $ (1+err)/(1-err))
-- --           alpha = 0.5 * (log $ (1-err)/(err))
--           err = weightedError newmodel classifier wds
--           errUnw = errorRate $ eval (classifier newmodel) $ map (\(w,(l,d))->(l,d)) wds
--           
--           newwds = map (\(w,(l,d)) -> (w/newwds_normconst,(l,d))) newwds_unnorm
--           newwds_normconst = foldl (+) 0 $ map (\(w,(l,d))->w) newwds_unnorm
--           newwds_unnorm = map newweights wds
--           newweights (w,(l,d)) = (w',(l,d))
--               where w' = w * exp ( (-0.5) * alpha * (xor l $ AdaBoost.classify (AdaBoost xs) d))
--                     xor a b = if a==b
--                                  then 1
--                                  else -1


weightedError :: a -> BoolClassifier a-> [(Double,(Bool,DataPoint))] -> Double
weightedError model classify wds = (sum $ map errorFunc wds) / (sum $ map (\(w,dp)->w) wds)
    where errorFunc (w,(l,d)) = if l/=(classify model d)
                                   then w
                                   else 0

sample :: (Show a) => StdGen -> Int -> [(Double,a)] -> [a]
sample rgen n xs = sampleWalk 0 xs randL
    where totalWeights = sum $ map (\(w,dp)->w) xs
          randL = sort $ randList rgen n (0,totalWeights)

sampleWalk :: Double -> [(Double,a)] -> [Double] -> [a]
sampleWalk tally [] _ = []
sampleWalk tally _ [] = []
sampleWalk tally xs ys = if ((fst $ head xs)+tally)>(head ys)
                            then (snd $ head xs):(sampleWalk tally xs $ tail ys)
                            else sampleWalk (tally+(fst $ head xs)) (tail xs) ys

randList :: StdGen -> Int -> (Double,Double) -> [Double]
randList rgen 0 interval = []
randList rgen n interval = r:(randList rgen' (n-1) interval)
    where (r,rgen') = randomR interval rgen

sampleTest = map length $group $ sample (mkStdGen 20) 5500 [(1,n) | n <- [1..50]]

-- classification

classify :: (AdaBoost a) -> DataPoint -> Bool
classify (AdaBoost xs) dp = 0 < sum list
    where
          list = {-take 1 $-} map (\(alpha,m,c) -> alpha * sign (c m dp)) xs
          labelgroup (w1,l1) (w2,l2) = l1==l2
          sign l = if l
                      then 1
                      else -1