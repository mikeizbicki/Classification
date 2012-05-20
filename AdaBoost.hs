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

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (AdaBoost model)
train trainer classifier ds = do
    logAI "train"
    trainItr 20 trainer classifier wds (AdaBoost [])
    where wds = map (\(l,d)->(w,(l,d))) ds
          w = 1/(fromIntegral $ length ds)

trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> [(Double,(Bool,DataPoint))] -> AdaBoost model -> LogAI (AdaBoost model)
trainItr itr trainer classifier wds (AdaBoost adas) = do
    logAI $ "trainItr: "++(show itr)
          ++ "  --  "++(show $ eval (classify adanew) [dp | (w,dp) <- wds])
          ++ "  --  "++(show err)
          ++ "  --  "++(show $ errorRate $ eval (classify adanew) [dp | (w,dp) <- wds])
    if itr==0
       then return $ AdaBoost adas
--        else return $ AdaBoost adas
       else trainItr (itr-1) trainer classifier wdsnew adanew
       
    where
          adanew = AdaBoost $ (alpha,h,classifier):adas
          
          h = trainer $ sample (mkStdGen itr) 300 wds
          
          err = (sum $ [ w * (indicator (l/=classifier h d)) | (w,(l,d)) <- wds ])
              / (sum $ [ w | (w,(l,d)) <- wds ])
              
          alpha = 0.5 * (log $ (1-err)/err)
          
          wdsnew_unnorm = [ (w*(exp $ (-alpha * (indicator (l==classifier h d)))) ,(l,d)) 
                          | (w,(l,d)) <- wds
                          ]
          wdsnew_total = sum [ w | (w,dp) <- wdsnew_unnorm]
          wdsnew = [ (w/wdsnew_total,dp) | (w,dp) <- wdsnew_unnorm ]
          
          indicator bool = if bool
                              then 1
                              else 0
    

-- trainM :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (AdaBoost model)
-- trainM trainer classifier ds = do 
--     logAI "trainM"
--     AdaBoost.trainItrM 50 trainer classifier (AdaBoost []) $ zip ws ds
--     where 
--           w = 1/(fromIntegral $ length ds)
--           ws = replicate (length ds) w
-- 
-- trainItrM :: Int 
--           -> Trainer Bool model 
--           -> BoolClassifier model 
--           -> AdaBoost model 
--           -> [(Double,(Bool,DataPoint))] 
--           -> LogAI (AdaBoost model)
--           
-- trainItrM n trainer classifier (AdaBoost xs) wds = do
--     logAI $ "trainItrM: "++show n++
--             "  --  "++(show $ eval h_m $ map (\(w,(l,d))->(l,d)) wds)++
--             "  --  "++(show err_m)++
--             "  --  "++(show $ errorRate $ eval (classifier h_m_model) $ map (\(w,(l,d))->(l,d)) wds_m1)
--     if n==0
--        then return $ AdaBoost xs
--        else trainItrM (n-1) trainer classifier (AdaBoost $ (alpha_m,h_m_model,classifier):xs) wds_m1_norm
--     where 
--           h_m_model = trainer $ sample (mkStdGen n) 500 wds
--           h_m = classifier h_m_model
--           
--           err_m = (sum $ map (\(w,dp)->w) $ filter (\(w,(l,d))->l==h_m d) wds) -- / (sum $ map (\(w,dp)->w) wds)
--           
-- --           alpha_m = 0.5 * log ((1-err_m)/err_m)
--           alpha_m = 0.5 * (log $ (1+err_m)/(1-err_m))
--           
--           wds_m1 :: [(Double,(Bool,DataPoint))]
--           wds_m1 = map (\(w,(l,d))->(w*exp (-alpha_m * (bool2num l) *(bool2num $ h_m d)),(l,d))) wds
--           wds_m1_tot = sum $ map (\(w,dp)->w) wds_m1
--           wds_m1_norm = map (\(w,dp)->(w/wds_m1_tot,dp)) wds_m1
--           
--           indicator a b = if a==b
--                              then 1
--                              else 0

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
--           newmodel = trainer $ sample rgen (200) wds
--           rgen = mkStdGen n
--           
-- --           alpha = 0.5 * (log $ (1+err)/(1-err))
--           alpha = 0.5 * (log $ (1-err)/(err))
--           err = weightedError newmodel classifier wds
--           errUnw = errorRate $ eval (classifier newmodel) $ map (\(w,(l,d))->(l,d)) wds
--           
--           newwds = map (\(w,(l,d)) -> (w/newwds_normconst,(l,d))) newwds_unnorm
--           newwds_normconst = foldl (+) 0 $ map (\(w,(l,d))->w) newwds_unnorm
--           newwds_unnorm = map newweights wds
--           newweights (w,(l,d)) = (w',(l,d))
--               where w' = w * exp ( (-0.5) * alpha * (bool2num l) * (bool2num $ classify newAdaBoost d))


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
classify (AdaBoost xs) dp = num2bool $ sum list
    where
          list = map (\(alpha,m,c) -> alpha * (bool2num $ c m dp) ) xs
