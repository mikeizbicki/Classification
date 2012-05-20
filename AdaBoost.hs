module AdaBoost
    where

import Control.Monad.Writer
import Data.List
import Database.HDBC
import Debug.Trace
import System.Random

import Classification

-- data types

data AdaBoost model = AdaBoost ![(Double,[SqlValue]->Bool)]

instance (Show b) => Show (AdaBoost b) where
    show (AdaBoost xs) = show $ map (\(a,b)->(a)) xs

-- training

{-

This implementation of AdaBoost most closely follows the implementation in 
http://www.deetc.isel.ipl.pt/sistemastele/docentes/AF/Textos/RT/SurveyBoosting.pdf

Unfortunately, this paper contains an error in the implementation.  In step 2.d
of the pseudocode on page 5, the indicator function should contain an ==, not a /=.
This was the source of a huge headache today.

-}

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (AdaBoost model)
train trainer classifier ds = do
    logAI "train"
    trainItr 20 trainer classifier wds (AdaBoost [])
    where wds = map (\(l,d)->(w,(l,d))) ds
          w = 1/(fromIntegral $ length ds)

trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> [(Double,(Bool,DataPoint))] -> AdaBoost model -> LogAI (AdaBoost model)
trainItr itr trainer classifier wds (AdaBoost adas) = do
    logAI $ "trainItr: "++(show itr)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show err)
          ++ "  --  "++(show $ errorRate classifier_test)
    if itr==0
       then return $ AdaBoost adas
       else trainItr (itr-1) trainer classifier wdsnew adanew
       
    where
          -- important functions
          adanew = AdaBoost $ (alpha,h):adas
          
          h = classifier $ trainer $ sample (mkStdGen itr) 300 wds
              
          err = (sum $ [ w * (indicator (l/=hl)) | (w,(l,d),hl) <- hL ])
              / (sum $ [ w | (w,(l,d)) <- wds ])
              
          alpha = 0.5 * (log $ (1-err)/err)
          
          wdsnew_unnorm = [ (w*(exp $ (-alpha * (indicator (l==hl)))) ,(l,d)) 
                          | (w,(l,d),hl) <- hL
                          ]
          wdsnew_total = sum [ w | (w,dp) <- wdsnew_unnorm]
          wdsnew = [ (w/wdsnew_total,dp) | (w,dp) <- wdsnew_unnorm ]
                              
          -- memoization functions
          classifier_test = eval (classify adanew) [dp | (w,dp) <- wds]
          hL = map (\(w,(l,d))->(w,(l,d),h d)) wds


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
          list = map (\(alpha,c) -> alpha * (bool2num $ c dp) ) xs
