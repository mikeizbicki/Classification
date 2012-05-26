module AdaBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import Classification
import Ensemble

-- training

{-

MarginBoost is very similar to AdaBoost, but seems to converge a little faster?

This implementation is based on Algorithm 12.4 in "Functional Gradient Techniques for Conbining Hypotheses"
by Mason, Baxter, Bartlett, and Frean

-}

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (Ensemble model)
train trainer classifier ds = do
    logAI "AdaBoost.train"
    trainItr 2000 trainer classifier wds (Ensemble [])
    where wds = map (\(l,d)->(w,(l,d))) ds
          w = 1/(fromIntegral $ length ds)

trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> [(Double,(Bool,DataPoint))] -> Ensemble model -> LogAI (Ensemble model)
trainItr itr trainer classifier wds (Ensemble adas) = do
    logAI $ "AdaBoost.trainItr: "++(show itr)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show err)
          ++ "  --  "++(show $ errorRate classifier_test)
    if itr==0
       then return $ Ensemble adas
       else trainItr (itr-1) trainer classifier wdsnew adanew
       
    where
          -- parameters
          cost  z =  exp(-z)
          cost' z = -exp(-z)
          
          -- important functions
          adanew = Ensemble $ (alpha,h):adas
          
          h = classifier $ trainer $ sample (mkStdGen itr) 300 wds
              
          err = (sum $ [ w * (indicator (l/=hl)) | (w,(l,d),hl) <- hL ])
--               / (sum $ [ w | (w,(l,d)) <- wds ])
              
          alpha = 0.5 * (log $ (1-err)/err)
          
          wdsnew_unnorm = [ (cost' $ (bool2num l) * (big_H d),(l,d)) 
                          | (w,(l,d),hl) <- hL
                          ]
{-          wdsnew_unnorm = [ (w*(exp $ (-alpha * (indicator (l==hl)))) ,(l,d)) 
                          | (w,(l,d),hl) <- hL
                          ]-}
          wdsnew_total = sum [ w | (w,dp) <- wdsnew_unnorm]
          wdsnew = [ (w/wdsnew_total,dp) | (w,dp) <- wdsnew_unnorm ]
                              
          -- memoization functions
          big_H = weightedClassify adanew
          classifier_test = eval (num2bool . big_H) [dp | (w,dp) <- wds]
          hL = map (\(w,(l,d))->(w,(l,d),h d)) wds
