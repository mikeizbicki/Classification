module AI.Supervised.AdaBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import AI.Classification
import AI.Ensemble

-- training

{-

This implementation of AdaBoost most closely follows the implementation in 
http://www.deetc.isel.ipl.pt/sistemastele/docentes/AF/Textos/RT/SurveyBoosting.pdf

Unfortunately, this paper contains an error in the implementation.  In step 2.d
of the pseudocode on page 5, the indicator function should contain an ==, not a /=.
This was the source of a huge headache today.

-}

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (Ensemble model)
train trainer classifier ds = do
    logAI "AdaBoost.train"
    trainItr 50 trainer classifier wds (Ensemble [])
    where wds = map (\(l,d)->(w,(l,d))) ds
          w = 1/(fromIntegral $ length ds)

trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> [(Double,(Bool,DataPoint))] -> Ensemble model -> LogAI (Ensemble model)
trainItr itr trainer classifier wds (Ensemble adas) = do
    logAI $ "AdaBoost.trainItr: "++(show itr)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show err)
          ++ "  --  "++(show $ errorRate classifier_test)
    if {-stopCondition ||-} itr==0
       then return $ Ensemble adas
       else trainItr (itr-1) trainer classifier wdsnew adanew
       
    where
          -- important functions
          adanew = Ensemble $ (alpha,model,classifier):adas
          
          h = classifier model
          model = trainer $ sample (mkStdGen itr) 1000 wds
              
          err = (sum $ [ w * (indicator (l/=hl)) | (w,(l,d),hl) <- hL ])
              / (sum $ [ w | (w,(l,d)) <- wds ])
              
          alpha = 0.5 * (log $ (1-err)/(err+0.0001)) 
--           alpha = 0.5 * (log $ (1-err)/err)  -- this equation leads to NaN if err=0
          
          wdsnew_unnorm = [ (w*(exp $ (-alpha * (indicator (l==hl)))) ,(l,d)) 
                          | (w,(l,d),hl) <- hL
                          ]
          wdsnew_total = sum [ w | (w,dp) <- wdsnew_unnorm]
          wdsnew = {-trace ("wdsnew_total: "++show wdsnew_total)-} [ (w/wdsnew_total,dp) | (w,dp) <- wdsnew_unnorm ]
                              
          -- memoization functions
          big_H = weightedClassify adanew
          classifier_test = eval (num2bool . big_H) [dp | (w,dp) <- wds]
          hL = map (\(w,(l,d))->(w,(l,d),h d)) wds
          
          stopCondition = err>0.5
