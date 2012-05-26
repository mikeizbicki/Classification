module AI.SemiSupervised.RegBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import AI.Classification
import AI.Ensemble

-- training

{-

This implementation is based on the original RegBoost paper, "Semi-Supervised Learning via Regularized 
Boosting Working on Multiple Semi-Supervised Assumptions," TPAMI '11

We use the exponential cost functional here, as did the example in the paper.

-}

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [DataPoint] -> LogAI (Ensemble model)
train trainer classifier ls us = do
    logAI "RegBoost.train"
    trainItr 20 trainer classifier wls wus (Ensemble [])
    where 
          -- parameters
          alpha = 1
          h = 1 
          n = 1 -- supposed to be the dimension of the input space
          kappa = 1
          
          -- step 1.2
          p x = 1/( fromIntegral $ ((length ls) + (length us))) / (h)^n
              * (sum [ (similarity x d)/h | d <- (map snd ls)++us])
              
          plL = [ p d | (w,(l,d)) <- wls]
          puL = [ p d | (w,(l,d)) <- wus]
              
          p_min = minimum $ plL++puL
          p_max = maximum $ plL++puL
          
          p_bar x = ((p x) - p_min) 
                  / (p_max - p_min)
              
          betal = [ sin $ (pi/2) * (p_bar d)^kappa | (l,d) <- ls]
          betau = [ sin $ (pi/2) * (p_bar d)^kappa | d <- us]
            
          -- step 1.3 / 1.4
          model = trainer ls
          
          -- step 1.4
          wls = [ (1,ld)                     | ld <- ls]
          wus = [ (1,(classifier model d,d)) | d <- us]
          
trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> [(Double,(Bool,DataPoint))] -> [(Double,(Bool,DataPoint))] -> Ensemble model -> LogAI (Ensemble model)
trainItr itr trainer classifier wls wus (Ensemble es) = do
    logAI $ "RegBoost.trainItr: "++(show itr)
          ++ "  --  l/u="++(show $ length wls)++"/"++(show $ length wus)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show $ errorRate classifier_test)
    if itr==0
       then return $ Ensemble es
       else trainItr (itr-1) trainer classifier wls wus (Ensemble es')
       
    where 
          -- memoization functions
          big_F dp = weightedClassify (Ensemble es) dp
          classifier_test = eval (num2bool . big_F) [dp | (w,dp) <- wls++wus]

          -- algorithm functions
          
          -- step 2.2          
          step = (1/2) * (log $ 9
                         
                         )
          
          pL = 1 / (fromIntegral $ length wus)
          
          -- step 2.3
          model = trainer $ sample (mkStdGen itr) 300 (wls++wus)
          es' = (step,classifier model):es