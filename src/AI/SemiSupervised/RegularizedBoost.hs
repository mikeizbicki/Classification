module AI.SemiSupervised.RegularizedBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import AI.Classification
import AI.Ensemble

-- training

{-

This implementation is based on the original RegularizedBoost paper, "Regularized Boost for Semi-Supervised Learning"

A later paper extends this algorithm into the RegBoost algorithm.  This first algorithm, however, is much simpler.

---

-}

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [DataPoint] -> LogAI (Ensemble model)
train trainer classifier ls us = do
    logAI "RegularizedBoost.train"
    trainItr 20 trainer classifier wls wus (Ensemble [])
    where beta = 0.9
          wl = beta/(fromIntegral $ (length ls))
          wu = (1-beta)/(fromIntegral $ (length us))
          
          wls = [(wl,ld) | ld <- ls]
          wus = [(wu,(f d,d)) | d <- us]
          
          f :: DataPoint -> Bool
          f d = classifier model d
          model = trainer ls
          
trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> [(Double,(Bool,DataPoint))] -> [(Double,(Bool,DataPoint))] -> Ensemble model -> LogAI (Ensemble model)
trainItr itr trainer classifier wls wus (Ensemble es) = do
    logAI $ "RegularizedBoost.trainItr: "++(show itr)
          ++ "  --  l/u="++(show $ length wls)++"/"++(show $ length wus)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show err)
          ++ "  --  "++(show $ errorRate classifier_test)
--           ++ "  --  "++(show [ (w,l,num2bool $ big_F d) | (w,(l,d)) <- wls])
    if itr==0
       then return $ Ensemble es
       else trainItr (itr-1) trainer classifier wls' wus' (Ensemble es')
       
    where 
          -- parameters
          alpha = 1
          beta = 1/2
          
          cost  z = exp(-z)
          cost' z = -exp(-z)
          cost_tilde z = (cost z) - 1
          
          -- same equations as ASSEMBLE
          model = trainer $ sample (mkStdGen itr) 100 (wls++wus)
          f_itr = classifier model
          weight = 0.5 * (log $ (1-err)/err)
          es'  = (weight,f_itr):es

          -- from equation 9, as modified in paragraph at the end of section 2.2
--           err = 2*misclassification_err+2*classincopbatability_err-1
          err = misclassification_err -- +classincompatability_err
--           err = classincompatability_err
          
          misclassification_err = sum [ w*(indicator $ l/=f_itr d) | (w,(l,d)) <- wls]
                                + sum [ w*(indicator $ l/=f_itr d) | (w,(l,d)) <- wus]
              
--           classincompatability_err = sum [ -beta*(r d)/denom | (w,(l,d)) <- wls++wus]
--           denom = sum [alpha*(cost' $ (bool2num l)*(big_F d))-beta*(r d) | (w,(l,d)) <- wls++wus]
              
          -- from equation 6          
--           r d' = sum [ (similarity d' d)*(cost_tilde $ 2*(indicator $ d'/=d)) | (w,(l,d)) <- wls++wus]
          
          -- from equation 8
--           wus'_unnorm = [ let l'=classify (Ensemble es') d 
--                               w'=alpha*(cost' $ (bool2num l')*(big_F d)) -- - beta*(r d)
--                           in (w',(l',d)) | (w,(l,d)) <- wus]
-- 
--           wls'_unnorm = [ let w'=alpha*(cost' $ (bool2num  l)*(big_F d)) -- - beta*(r d)
--                           in (w',(l,d)) | (w,(l,d)) <- wls]

          wus'_unnorm = [ let l'=classify (Ensemble es') d in
                          (w*(exp $ -weight * (indicator $ l'==f_itr d)),(l',d)) | (w,(l,d)) <- wus]
          wls'_unnorm = [ (w*(exp $ -weight * (indicator $ l ==f_itr d)),(l, d)) | (w,(l,d)) <- wls]

       
          w_tot = sum [ w | (w,ld) <- wus'_unnorm]
                + sum [ w | (w,ld) <- wls'_unnorm]
          
          wus' = [ (w/w_tot,ld) | (w,ld) <- wus'_unnorm]
          wls' = [ (w/w_tot,ld) | (w,ld) <- wls'_unnorm]
          
          -- memoization functions
          big_F = weightedClassify (Ensemble es)
          classifier_test = eval (num2bool . big_F) [dp | (w,dp) <- wls++wus]
             