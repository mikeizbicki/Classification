module AI.SemiSupervised.ASSEMBLE
    where

import Database.HDBC
import Debug.Trace
import System.Random

import AI.Classification
import AI.Ensemble

-- training

{-

This implementation is based on the original ASSEMBLE paper, "Exploiting Unlabeled Data 
in Ensemble Methods," Algorithm 3.1

-}

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [DataPoint] -> LogAI (Ensemble model)
train trainer classifier ls us = do
    logAI "ASSEMBLE.train"
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
    logAI $ "ASSEMBLE.trainItr: "++(show itr)
          ++ "  --  l/u="++(show $ length wls)++"/"++(show $ length wus)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show err)
          ++ "  --  "++(show $ errorRate classifier_test)
    if itr==0
       then return $ Ensemble es
       else trainItr (itr-1) trainer classifier wls' wus' (Ensemble es')
       
    where 
          model = trainer $ sample (mkStdGen itr) 1000 (wls++wus)
          f_itr = classifier model
          
          err = err_l+err_u
          err_l = sum [ w*(indicator $ l/=f_itr d) | (w,(l,d)) <- wls]
          err_u = sum [ w*(indicator $ l/=f_itr d) | (w,(l,d)) <- wus]
              
          alpha = 0.5 * (log $ (1-err)/err)
          
          es'  = (alpha,f_itr):es
{-          wus'_unnorm = [ let l'=big_f d in
                          ((exp $ -(bool2num $ l'==big_f d)),(l',d)) | (w,(l,d)) <- wus]
          wls'_unnorm = [ ((exp $ -(bool2num $ l ==big_f d)),(l, d)) | (w,(l,d)) <- wls]-}
          wus'_unnorm = [ let l'=classify (Ensemble es') d in
                          (w*(exp $ -alpha * (indicator $ l'==f_itr d)),(l',d)) | (w,(l,d)) <- wus]
          wls'_unnorm = [ (w*(exp $ -alpha * (indicator $ l ==f_itr d)),(l, d)) | (w,(l,d)) <- wls]
{-          wus'_unnorm = [ let l'=classify (Ensemble es') d in
                          (-(exp $ -(indicator $ l')*(big_F d)),(l',d)) | (w,(l,d)) <- wus]
          wls'_unnorm = [ (-(exp $ -(indicator $ l )*(big_F d)),(l, d)) | (w,(l,d)) <- wls]-}
       
          w_tot = sum [ w | (w,ld) <- wus'_unnorm]
                + sum [ w | (w,ld) <- wls'_unnorm]
          
          wus' = [ (w/w_tot,ld) | (w,ld) <- wus'_unnorm]
          wls' = [ (w/w_tot,ld) | (w,ld) <- wls'_unnorm]
       
{-          wdsnew_unnorm = [ (w*(exp $ (-alpha * (indicator (l==hl)))) ,(l,d)) 
                          | (w,(l,d),hl) <- hL
                          ]
          wdsnew_total = sum [ w | (w,dp) <- wdsnew_unnorm]
          wdsnew = [ (w/wdsnew_total,dp) | (w,dp) <- wdsnew_unnorm ]-}
          
          -- memoization functions
          big_F = weightedClassify $ Ensemble es'
          big_f = classify $ Ensemble es'
          classifier_test = eval (big_f) [dp | (w,dp) <- wls++wus]
             