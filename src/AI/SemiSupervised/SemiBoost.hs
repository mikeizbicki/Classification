module AI.SemiSupervised.SemiBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import AI.Classification
import AI.Ensemble

-- training

{-

This implementation is based on the original SemiBoost paper, "SemiBoost: Boosting for Semi-supervised Learning," TPAMI '09

-}

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [DataPoint] -> LogAI (Ensemble model)
train trainer classifier ls us = do
    logAI "SemiBoost.train"
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
    logAI $ "SemiBoost.trainItr: "++(show itr)
          ++ "  --  l/u="++(show $ length wls)++"/"++(show $ length wus)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show alpha)
          ++ "  --  "++(show $ errorRate classifier_test)
    if itr==0
       then return $ Ensemble es
       else trainItr (itr-1) trainer classifier wls' wus' (Ensemble es')
       
    where 
          -- parameters
          c=1
          
          -- memoization functions
          big_H::DataPoint -> Double
          big_H dp = weightedClassify (Ensemble es) dp
          classifier_test = eval (num2bool . big_H) [dp | (w,dp) <- wls++wus]
          
          wlsH = [ (w,(l,d),h_itr d, big_H d) | (w,(l,d)) <- wls]
          wusH = [ (w,(l,d),h_itr d, big_H d) | (w,(l,d)) <- wus]
          
          -- algorithm
                    
          pl=[sum [ (indicator       l2)*(similarity d1 d2) * (exp $ -2*(_H1)) | (w2,(l2,d2),h2,_H2) <- wlsH] | (w1,(l1,d1),h1,_H1) <- wlsH]
          ql=[sum [ (indicator $ not l2)*(similarity d1 d2) * (exp $  2*(_H1)) | (w2,(l2,d2),h2,_H2) <- wlsH] | (w1,(l1,d1),h1,_H1) <- wlsH]
          qu=[c/2*sum [ (similarity d1 d2) * (exp $ (_H2)-(_H1)) | (w2,(l2,d2),h2,_H2) <- wusH] | (w1,(l1,d1),h1,_H1) <- wusH]
          pu=[c/2*sum [ (similarity d1 d2) * (exp $ (_H1)-(_H2)) | (w2,(l2,d2),h2,_H2) <- wusH] | (w1,(l1,d1),h1,_H1) <- wusH]
          
          pqwls=zip (zip pl ql) wls
          pqwus=zip (zip pu qu) wus
          
          wls' = [(abs $ p-q,(             l,d)) | ((p,q),(w,(l,d))) <- pqwls]
          wus' = [(abs $ p-q,(num2bool $ p-q,d)) | ((p,q),(w,(l,d))) <- pqwus]
          
--           model = trainer $ sample (mkStdGen itr) 300 (wls'++wus')
          model = trainer $ sample (mkStdGen itr) 300 (wls++wus)
          h_itr = classifier model
          
          alpha = (1/4) * (log $ ((sum [p*(indicator $ h_itr d==True ) | ((p,q),(w,(l,d)))<-pqwls])
                                 +(sum [q*(indicator $ h_itr d==False) | ((p,q),(w,(l,d)))<-pqwls])) 
                               / ((sum [p*(indicator $ h_itr d==False) | ((p,q),(w,(l,d)))<-pqwls])
                                 +(sum [q*(indicator $ h_itr d==True ) | ((p,q),(w,(l,d)))<-pqwls]))
                               )
          
          es' = (alpha,h_itr):es
