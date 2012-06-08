module AI.SemiSupervised.RegularizedBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import qualified Data.Vector as V

import AI.Classification
import AI.Ensemble

-- training

{-

This implementation is based on the original RegularizedBoost paper, "Regularized Boost for Semi-Supervised Learning"

A later paper extends this algorithm into the RegBoost algorithm.  This first algorithm, however, is much simpler.

---

-}

data MBParams model = 
    MBParams { iterations :: Int 
             , classifier :: BoolClassifier model 
             , trainer :: Trainer Bool model 
             , sampleRate :: Int
             }

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)
train t c ls us = do
    logAI "RegularizedBoost.train"
    trainItrArr param 0 td (Ensemble [])
                  
    where 
          param = MBParams { iterations = 50 
                           , classifier = c
                           , trainer = t
                           , sampleRate = 1000
                           }
                           
          td = TrainingVec { weights = V.fromList    $ (replicate (length ls) wl) ++ (replicate (length us) wu)
                           , labels = V.fromList     $ [ l | (l,d) <- ls ]++[f d | d<-us]
                           , dataPoints = V.fromList $ [ d | (l,d) <- ls ]++[d | d<-us]
                           , numLabels = length ls
                           }
                           
          beta = 1 -- 0.9
          wl = beta/(fromIntegral $ (length ls))
          wu = (1-beta)/(fromIntegral $ (length us))
          
          f :: DataPoint -> Bool
          f d = c model d
          model = t ls
       
trainItrArr :: MBParams model -> Int -> TrainingVec Bool -> Ensemble model -> LogAI (Ensemble model)
trainItrArr param itr td (Ensemble es) = do
    logAI $ "RegularizedBoost.trainItr: " ++ (show itr)
          ++ "  --  l/u="++(show $ numLabels td)++"/"++(show $ V.length $ labels td)
          ++ "  --  "++(show $ classifier_test)
--           ++ "  --  "++(show stopIndex)
          ++ "  --  "++(show w)
          ++ "  --  "++(show $ errorRate classifier_test)
    if {-stopCondition || -}itr>=iterations param
       then return $ Ensemble es
       else trainItrArr param (itr+1) td' (Ensemble es')
    
    where 
          -- parameters
          beta = 1/2
          
          cost  z =  exp(-z)
          cost' z = -exp(-z)
          cost_tilde z = (cost z) -1
--           cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
--           cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
--           cost  z = (1 - z)^2
--           cost' z = -2*(1-z)
          
          
          es' = (w,model,classifier param):es
          td' = td  { weights = weights' 
                    , labels = labels'
                    }
                    
          labels' = V.fromList [ newlabel i | i<-indexL]
          newlabel i = if i<numLabels td
                          then _y i
                          else num2bool $ _F $ _x i
          
          -- memoization
          indexL = [0 .. (V.length $ labels td)-1 ]
          
          _D i = weights td V.! i
          _y i = labels td V.! i
          _x i = dataPoints td V.! i
          
          _f i = _f_vec V.! i
          _f_vec = V.fromList [f $ _x i | i<-indexL]

          _R i = sum 
              [ (similarity (_x i) (_x j))*(abs $ (bool2num $ _y i) - (bool2num $ _y j))
              | j <- filter (/=i) indexL
              ]

--           algorithm
--           w=1/(1+fromIntegral itr)
          w = (1/2) * (log $ (1 - err)
                           / (err + 0.00001))

          err = sum [ (_D i)*(indicator $ _y i/=(_f i)) | i<-indexL]
--               + sum [ -beta*(_R i)/weights'_unnorm | i<- filter (\i -> _f i == _y i) indexL]
          
          wds = zip (V.toList $ weights td) (zip (V.toList $ labels td) (V.toList $ dataPoints td))
          f = (classifier param) $ model
          model = {-trace (show sampledist) $ -}(trainer param) sampledist
          sampledist = sample (mkStdGen itr) (sampleRate param) wds
          
          _F = weightedClassify $ Ensemble es'
          classifier_test = eval (num2bool . _F) [ (_y i,_x i) | i <- indexL]
          
          margin i = if i < numLabels td
                        then (bool2num $ _y i)*(_F $ _x i)
                        else abs $ _F $ _x i
          
          weights'_unnorm = V.fromList [ (cost' $ margin i)-(beta*(_R i)) | i<-indexL]
          weights'_tot = V.sum weights'_unnorm
          weights' = V.map (\x -> x/weights'_tot) weights'_unnorm

          stopCondition = 0 >= stopIndex
          stopIndex = sum [ (_D i)*(bool2num $ _y i)*(bool2num $ _f i) | i <- indexL]


-- train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [DataPoint] -> LogAI (Ensemble model)
-- train trainer classifier ls us = do
--     logAI "RegularizedBoost.train"
--     trainItr 20 trainer classifier wls wus (Ensemble [])
--     where beta = 0.9
--           wl = beta/(fromIntegral $ (length ls))
--           wu = (1-beta)/(fromIntegral $ (length us))
--           
--           wls = [(wl,ld) | ld <- ls]
--           wus = [(wu,(f d,d)) | d <- us]
--           
--           f :: DataPoint -> Bool
--           f d = classifier model d
--           model = trainer ls
--           
-- trainItr :: Int -> Trainer Bool model -> BoolClassifier model -> [(Double,(Bool,DataPoint))] -> [(Double,(Bool,DataPoint))] -> Ensemble model -> LogAI (Ensemble model)
-- trainItr itr trainer classifier wls wus (Ensemble es) = do
--     logAI $ "RegularizedBoost.trainItr: "++(show itr)
--           ++ "  --  l/u="++(show $ length wls)++"/"++(show $ length wus)
--           ++ "  --  "++(show $ classifier_test)
--           ++ "  --  "++(show err)
--           ++ "  --  "++(show $ errorRate classifier_test)
-- --           ++ "  --  "++(show [ (w,l,num2bool $ big_F d) | (w,(l,d)) <- wls])
--     if itr==0
--        then return $ Ensemble es
--        else trainItr (itr-1) trainer classifier wls' wus' (Ensemble es')
--        
--     where 
--           -- parameters
--           alpha = 1
--           beta = 1/2
--           
--           cost  z = exp(-z)
--           cost' z = -exp(-z)
--           cost_tilde z = (cost z) - 1
--           
--           -- same equations as ASSEMBLE
--           model = trainer $ sample (mkStdGen itr) 100 (wls++wus)
--           f_itr = classifier model
--           weight = 0.5 * (log $ (1-err)/err)
--           es'  = (weight,model,classifier):es
-- 
--           -- from equation 9, as modified in paragraph at the end of section 2.2
-- --           err = 2*misclassification_err+2*classincopbatability_err-1
--           err = misclassification_err -- +classincompatability_err
-- --           err = classincompatability_err
--           
--           misclassification_err = sum [ w*(indicator $ l/=f_itr d) | (w,(l,d)) <- wls]
--                                 + sum [ w*(indicator $ l/=f_itr d) | (w,(l,d)) <- wus]
--               
-- --           classincompatability_err = sum [ -beta*(r d)/denom | (w,(l,d)) <- wls++wus]
-- --           denom = sum [alpha*(cost' $ (bool2num l)*(big_F d))-beta*(r d) | (w,(l,d)) <- wls++wus]
--               
--           -- from equation 6          
-- --           r d' = sum [ (similarity d' d)*(cost_tilde $ 2*(indicator $ d'/=d)) | (w,(l,d)) <- wls++wus]
--           
--           -- from equation 8
--           wus'_unnorm = [ let l'=classify (Ensemble es') d 
--                               w'=alpha*(cost' $ (bool2num l')*(big_F d)) -- - beta*(r d)
--                           in (w',(l',d)) | (w,(l,d)) <- wus]
-- 
--           wls'_unnorm = [ let w'=alpha*(cost' $ (bool2num  l)*(big_F d)) -- - beta*(r d)
--                           in (w',(l,d)) | (w,(l,d)) <- wls]
-- 
-- --           wus'_unnorm = [ let l'=classify (Ensemble es') d in
-- --                           (w*(exp $ -weight * (indicator $ l'==f_itr d)),(l',d)) | (w,(l,d)) <- wus]
-- --           wls'_unnorm = [ (w*(exp $ -weight * (indicator $ l ==f_itr d)),(l, d)) | (w,(l,d)) <- wls]
-- 
--        
--           w_tot = sum [ w | (w,ld) <- wus'_unnorm]
--                 + sum [ w | (w,ld) <- wls'_unnorm]
--           
--           wus' = [ (w/w_tot,ld) | (w,ld) <- wus'_unnorm]
--           wls' = [ (w/w_tot,ld) | (w,ld) <- wls'_unnorm]
--           
--           -- memoization functions
--           big_F = weightedClassify (Ensemble es)
--           classifier_test = eval (num2bool . big_F) [dp | (w,dp) <- wls++wus]
--              