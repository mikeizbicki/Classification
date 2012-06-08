module AI.SemiSupervised.SemiMarginBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import qualified Data.Vector as V

import AI.Classification
import AI.Ensemble

-- training

{-

SemiMarginBoost is a semi-supervised extension of MarginBoost

This implementation is based on "Semi-Supervised MarginBoost"
by d'Alche-Buc, Grandvalet, and Abroise

-}

data MBParams model = 
    MBParams { iterations :: Int 
             , classifier :: BoolClassifier model 
             , trainer :: Trainer Bool model 
             , sampleRate :: Int
             }

train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)
train t c ls us = do
    logAI "SemiMarginBoost.train"
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
       
-- trainItrArr :: MBParams model -> Int -> TrainingVec Bool -> Ensemble model -> LogAI (Ensemble model)
-- trainItrArr param itr td (Ensemble es) = do
--     logAI $ "MarginBoost.trainItr: " ++ (show itr)
--           ++ "  --  "++(show $ classifier_test)
--           ++ "  --  "++(show stopIndex)
--           ++ "  --  "++(show w)
--           ++ "  --  "++(show $ errorRate classifier_test)
--     if {-stopCondition || -}itr>=iterations param
--        then return $ Ensemble es
--        else trainItrArr param (itr+1) td' (Ensemble es')
--     
--     where 
--           -- parameters
--           cost  z =  exp(-z)
--           cost' z = -exp(-z)
-- --           cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
-- --           cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
-- --           cost  z = (1 - z)^2
-- --           cost' z = -2*(1-z)
--           
--           
--           es' = (w,model,classifier param):es
--           td' = td  { weights = weights' }
--           
--           -- memoization
--           indexL = [0 .. (V.length $ labels td)-1 ]
--           
--           _D i = weights td V.! i
--           _y i = labels td V.! i
--           _x i = dataPoints td V.! i
--           
--           _f i = _f_vec V.! i
--           _f_vec = V.fromList [f $ _x i | i<-indexL]
-- 
--           -- algorithm
-- --           w=1/(1+fromIntegral itr)
--           w = (1/2) * (log $ (sum [ (_D i)*(indicator $ _y i==(_f i)) | i<-indexL])
--                            / (sum [ (_D i)*(indicator $ _y i/=(_f i)) | i<-indexL] + 0.00001))
-- 
--           wds = zip (V.toList $ weights td) (zip (V.toList $ labels td) (V.toList $ dataPoints td))
--           f = (classifier param) $ model
--           model = {-trace (show sampledist) $ -}(trainer param) sampledist
--           sampledist = sample (mkStdGen itr) (sampleRate param) wds
--           
--           _F = weightedClassify $ Ensemble es'
--           classifier_test = eval (num2bool . _F) [ (_y i,_x i) | i <- indexL]
-- 
--           weights'_unnorm = V.fromList [ cost' $ (bool2num $ _y i)*(_F $ _x i) | i<-indexL]
--           weights'_tot = V.sum weights'_unnorm
--           weights' = V.map (\x -> x/weights'_tot) weights'_unnorm
-- 
--           stopCondition = 0 >= stopIndex
--           stopIndex = sum [ (_D i)*(bool2num $ _y i)*(bool2num $ _f i) | i <- indexL]

       
trainItrArr :: MBParams model -> Int -> TrainingVec Bool -> Ensemble model -> LogAI (Ensemble model)
trainItrArr param itr td (Ensemble es) = do
    logAI $ "SemiMarginBoost.trainItr: " ++ (show itr)
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
          cost  z =  exp(-z)
          cost' z = -exp(-z)
--           cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
--           cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
--           cost  z = (1 - z)^2
--           cost' z = -2*(1-z)
          
          
          es' = (w,model,classifier param):es
          td' = td  { weights = weights' 
                    , labels = labels'
                    }
                    
          labels' = V.fromList [ newlabel i | i<-indexLsmall]
          newlabel i = if i<numLabels td
                          then _y i
                          else num2bool $ _F $ _x i
--                           else _f i

--           td' = td  { weights = weights' }
          
          -- memoization
--           indexLsmall = [0 .. numLabels td ]
          indexLsmall = indexL
          indexL = [0 .. (V.length $ labels td)-1 ]
          
          _D i = weights td V.! i
          _y i = labels td V.! i
          _x i = dataPoints td V.! i
          
          _f i = _f_vec V.! i
          _f_vec = V.fromList [f $ _x i | i<-indexLsmall]

--           algorithm
--           w=1/(1+fromIntegral itr)
          w = (1/2) * (log $ (sum [ (_D i)*(indicator $ _y i==(_f i)) | i<-indexLsmall])
                           / (sum [ (_D i)*(indicator $ _y i/=(_f i)) | i<-indexLsmall] + 0.00001))
--           w = 0.1

          wds = zip (V.toList $ weights td) (zip (V.toList $ labels td) (V.toList $ dataPoints td))
          f = (classifier param) $ model
          model = {-trace (show sampledist) $ -}(trainer param) sampledist
          sampledist = sample (mkStdGen itr) (sampleRate param) wds
          
          _F = weightedClassify $ Ensemble es'
          classifier_test = eval (num2bool . _F) [ (_y i,_x i) | i <- indexLsmall]
          
          margin i = if i < numLabels td
                        then (bool2num $ _y i)*(_F $ _x i)
                        else abs $ _F $ _x i
          
          weights'_unnorm = V.fromList [ cost' $ margin i | i<-indexL]
          weights'_tot = V.sum weights'_unnorm
          weights' = V.map (\x -> x/weights'_tot) weights'_unnorm

          stopCondition = 0 >= stopIndex
          stopIndex = sum [ (_D i)*(bool2num $ _y i)*(bool2num $ _f i) | i <- indexLsmall]


-- train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)
-- train t c ls us = do
--     logAI "SemiMarginBoost.train"
--     trainItrArr param 0 td (Ensemble [])
--                   
--     where 
--           param = MBParams { iterations = 50 
--                            , classifier = c
--                            , trainer = t
--                            , sampleRate = 1000
--                            }
--                            
--           td = TrainingVec { weights = V.fromList    $ (replicate (length ls) wl) ++ (replicate (length us) wu)
--                            , labels = V.fromList     $ [ l | (l,d) <- ls ]++[f d | d<-us]
--                            , dataPoints = V.fromList $ [ d | (l,d) <- ls ]++[d | d<-us]
--                            , numLabels = length ls
--                            }
--                            
--           beta = 0.9
--           wl = beta/(fromIntegral $ (length ls))
--           wu = (1-beta)/(fromIntegral $ (length us))
--           
--           f :: DataPoint -> Bool
--           f d = c model d
--           model = t ls
--         
-- trainItrArr :: MBParams model -> Int -> TrainingVec Bool -> Ensemble model -> LogAI (Ensemble model)
-- trainItrArr param itr td (Ensemble es) = do
--     logAI $ "SemiMarginBoost.trainItr: " ++ (show itr)
--           ++ "  --  "++(show $ classifier_test)
--           ++ "  --  "++(show stopIndex)
--           ++ "  --  "++(show $ errorRate classifier_test)
--     if {-stopCondition || -}itr>=iterations param
--        then return $ Ensemble es
--        else trainItrArr param (itr+1) td' (Ensemble es')
--     
--     where 
--           -- params
--           cost  z =  exp(-z)
--           cost' z = -exp(-z)
-- --           cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
-- --           cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
-- --           cost  z = (1 - z)^2
-- --           cost' z = -2*(1-z)
--           
--           
--           es' = (w,model,classifier param):es
--           td' = td  { weights = weights' 
--                     , labels = labels'
--                     }
--                     
--           labels' = V.fromList [ newlabel i | i<-indexL]
--           newlabel i = if i<numLabels td
--                           then _y i
--                           else _f i
--           
--           -- memoization
--           indexL = [0 .. (V.length $ labels td)-1 ]
--           
--           _D i = weights td V.! i
--           _y i = labels td V.! i
--           _x i = dataPoints td V.! i
--           
--           _f i = _f_vec V.! i
--           _f_vec = V.fromList [f $ _x i | i<-indexL]
-- 
--           -- algorithm
-- --           w=1/(1+fromIntegral itr)
--           w = (1/2) * (log $ (sum [ (_D i)*(indicator $ _y i==(_f i)) | i<-indexL])
--                            / (sum [ (_D i)*(indicator $ _y i/=(_f i)) | i<-indexL]))
-- 
--           wds = take (numLabels td) $ zip (V.toList $ weights td) (zip (V.toList $ labels td) (V.toList $ dataPoints td))
--           f = (classifier param) model
--           model = (trainer param) $ sample (mkStdGen itr) (sampleRate param) wds
--           
--           _F = weightedClassify $ Ensemble es
--           classifier_test = eval (num2bool . _F) [ (_y i,_x i) | i <- take (numLabels td) indexL]
-- --           classifier_test = eval (num2bool . _F) [ (_y i,_x i) | i <- indexL]
-- 
--           g i = if i < numLabels td
--                    then (bool2num $ _y i)*(_F $ _x i)
--                    else abs (_F $ _x i)
-- 
--           weights'_unnorm = V.fromList [ cost' $ (g i) | i<-indexL]
--           weights'_tot = V.sum weights'_unnorm
--           weights' = V.map (\x -> x/weights'_tot) weights'_unnorm
-- 
--           stopCondition = 0 >= stopIndex
--           stopIndex = sum [ (_D i)*(g i) | i <- indexL]
-- --           stopIndex = sum [ (_D i)*(bool2num $ _y i)*(bool2num $ _f i) | i <- indexL]

