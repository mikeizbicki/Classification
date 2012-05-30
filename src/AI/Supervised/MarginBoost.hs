module AI.Supervised.MarginBoost
    where

import Database.HDBC
import Debug.Trace
import System.Random

import qualified Data.Vector as V
-- import qualified Data.Array.Repa as R

import AI.Classification
import AI.Ensemble

-- training

{-

MarginBoost is very similar to AdaBoost, but seems to converge a little faster?

This implementation is based on Algorithm 12.4 in "Functional Gradient Techniques for Conbining Hypotheses"
by Mason, Baxter, Bartlett, and Frean

-}

data MBParams model = 
    MBParams { iterations :: Int 
             , classifier :: BoolClassifier model 
             , trainer :: Trainer Bool model 
             , sampleRate :: Int
             }

data TrainingVec = TrainingVec { weights :: V.Vector Double
                               , labels :: V.Vector Bool
                               , dataPoints :: V.Vector DataPoint
                               }

trainArr :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (Ensemble model)
trainArr t c ds = 
    supertrainArr MBParams { iterations = 20 
                           , classifier = c
                           , trainer = t
                           , sampleRate = 5000
                           }
                  TrainingVec { weights = V.empty
                              , labels = V.fromList [ l | (l,d) <- ds ]
                              , dataPoints = V.fromList [ d | (l,d) <- ds ]
                              }

supertrainArr :: MBParams model -> TrainingVec -> LogAI (Ensemble model)
supertrainArr param td = do
    logAI "MarginBoost.train"
    trainItrArr param 0 td' (Ensemble [])
        where td' = td { weights = V.fromList $ replicate len val }
              len = V.length $ labels td
              val = 1/(fromIntegral $ V.length $ dataPoints td) :: Double
        
trainItrArr :: MBParams model -> Int -> TrainingVec -> Ensemble model -> LogAI (Ensemble model)
trainItrArr param itr td (Ensemble es) = do
    logAI $ "MarginBoost.trainItr: " ++ (show itr)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show stopIndex)
          ++ "  --  "++(show $ errorRate classifier_test)
    if stopCondition || itr>iterations param
       then return $ Ensemble es
       else trainItrArr param (itr+1) td' (Ensemble es')
    
    where 
          -- params
--           cost  z =  exp(-z)
--           cost' z = -exp(-z)
--           cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
--           cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
          cost  z = (1 - z)^2
          cost' z = -2*(1-z)
          
          
          es' = (w,f):es
          td' = td  { weights = weights' }
          
          -- memoization
          indexL = [0 .. (V.length $ labels td)-1 ]
          
          _D i = weights td V.! i
          _y i = labels td V.! i
          _x i = dataPoints td V.! i
          
          _f i = _f_vec V.! i
          _f_vec = V.fromList [f $ _x i | i<-indexL]

          -- algorithm
          w=1/(1+fromIntegral itr)
--           w = (1/2) * (log $ (sum [ (_D i)*(indicator $ _y i==(_f i)) | i<-indexL])
--                            / (sum [ (_D i)*(indicator $ _y i/=(_f i)) | i<-indexL]))

          wds = zip (V.toList $ weights td) (zip (V.toList $ labels td) (V.toList $ dataPoints td))
          f = (classifier param) $ (trainer param) $ sample (mkStdGen itr) (sampleRate param) wds
          
          _F = weightedClassify $ Ensemble es'
          classifier_test = eval (num2bool . _F) [ (_y i,_x i) | i <- indexL]

          weights'_unnorm = V.fromList [ cost' $ (bool2num $ _y i)*(_F $ _x i)| i<-indexL]
          weights'_tot = V.sum weights'_unnorm
          weights' = V.map (\x -> x/weights'_tot) weights'_unnorm

          stopCondition = 0 >= stopIndex
          stopIndex = sum [ (_D i)*(bool2num $ _y i)*(bool2num $ _f i) | i <- indexL]

---------------

-- train :: Trainer Bool model -> BoolClassifier model -> [(Bool,DataPoint)] -> LogAI (Ensemble model)
-- train t c = 
--     supertrain $ MBParams { iterations = 20 
--                           , classifier = c
--                           , trainer = t
--                           , sampleRate = 5000
--                           }
-- 
-- supertrain :: MBParams model -> [(Bool,DataPoint)] -> LogAI (Ensemble model)
-- supertrain param ds = do
--     logAI "MarginBoost.train"
--     trainItr param 0 wds (Ensemble [])
--    where wds = [ (w,dl) | dl <- ds]
--          w = 1/(fromIntegral $ length ds)
--        
-- trainItr :: MBParams model -> Int -> [(Double,(Bool,DataPoint))] -> Ensemble model -> LogAI (Ensemble model)
-- trainItr param itr wds (Ensemble es) = do
--     logAI $ "MarginBoost.trainItr: " ++ (show itr)
--           ++ "  --  "++(show $ classifier_test)
--           ++ "  --  "++(show stopIndex)
--           ++ "  --  "++(show $ errorRate classifier_test)
--     if stopCondition || itr>iterations param
--        then return $ Ensemble es
--        else trainItr param (itr+1) wds' (Ensemble es')
--     
--     where 
--           -- params
--           cost  z =  exp(-z)
--           cost' z = -exp(-z)
--           
--           w = (1/2) * (log $ (sum [ w*(indicator $ l==fd) | (w,(l,d),fd) <- wdsf])
--                            / (sum [ w*(indicator $ l/=fd) | (w,(l,d),fd) <- wdsf]))
-- 
--           -- algorithm
--           f = (classifier param) $ (trainer param) $ sample (mkStdGen itr) (sampleRate param) wds
--           
--           es' = (w,f):es
--           _F = weightedClassify $ Ensemble es'
--           classifier_test = eval (num2bool . _F) [ld | (w,ld) <- wds]
--           
--           wds'_unnorm = [ (cost' $ (bool2num l)*(_F d),(l,d)) | (w,(l,d)) <- wds ] 
--           wds'_tot = sum [ w | (w,ld) <- wds'_unnorm ]
--           wds' = [ (w/wds'_tot,ld) | (w,ld) <- wds'_unnorm ]
-- 
--           stopCondition = 0 >= stopIndex
--           stopIndex = sum [ w*(bool2num l)*(bool2num $ fd) | (w,(l,d),fd) <- wdsf]
-- 
--           -- memoization functions
--           wdsf = [ (w,(l,d),f d) | (w,(l,d)) <- wds ]

