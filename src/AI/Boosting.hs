{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module AI.Boosting
    where

import Database.HDBC
import Debug.Trace
import System.Random

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

-- import qualified AI.Classification as Q
import AI.Classification (num2bool, bool2num, indicator, eval, sample, logAI, errorRate, LogAI, DataPoint, Trainer, BoolClassifier)
import AI.Ensemble


{-

This is a generic boosting framework.

-}

data BoostParams model = BoostParams 
    { iterations :: Int 
    , sampleRate :: Int
    , trainer :: Trainer Bool model 
    }

data TrainingVec label = TrainingVec 
    { weights :: V.Vector Double
    , labels :: V.Vector label
    , dataPoints :: VB.Vector DataPoint
    , _f_vec :: V.Vector Bool
    , _F_vec :: V.Vector Double
    , numLabels :: Int
    }
    
data BoostAlg = BoostAlg
    { name :: String
    
    , _D0 :: Int -> Double
    , _y0 :: Int -> Bool
    
    , stepSize :: TrainingVec Bool -> TrainingVec Bool -> Double
    , newWeight :: TrainingVec Bool -> TrainingVec Bool -> Int -> Double
    
    , stopCondition :: TrainingVec Bool -> TrainingVec Bool -> Bool
    , stopIndex :: TrainingVec Bool -> TrainingVec Bool -> Double
    }
    
type BoostAlgCreator model = BoostParams model -> [(Bool,DataPoint)] -> [(DataPoint)] -> BoostAlg
    
--------------

ssmBoost params ls us = BoostAlg
    { name = " <<< SSMBoost >>> "
    
    -- initialization 
    , _D0 = \i -> 
        if i<numLabels
            then beta/(fromIntegral $ (length ls))
            else (1-beta)/(fromIntegral $ (length us))

    , _y0 = \i ->
        if i<numLabels
            then fst $ ls !! i
            else f0 $ us !! (i-numLabels)

    -- iteration
    , stepSize = \td -> \td' ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td' V.! i
            
        in (1/2) * (log $ (sum [ (_D i)*(indicator $ _y i==(_f i)) | i<-indexL])
                        / (sum [ (_D i)*(indicator $ _y i/=(_f i)) | i<-indexL] + 0.00001))

    , newWeight = \td -> \td' -> \i ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td' V.! i
            _F i = _F_vec td' V.! i
            margin i = 
                if i < numLabels
                    then (bool2num $ _y i)*(_F i)
                    else abs $ _F i
          
        in cost' $ margin i

    , stopCondition = \td -> \td' -> 
        0 >= (stopIndex (ssmBoost params ls us) td td')
        
    , stopIndex = \td -> \td' ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td' V.! i
            _F i = _F_vec td' V.! i
        
        in sum [ (_D i)*(bool2num $ _y i)*(bool2num $ _f i) | i <- indexL]
    }
    where 
        -- initialization
        beta = 0.9
        numLabels = length ls 
        
        f0 :: DataPoint -> Bool
--         f0 d = (classifier params) model0 d
        f0 d = classify model0 d
        model0 = (trainer params) ls
        
        indexL = [0..(length ls)+(length us)-1]

        cost  z =  exp(-z)
        cost' z = -exp(-z)
--         cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
--         cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
--         cost  z = (1 - z)^2
--         cost' z = -2*(1-z)
        
--------------

train :: (ClassifyModel model Bool)=>BoostAlgCreator model -> BoostParams model -> [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)
train algcreator param ls us = do
    logAI $ (name alg)++".train"
    trainItrArr alg param 0 td (Ensemble [])
                  
    where 
          td = TrainingVec { weights = V.fromList     $ [ _D0 alg i | i <- [0..(length ls)+(length us)-1] ]
                           , labels = V.fromList      $ [ _y0 alg i | i <- [0..(length ls)+(length us)-1] ]
                           , dataPoints = VB.fromList $ [ d | (l,d) <- ls ]++[d | d<-us]
                           , _f_vec = V.empty
                           , _F_vec = V.empty
                           , numLabels = length ls
                           }
          alg = algcreator param ls us
       
trainItrArr :: (ClassifyModel model Bool)=>BoostAlg -> BoostParams model -> Int -> TrainingVec Bool -> Ensemble model -> LogAI (Ensemble model)
trainItrArr alg param itr td (Ensemble es) = do
    logAI $ (name alg) ++ ".trainItr: " ++ (show itr)
          ++ "  --  l/u="++(show $ numLabels td)++"/"++(show $ V.length $ labels td)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show $ errorRate classifier_test)
    if (stopCondition alg td td') || itr>=iterations param
       then return $ Ensemble es
       else trainItrArr alg param (itr+1) td' (Ensemble es')
    
    where 
          -- create parameters for next iteration
--           es' = (stepSize alg td td',model,classifier param):es
          es' = (stepSize alg td td',model):es
          td' = td  { weights = weights' 
                    , labels = labels'
                    , _f_vec = V.fromList [f $ _x i | i<-indexL]
                    , _F_vec = V.fromList [ weightedClassify (Ensemble es') (_x i) | i<-indexL]
                    }

          _F = weightedClassify $ Ensemble es'
          classifier_test = eval (num2bool . _F) [ (_y i,_x i) | i <- indexL]

          labels' = V.fromList [ newlabel i | i<-indexL]
          newlabel i = if i<numLabels td
                          then _y i
                          else num2bool $ _F $ _x i

          weights' = normalizeVector $ V.fromList [ newWeight alg td td' i | i<-indexL]
          
          -- sample our next distribution
          wds = zip (V.toList $ weights td) (zip (V.toList $ labels td) (VB.toList $ dataPoints td))
--           f = (classifier param) $ model
          f = classify model
          model = (trainer param) sampledist
          sampledist = sample (mkStdGen itr) (sampleRate param) wds
          
          -- shortcuts
          indexL = [0 .. (V.length $ labels td)-1 ]
          
          _D i = weights td V.! i
          _y i = labels td V.! i
          _x i = dataPoints td VB.! i
          
-- normalizeVector :: (Fractional a, V.Storable a) => V.Vector a -> V.Vector a
normalizeVector :: (Fractional a, V.Unbox a) => V.Vector a -> V.Vector a
normalizeVector v = V.map (\x -> x/normFactor) v
    where
        normFactor = V.sum v
