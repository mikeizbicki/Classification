{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module AI.Boosting
    where

import Debug.Trace
import System.Random

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

-- import qualified AI.Classification as Q
import AI.Classification -- (num2bool, bool2num, indicator, eval, sample, logAI, errorRate, LogAI, DataPoint, Trainer, BoolClassifier)
import AI.Ensemble
import AI.RandomUtils

import Control.Monad.Random 
import System.Random
{-

This is a generic boosting framework.

-}

data BoostParams model = BoostParams 
    { iterations :: Int 
    , sampleRate :: Int
    , obeyStopCriteria :: Bool
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
    , indices :: [Int]
    
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
    { name = "SSMBoost"
    , indices = indexL
        
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
            _y' i = labels td' V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td V.! i
            _f' i = _f_vec td' V.! i
            
            err = sum [ (_D i)*(indicator $ _y i /= _f i) | i<-indexL ]
            
        in 0.5 * (log $ (1-err)/(err+0.00001))

    , newWeight = \td -> \td' -> \i ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _y' i = labels td' V.! i
            _x i = dataPoints td VB.! i
            _f' i = _f_vec td' V.! i
            _F' i = _F_vec td' V.! i
            margin i = (bool2num $ _y' i)*(_F' i)
        in (_D0 (ssmBoost params ls us) i)*(cost' $ margin i)
          
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
--         beta = 1
        beta = 0.9
        numLabels = length ls 
        
        f0 :: DataPoint -> Bool
        f0 d = classify model0 d
        model0 = (trainer params) ls
        
--         indexL = [0..(length ls)-1]
        indexL = [0..(length ls)+(length us)-1]

        cost  z =  exp(-z)
        cost' z = -exp(-z)
--         cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
--         cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
--         cost  z = (1 - z)^2
--         cost' z = -2*(1-z)

marginBoost params ls us = BoostAlg
    { name = "MarginBoost"
    , indices = indexL
    
    -- initialization 
    , _D0 = \i -> 1/(fromIntegral $ length indexL)
    , _y0 = \i ->fst $ ls !! i

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
            margin i = (bool2num $ _y i)*(_F i)
        in cost' $ margin i

    , stopCondition = \td -> \td' -> 
        0 >= (stopIndex (marginBoost params ls us) td td')
        
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
        indexL = [0..(length ls)-1]

        cost  z =  exp(-z)
        cost' z = -exp(-z)
--         cost  z =  log $ 1 + (exp $ -2*z) -- LogitBoost
--         cost' z = (-2 * (exp $ -2*z)) / (1+(exp $ -2*z))
--         cost  z = (1 - z)^2
--         cost' z = -2*(1-z)


adaBoost params ls us = (marginBoost params ls us)
    { name = "adaBoost"
    , newWeight = \td -> \td' -> \i ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _f i = _f_vec td' V.! i
            alpha = stepSize (adaBoost params ls us) td td'
          
        in (_D i)*(exp $ -alpha * (bool2num $ _y i==_f i))
    }

assembleBoost params ls us = (ssmBoost params ls us)
    { name = "assembleBoost"
    , newWeight = \td -> \td' -> \i ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _y' i = labels td' V.! i
            _f' i = _f_vec td' V.! i
            alpha = stepSize (assembleBoost params ls us) td td'
          
        in (_D i)*(exp $ -alpha * (bool2num $ _y' i==_f' i))
    }

semiBoost params ls us = (ssmBoost params ls us)
    { name = "SemiBoost"
    , indices = indexL

    -- iteration
    , stepSize = \td -> \td' ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td' V.! i
            _F i = _F_vec td V.! i

            p i =       sum [(indicator $ _y j)*(similarity (_x i) (_x j))*(exp $ -2*(_F i)) | j <- labeledPoints]
                + c/2 * sum [                   (similarity (_x i) (_x j))*(exp $ (_F j) - (_F i)) | j <- unlabeledPoints]
                
            q i =       sum [(indicator $ not $_y j)*(similarity (_x i) (_x j))*(exp $ 2*(_F i)) | j <- labeledPoints]
                + c/2 * sum [                        (similarity (_x i) (_x j))*(exp $ (_F i) - (_F j)) | j <- unlabeledPoints]
          
            
            err = ( sum [ (indicator $ not $ _f i) * (p i) | i <- indexL]
                   +sum [ (indicator $       _f i) * (q i) | i <- indexL]
                  )
                  / sum [ (p i) + (q i) | i <- indexL ]
            
        in (1/4)*(log $ (1-err)/(err+0.0000001))

    , newWeight = \td -> \td' -> \i ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td' V.! i
            _F i = _F_vec td V.! i
            
            p i =       sum [(indicator $ _y j)*(similarity (_x i) (_x j))*(exp $ -2*(_F i)) | j <- labeledPoints]
                + c/2 * sum [                   (similarity (_x i) (_x j))*(exp $ (_F j) - (_F i)) | j <- unlabeledPoints]
                
            q i =       sum [(indicator $ not $_y j)*(similarity (_x i) (_x j))*(exp $ 2*(_F i)) | j <- labeledPoints]
                + c/2 * sum [                        (similarity (_x i) (_x j))*(exp $ (_F i) - (_F j)) | j <- unlabeledPoints]
          
        in abs $ (p i) - (q i)

    , stopCondition = \td -> \td' -> 
        0 >= (stopIndex (ssmBoost params ls us) td td')
        
    , stopIndex = \td -> \td' ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td' V.! i
            _F i = _F_vec td' V.! i
        
        in 0.1
    }
    where 
        -- initialization
        indexL = [0..(length ls)+(length us)-1]
        
        labeledPoints = [0..(length ls)-1]
        unlabeledPoints = [(length ls)-1 .. (length us) -1]
        
        c = (fromIntegral $ length ls)/(fromIntegral $ length us)


regAssembleBoost1 params ls us = (ssmBoost params ls us)
    { name = "regAssembleBoost1"
    , newWeight = \td -> \td' -> \i ->
        let _D i = weights td V.! i
            _y i = labels td V.! i
            _x i = dataPoints td VB.! i
            _f i = _f_vec td' V.! i
            _F i = _F_vec td' V.! i
            step = stepSize (assembleBoost params ls us) td td'
            
            _R i = sum [ (similarity (_x i) (_x j))*(cost_tilde $ -(abs $ (bool2num $ _y i) - (bool2num $ _y j))) | j <- indexL]

        in cost' $ (bool2num $ _y i)*(_F i)
--         in alpha*(_D i)*(exp $ -step * (bool2num $ _y i==_f i))
--           -beta*(_R i)
        
    }
    where 
        alpha = 1
        beta = 1/2
        
        indexL = [0..(length ls)+(length us)-1]

        cost  z =  exp(-z)
        cost' z = -exp(-z)
        cost_tilde z = (cost z) -1

-- regAssembleBoost2 params ls us = (ssmBoost params ls us)
--     { name = "regAssembleBoost2"
--     , stepSize = \td -> \td' ->
--         let _D i = weights td V.! i
--             _D' i = weights td' V.! i
--             _y i = labels td V.! i
--             _x i = dataPoints td VB.! i
--             _f i = _f_vec td' V.! i
--             _F i = _F_vec td' V.! i
--             
--             _R i = sum [ (similarity (_x i) (_x j))*(cost_tilde $ -(abs $ (bool2num $ _y i) - (bool2num $ _y j))) | j <- filter (\j->j/=i) indexL]
--             
--             err = trace ("errReg: "++show errReg) $ sum [ (_D i)*(indicator $ _y i/=(_f i)) | i<-indexL]
-- --                 
--             errReg = -(sum [ (beta)*(_R i) | i<-indexL])
--                      /(sum [ (alpha)*(cost' $ (bool2num $ _y k)*(_F k)) - (beta)*(_R k) | k<-indexL]) 
--             
--         in (1/2) * (log $ (1 - err)/(err+0.00000001)) / (log 10)
-- 
--     , newWeight = \td -> \td' -> \i ->
--         let _D i = weights td V.! i
--             _y i = labels td V.! i
--             _x i = dataPoints td VB.! i
--             _f i = _f_vec td' V.! i
--             step = stepSize (assembleBoost params ls us) td td'
--             
--             _R i = sum [ (similarity (_x i) (_x j))*(cost_tilde $ -(abs $ (bool2num $ _y i) - (bool2num $ _y j))) | j <- indexL]
--           
--         in alpha*(_D i)*(exp $ -step * (bool2num $ _y i==_f i))
--           -beta*(_R i)
--         
--     }
--     where 
--         alpha = 1
--         beta = 1/2
--         
-- --         indexL = [0..(length ls)+(length us)-1]
--         indexL = [0..(length ls){-+(length us)-}-1]
-- 
--         cost  z =  exp(-z)
--         cost' z = -exp(-z)
--         cost_tilde z = (cost z) -1


------------

train :: (ClassifyModel model Bool)=>BoostAlgCreator model -> BoostParams model -> [(Bool,DataPoint)] -> [(DataPoint)] -> LogAI (Ensemble model)
train algcreator param ls us = do
    seed' <- getRandom -- (1,100000::Int)
    logAI $ (name alg)++".train"
    trainItrArr alg param 0 seed' td (Ensemble [])
                  
    where 
        alg = algcreator param ls us

        td = TrainingVec 
            { weights = weightsVec
            , labels = labelsVec
            , dataPoints = dataVec
            , _f_vec = V.fromList $ [ f i | i <- indices alg] 
            , _F_vec = V.fromList $ replicate (length $ indices alg) 0
            , numLabels = length ls
            }
        weightsVec = V.fromList     $ [ _D0 alg i | i <- indices alg ]
        labelsVec = V.fromList      $ [ _y0 alg i | i <- indices alg ]
        dataVec = VB.fromList       $ [ d | (l,d) <- ls ]++[d | d<-us]
        
        model = genModelFromDistribution 0 param weightsVec labelsVec dataVec
        f i = (classify model) (dataVec VB.! i)
          
trainItrArr :: (ClassifyModel model Bool)=>BoostAlg -> BoostParams model -> Int -> Int -> TrainingVec Bool -> Ensemble model -> LogAI (Ensemble model)
trainItrArr alg param itr seed td (Ensemble es) = do
    seed' <- getRandom -- (1,100000::Int)
    logAI $ (name alg) ++ ".trainItr: " ++ (show itr)
          ++ "  --  l/u="++(show $ numLabels td)++"/"++(show $ V.length $ labels td)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show $ stopIndex alg td td')
          ++ "  --  "++(show $ errorRate classifier_test)
    if ( (obeyStopCriteria param) && (stopCondition alg td td') ) || itr>=iterations param
       then return $ Ensemble $ (replicate (iterations param - length es) (0,snd $ head es))++es
       else trainItrArr alg param (itr+1) seed' td' (Ensemble es')
    
    where 
          -- sample our next distribution
          wds = zip (V.toList $ weights td) (zip (V.toList $ labels td) (VB.toList $ dataPoints td))
          sampledist = sample (mkStdGen seed) (sampleRate param) wds
          f = classify model
          model = (trainer param) sampledist
          
          -- create parameters for next iteration
          es' = (stepSize alg td td',model):es
          td' = td  { weights = weights' 
                    , labels = labels'
                    , _f_vec = V.fromList [f $ _x i | i<-indices alg]
                    , _F_vec = V.fromList [ weightedClassify (Ensemble es') (_x i) | i<-indices alg]
                    }

          _F' = weightedClassify $ Ensemble es'
--           _F  = weightedClassify $ Ensemble es
          classifier_test = genConfusionMatrix (num2bool . _F') [ (_y i,_x i) | i <- indices alg]

          labels' = V.fromList [ newlabel i | i<-indices alg]
          newlabel i = if i<numLabels td
                          then _y i
                          else num2bool $ _F' $ _x i

          weights' = normalizeVector $ V.fromList [ newWeight alg td td' i | i<-indices alg]
          
          -- shortcuts
          _D i = weights td V.! i
          _y i = labels td V.! i
--           _y' i = labels td' V.! i
          _x i = dataPoints td VB.! i
          
-- normalizeVector :: (Fractional a, V.Storable a) => V.Vector a -> V.Vector a
normalizeVector :: (Fractional a, V.Unbox a) => V.Vector a -> V.Vector a
normalizeVector v = V.map (\x -> x/normFactor) v
    where
        normFactor = V.sum v
        
genModelFromDistribution rseed param weightsVec labelsVec dataVec = model
    where
        wds = zip (V.toList $ weightsVec) (zip (V.toList $ labelsVec) (VB.toList $ dataVec))
        sampledist = sample (mkStdGen rseed) (sampleRate param) wds
        model = (trainer param) sampledist
