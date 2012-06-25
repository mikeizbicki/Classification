{-# LANGUAGE MultiParamTypeClasses,FlexibleContexts, FlexibleInstances #-}

module AI.ASSEMBLE2
    where

import AI.Classification
import AI.Ensemble
import AI.RandomUtils

import AI.Supervised.KNN

import Data.Monoid
import Debug.Trace

import System.Random
import Control.Monad.Random

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

-------------------------------------------------------------------------------

data BoostParams model = BoostParams 
    { iterations :: Int 
    , sampleRate :: DataRate
    , obeyStopCriteria :: Bool
    , modelConf :: model
    }

data TrainingVec label model = TrainingVec 
    { weights :: V.Vector Double
    , labels :: V.Vector label
    , dataPoints :: VB.Vector DataPoint
    , _f_vec :: V.Vector Bool
    , _F_vec :: V.Vector Double
    , alpha :: Int -> Double
    , numLabels :: Int
    , curItr :: Int
    , lastModel :: model
    , lastEnsemble :: Ensemble model
    }

data Ensemble model = Ensemble 
    { boostParams :: BoostParams model
    , funcL :: ![(Double,model)]
    }

instance (Show b) => Show (Ensemble b) where
    show (Ensemble params xs) = show $ map (\(a,b)->(a)) xs


-- classification

weightedClassify :: (ClassifyModel a Bool) => (Ensemble a) -> DataPoint -> Double
weightedClassify (Ensemble params xs) dp = (sum $ [alpha * (bool2num $ classify model dp) | (alpha,model) <- xs])


-------------------------------------------------------------------------------

instance (ClassifyModel basemodel Bool) => ClassifyModel (Ensemble basemodel) Bool where
    modelName _ = "Ensemble"
    train ens td ud = trainBoost ens td ud
    classify ens dp = num2bool $ weightedClassify ens dp    
    probClassify = error "Ensemble.probClassify not yet implemented"

-------------------------------------------------------------------------------

trainBoost :: (ClassifyModel model Bool)=>Ensemble model -> TrainingData Bool -> [DataPoint] -> LogAI (Ensemble model)
trainBoost ens ls us = do
    logAI "ASSEMBLE.train"
    let params = boostParams ens
    
    let dataVec = VB.fromList $ (map snd ls)++us
    let _x i = dataVec VB.! i

    let _D1 i = if isLabeled i
                    then beta/(fromIntegral $ length ls)
                    else (1-beta)/(fromIntegral $ length us)
    let weightsVec = normalizeVector $ V.fromList [ _D1 i | i<-indexAll ]
    
    let baseModel = defKNN {k=1} :: KNN Bool
--     model0 <- test-- KNN.train 1 ls
    model0 <- train (baseModel) ls []
    let _y1 i = if isLabeled i
                    then fst $ ls !! i
                    else classify model0 $ _x i
    let labelsVec = V.fromList [ _y1 i | i<-indexAll ]
    
    nextrand <- getRandom
    model1 <- genModelFromDistribution nextrand params $ TrainingVec { weights=weightsVec, labels=labelsVec, dataPoints=dataVec, numLabels=length indexLabeled}
    let _f i = classify model1 $ _x i
    
    trainItr params $ TrainingVec
        { weights = weightsVec
        , labels = labelsVec
        , dataPoints = dataVec
        , _f_vec = V.fromList [ _f i | i <- indexAll]
        , _F_vec = V.fromList [ 0 | i <- indexAll]
        , numLabels = length indexLabeled
        , curItr = 1
        , lastModel = model1
        , lastEnsemble = Ensemble params []
        , alpha = _D1
        }
        
    where
        indexAll=indexLabeled++indexUnlabeled
        indexLabeled=[0..(length ls)-1]
        indexUnlabeled=[(length ls)..(length ls)+(length us)-1]
        beta=0.9
        
        isLabeled i = i<length ls
        
    
trainItr :: (ClassifyModel model Bool)=>BoostParams model -> TrainingVec Bool model -> LogAI (Ensemble model)
trainItr params tv = do
    let _y_hat i = _f i
        
    let err = {-trace (show $ V.take 10 $ weights tv) $ -}sum [ (indicator $ _y i /= _y_hat i)*(_D i) | i <- indexAll ]
    
    let stepSize = (1/2) * (log $ (1-err)/(err+0.00001))
        
    let ensemble' = Ensemble params $ (stepSize,lastModel tv):(funcL $ lastEnsemble tv)
    
    let _F'generic = weightedClassify ensemble'
    let _F' i = _F'generic $ _x i
        
    let _y' i = if isLabeled i
                    then _y i
                    else num2bool $ _F' i
    let labelsVec = V.fromList [ _y' i | i <- indexAll]
                    
    let margin i = (bool2num $ _y' i)*(_F' i)
                    
    let _D' i = (alpha tv i)*(cost' $ margin i)
--     let _D' i = (_D i)*(exp $ -stepSize * (bool2num $ _y' i==_f i))
    let weightsVec = normalizeVector $ V.fromList [_D' i | i<- indexAll]
    
    nextrand <- getRandom
    model' <- genModelFromDistribution nextrand params (tv {weights=weightsVec,labels=labelsVec})
--     let model' = genModelFromDistribution (curItr tv) params weightsVec labelsVec (dataPoints tv)
    let _f' i = classify model' $ _x i
    
    -----------------------------------
    -- debug messages
    
    let classifier_test = genConfusionMatrix (num2bool . _F'generic) [ (_y i,_x i) | i <- indexAll]
        
    logAI $ "ASSEMBLE.trainItr: " ++ (show $ curItr tv)
          ++ "  --  l/u="++(show $ length indexLabeled)++"/"++(show $ length $ indexAll)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show $ stepSize)
          ++ "  --  "++(show $ errorRate classifier_test)


    -----------------------------------
    -- recurse
    if curItr tv < iterations params
        then trainItr params $ tv 
                { weights=weightsVec
                , labels = labelsVec
                , curItr = (curItr tv)+1
                , _f_vec = V.fromList [ _f' i | i <- indexAll]
                , _F_vec = V.fromList [ _F' i | i <- indexAll]
                , lastModel = model'
                , lastEnsemble = ensemble'
                }
        else return $ ensemble'
    
    where
        cost  z =  exp (-z)
        cost' z = -exp (-z)
          
        _f i = (_f_vec tv) V.! i
        _x i = (dataPoints tv) VB.! i
        _y i = (labels tv) V.! i
        _D i = (weights tv) V.! i

        indexAll=indexLabeled++indexUnlabeled
        indexLabeled=[0..(numLabels tv)-1]
        indexUnlabeled=[(numLabels tv)..(VB.length $ dataPoints tv)-1]

        isLabeled i = i<length indexLabeled


-------------------------------------------------------------------------------

genModelFromDistribution rseed param tv = model
    where
        wds = zip (V.toList $ weights $ tv) (zip (V.toList $ labels $ tv) (VB.toList $ dataPoints $ tv))
        sampledist = sample (mkStdGen rseed) numSamples wds
        model = train (modelConf param) sampledist []
        
        numSamples = case (sampleRate param) of
                          Absolute x -> x
                          Auto -> numLabels tv
                          
                          
normalizeVector :: (Fractional a, V.Unbox a) => V.Vector a -> V.Vector a
normalizeVector v = V.map (\x -> x/normFactor) v
    where
        normFactor = V.sum v
