{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts, FlexibleInstances #-}

module AI.Classification.SemiBoost
--     ( train
--     , classify
--     , mkBoost
-- --     , module AI.Classification.BoostingAlgs
--     )
    where

import AI.Classification
import AI.RandomUtils
import AI.Classification.KNN

import Debug.Trace
import System.Random
import Control.Monad.Random

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

-------------------------------------------------------------------------------
-- storage data

data TrainingVec label model alg = TrainingVec 
    { weights :: V.Vector Double
    , labels :: V.Vector label
    , dataPoints :: VB.Vector DataPoint
    , _f_vec :: V.Vector Bool
    , _F_vec :: V.Vector Double
    , numLabels :: Int
    , curItr :: Int
    , lastModel :: model
    , lastEnsemble :: Ensemble model alg
    }

data BoostParams model alg = BoostParams 
    { iterations :: Int 
    , sampleRate :: DataRate
    , obeyStopCriteria :: Bool
    , modelConf :: model
    , boostAlg :: alg
    }

-------------------------------------------------------------------------------
-- Ensemble

data Ensemble model alg = Ensemble 
    { boostParams :: BoostParams model alg
    , funcL :: ![(Double,model)]
    }

instance (Show model) => Show (Ensemble model alg) where
    show (Ensemble params xs) = show $ map (\(a,b)->(a)) xs

instance (BoostAlg alg, ClassifyModel basemodel Bool) => ClassifyModel (Ensemble basemodel alg) Bool where
    modelName ens = (boostAlgName $ boostAlg $ boostParams $ ens)++"."++(modelName $ modelConf $ boostParams $ ens)
    train ens td ud = trainBoost ens td ud
    classify ens dp = num2bool $ weightedClassify ens dp    
    probClassify = error "Ensemble.probClassify not yet implemented"

weightedClassify :: (ClassifyModel model Bool) => (Ensemble model alg) -> DataPoint -> Double
weightedClassify (Ensemble params xs) dp = (sum $ [alpha * (bool2num $ classify model dp) | (alpha,model) <- xs])

mkBoost :: BoostParams model alg -> Ensemble model alg
mkBoost params = Ensemble
    { boostParams = params
    , funcL = []
    }

-------------------------------------------------------------------------------

class BoostAlg alg where
    boostAlgName :: alg -> String

data SemiBoost = SemiBoost

instance BoostAlg SemiBoost where
    boostAlgName _ = "SemiBoost"

similarity :: DataPoint -> DataPoint -> Double
similarity dp1 dp2 = {-trace (" << diff="++show diff++" << res="++show res)-} res
    where res = exp $ (-1 * (diff) / (2*sigma^2))
          sigma = 2
          diff = sum [ (dataminus x1 x2)^2 | (x1,x2) <- zip dp1 dp2] :: Double
          
dataminus :: DataItem -> DataItem -> Double
dataminus (Continuous x1) (Continuous x2) = x1 - x2
dataminus (Discrete x1) (Discrete x2) = 
    if (x1==x2)
       then 0
       else 1


-------------------------------------------------------------------------------

trainBoost :: (BoostAlg alg, ClassifyModel model Bool)=>Ensemble model alg -> TrainingData Bool -> [DataPoint] -> LogAI (Ensemble model alg)
trainBoost ens ls us = do
    logAI $ "train."++(modelName $ ens)
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
        { weights = undefined -- weightsVec
        , labels = labelsVec
        , dataPoints = dataVec
        , _f_vec = undefined -- V.fromList [ _f i | i <- indexAll]
        , _F_vec = V.fromList [ 0 | i <- indexAll]
        , numLabels = length indexLabeled
        , curItr = 1
        , lastModel = undefined -- model1
        , lastEnsemble = Ensemble params []
        }
        
    where
        indexAll=indexLabeled++indexUnlabeled
        indexLabeled=[0..(length ls)-1]
        indexUnlabeled=[(length ls)..(length ls)+(length us)-1]
        beta=0.9
        
        isLabeled i = i<length ls
        
    
trainItr :: (BoostAlg alg, ClassifyModel model Bool)=>BoostParams model alg -> TrainingVec Bool model alg -> LogAI (Ensemble model alg)
trainItr params tv = do
    let c = (fromIntegral $ length indexLabeled)/(fromIntegral $ length $ indexUnlabeled)
    
    let p i = sum [ (similarity (_x i) (_x j))*(exp $ -2*(_F i))*(indicator $       _y j) | j<-indexLabeled]
            + sum [ (c/2)*(similarity (_x i) (_x j))*(exp $ (_F j) - (_F i)) | j <- indexAll]
    
    let q i = sum [ (similarity (_x i) (_x j))*(exp $  2*(_F i))*(indicator $ not $ _y j) | j<-indexLabeled]
            + sum [ (c/2)*(similarity (_x i) (_x j))*(exp $ (_F i) - (_F j)) | j <- indexAll]
    
    let _y' i = if isLabeled i
                    then _y i
                    else num2bool $ (p i) - (q i)
    let labelsVec = V.fromList [ _y' i | i <- indexAll]
                    
    let weightsVec = normalizeVector $ V.fromList [abs $ (p i) - (q i) | i<- indexAll]

    nextrand <- getRandom
    model' <- genModelFromDistribution nextrand params (tv {weights=weightsVec,labels=labelsVec})
    let _f' i = classify model' $ _x i

    let stepSize = 
            (1/4)* (log $ (sum [ (p i)*(indicator $ (_f' i)==True ) + (q i)*(indicator $ (_f' i)==False) | i <- indexAll])
                        / (sum [ (p i)*(indicator $ (_f' i)==False) + (q i)*(indicator $ (_f' i)==True ) | i <- indexAll])
                   )

    let ensemble' = Ensemble params $ (stepSize,model'):(funcL $ lastEnsemble tv)

    -----------------------------------
    -- debug messages
    
    let _F'generic = weightedClassify ensemble'
    let _F' i = _F'generic $ _x i
    let classifier_test = genConfusionMatrix (num2bool . _F'generic) [ (_y i,_x i) | i <- indexAll]
        
    logAI $ "trainItr."++(modelName $ lastEnsemble $ tv)
          ++ ": " ++ (show $ curItr tv)
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
          
        _F i = (_F_vec tv) V.! i
        _f i = (_f_vec tv) V.! i
        _x i = (dataPoints tv) VB.! i
        _y i = (labels tv) V.! i
        _D i = (weights tv) V.! i

        indexAll=indexLabeled++indexUnlabeled
        indexLabeled=[0..(numLabels tv)-1]
        indexUnlabeled=[(numLabels tv)..(VB.length $ dataPoints tv)-1]

        isLabeled i = i<length indexLabeled


-------------------------------------------------------------------------------

genModelFromDistribution :: (ClassifyModel model label, Eq label, Show label, V.Unbox label)=>
    Int -> BoostParams model alg -> TrainingVec label model alg -> LogAI model
genModelFromDistribution rseed param tv = model
    where
        ls = take (numLabels tv) $ zip (V.toList $ labels tv) (VB.toList $ dataPoints $ tv)
        wds = {-drop (numLabels tv) $ -}zip (V.toList $ weights $ tv) (zip (V.toList $ labels $ tv) (VB.toList $ dataPoints $ tv))
        sampledist = sample (mkStdGen rseed) numSamples wds
                
        model = train (modelConf param) ({-ls++-}sampledist) []
        
        numSamples = case (sampleRate param) of
                          Absolute x -> x
                          Relative x -> floor $ x*(fromIntegral $ (V.length $ labels tv){--(numLabels tv)-})
                          Auto -> numLabels tv
                          
                          
normalizeVector :: (Fractional a, V.Unbox a) => V.Vector a -> V.Vector a
normalizeVector v = V.map (\x -> x/normFactor) v
    where
        normFactor = V.sum v
