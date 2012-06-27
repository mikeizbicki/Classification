{-# LANGUAGE ExistentialQuantification,MultiParamTypeClasses,FunctionalDependencies,FlexibleContexts, FlexibleInstances #-}

module AI.Classification.Boosting
--     ( train
--     , classify
--     , mkBoost
-- --     , module AI.Classification.BoostingAlgs
--     )
    where

import AI.Classification
import AI.RandomUtils
import AI.Classification.KNN

import Control.Monad
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

data BoostParams model alg = {-forall alg . (BoostAlg alg) => -}BoostParams 
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
weightedClassify (Ensemble params xs) dp = sum $ [step * (bool2num $ classify model dp) | (step,model) <- xs]

-------------------------------------------------------------------------------

data EnsembleContainer = forall model alg . (ClassifyModel model Bool, BoostAlg alg)=> 
        EC { fetchEnsemble :: (Ensemble model alg) }
        
instance Show EnsembleContainer where
    show ec = modelName ec

instance ClassifyModel EnsembleContainer Bool where
    modelName (EC ens) = modelName ens
    train (EC ens) ls us = liftM EC $ train ens ls us
    probClassify (EC ens) = probClassify ens

mkBoost :: (ClassifyModel model Bool,BoostAlg alg)=>BoostParams model alg -> EnsembleContainer
mkBoost params = EC $ Ensemble
    { boostParams = params
    , funcL = []
    }

-------------------------------------------------------------------------------
-- The BoostAlg class follows the code for ASSEMBLE, since it is the most general

class BoostAlg alg where
    boostAlgName :: alg -> String
    
    indices :: TrainingVec Bool model alg -> [Int]
    indices tv = [0..(V.length $ labels tv)-1]
    
    boostErr :: TrainingVec Bool model alg -> Double
    boostErr tv = sum [ (indicator $ _y i /= _f i)*(_D i) | i <- indices tv ]
        where
            _f i = (_f_vec tv) V.! i
            _x i = (dataPoints tv) VB.! i
            _y i = (labels tv) V.! i
            _D i = (weights tv) V.! i
            
    boostStepSize :: TrainingVec Bool model alg -> Double
    boostStepSize tv = (1/2) * (log $ (1-err)/(err+0.00001))
        where
            err = boostErr tv
            
    boostNewWeight :: TrainingVec Bool model alg -> Int -> Double
    boostNewWeight tv' i = (alpha tv' i)*(cost' $ margin i)
        where
            cost  z =  (exp $ -z)
            cost' z = -(exp $ -z)
            
            alpha tv i =
                if i<numLabels tv
                   then 1
                   else 0.05
            
            margin i = (bool2num $ _y' i)*(_F' i)

            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i

---------------------------------------
-- ASSEMBLE

data ASSEMBLE = ASSEMBLE

instance BoostAlg ASSEMBLE where
    boostAlgName _ = "ASSEMBLE"

---------------------------------------
-- ASSEMBLE.LogitBoost

data LogitASSEMBLE = LogitASSEMBLE

instance BoostAlg LogitASSEMBLE where
    boostAlgName _ = "ASSEMBLE.LogitBoost"

    boostNewWeight tv' i = (alpha tv' i)*(cost' $ margin i)
        where
            cost  z = log $ 1+(exp $ -z)
            cost' z = {-trace ("z="++show z) $ -}-2*(exp $ -2*z)/(log $ 1+(exp $ -2*z))
            
            
            alpha tv i =
                if i<numLabels tv
                   then 1
                   else 0.05

            margin i = 
                if m>10
                   then {-trace ("m="++show m) -}10
                   else m
                where m=(bool2num $ _y' i)*(_F' i)

            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i

---------------------------------------
-- EntropyBoost (See "Information Theoretic Regularization for Semi-Supervised Boosting," Zheng, Wang, Liu.

data EntropyBoost = EntropyBoost

instance BoostAlg EntropyBoost where
    boostAlgName _ = "EntropyBoost"

--     boostErr tv = sum [ (indicator $ _y i /= _f i)*(_D i) | i <- indices tv ]
--         where
--             _f i = (_f_vec tv) V.! i
--             _x i = (dataPoints tv) VB.! i
--             _y i = (labels tv) V.! i
--             _D i = (weights tv) V.! i


    boostNewWeight tv' i =
        if i<(numLabels tv')
           then (cost_label' $ margin i)
           else gamma*(sum [(cost_unlabel' $ (bool2num y)*(margin_F' i)) | y <- [True, False]])
           
        where
            
            fart z more = if (log $ 1+(exp $ -z))==0
                                then error $ "poop z="++show z
                                else more
            
            cost_label  z = log $ 1+(exp $ -z)
            cost_label' z = -(exp $ -z)/(log $ 1+(exp $ -z))
            
            cost_unlabel  z = (log $ 1+(exp $ -z))/(1+(exp $ -z))
--             cost_unlabel' z = (-1 + (log $ 1+(exp $ -z)))*(exp $ -z)/(log $ 1+(exp $ -z))^2
            cost_unlabel' z = (-1 + (log $ 1+(exp $ -z)))/(1+(exp $ -z)) * (cost_label' z)
            
--             margin i = (bool2num $ _y' i)*(_F' i)
            margin i = (bool2num $ _y' i)*(margin_F' i)
            margin_F' i = 
                if (_F' i)>10
                   then 10
                   else _F' i

            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i
            
            gamma = 0.001

---------------------------------------
-- RegularizedBoost

data RegularizedBoost = RegularizedBoost

instance BoostAlg RegularizedBoost where
    boostAlgName _ = "RegularizedBoost"

    boostErr tv = trace ("left="++show left++" >> right="++show right) left+right

        where
            left =   sum [ (indicator $ _y i /= _f i)*(_D i) | i <- indices tv ]
            right= -(sum [ (indicator $ _y i == _f i)*(betaR tv i) | i <- indices tv ]
                    /sum [ (cost' $ margin i) - (betaR tv i)| i<- indices tv]
                    )
            _F i = (_F_vec tv) V.! i
            _f i = (_f_vec tv) V.! i
            _x i = (dataPoints tv) VB.! i
            _y i = (labels tv) V.! i
            _D i = (weights tv) V.! i
            
            cost  z =  (exp $ -z)
            cost' z = -(exp $ -z)
            margin i = (bool2num $ _y i)*(_F i)

    boostNewWeight tv' i = (alpha i)*(cost' $ margin i) - (betaR tv' i)
        where
            alpha i = 1
            margin i = (bool2num $ _y' i)*(_F' i)
            
            cost  z =  (exp $ -z)
            cost' z = -(exp $ -z)
            
            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i

betaR tv i = (beta i)*(_R i)
    where
        beta i = 0.5
        _R i = sum [(similarity (_x i) (_x j))*(cost_tilde $ (-1)*(compatability i j)) 
                   | j <- filter (i/=) $ indices tv
                   ]
        
        cost  z =  (exp $ -z)
        cost' z = -(exp $ -z)
        cost_tilde z = (cost z)-1
        
        compatability i j = abs $ (bool2num $ _y i)-(bool2num $ _y j)
        
        _F i = (_F_vec tv) V.! i
        _y i = (labels tv) V.! i
        _x i = (dataPoints tv) VB.! i
        
similarity :: DataPoint -> DataPoint -> Double
similarity dp1 dp2 = {-trace (" << diff="++show diff++" << res="++show res)-} res
    where res = exp $ (-1 * ({-sqrt $ -}diff) / (2*sigma^2))
          sigma = 5
          diff = sum [ (dataminus x1 x2)^2 | (x1,x2) <- zip dp1 dp2] :: Double
          
          
dataminus :: DataItem -> DataItem -> Double
dataminus (Continuous x1) (Continuous x2) = x1 - x2
dataminus (Discrete x1) (Discrete x2) = 
    if (x1==x2)
       then 0
       else 1

---------------------------------------
-- AdaBoost

data AdaBoost = AdaBoost

instance BoostAlg AdaBoost where
    boostAlgName _ = "AdaBoost"
    indices tv = [0..(numLabels tv)-1]

---------------------------------------
-- LogitBoost

data LogitBoost = LogitBoost

instance BoostAlg LogitBoost where
    boostAlgName _ = "LogitBoost"

    indices tv = [0..(numLabels tv)-1]

    boostNewWeight tv' i = (cost' $ margin i)
        where
            cost  z = log $ 1+(exp $ -2*z)
            cost' z = {-trace ("z="++show z) $ -}-2*(exp $ -2*z)/(log $ 1+(exp $ -2*z))
            
            margin i = 
                if m>10
                   then {-trace ("m="++show m) -}10
                   else m
                where m=(bool2num $ _y' i)*(_F' i)

            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i

-------------------------------------------------------------------------------

trainBoost :: (BoostAlg alg, ClassifyModel model Bool)=>Ensemble model alg -> TrainingData Bool -> [DataPoint] -> LogAI (Ensemble model alg)
trainBoost ens ls us = do
    logAI "print" $ "train."++(modelName $ ens)
    let params = boostParams ens
    
    let dataVec = VB.fromList $ (map snd ls)++us
    let _x i = dataVec VB.! i

    let _D1 i = if isLabeled i
                    then beta/(fromIntegral $ length ls)
                    else (1-beta)/(fromIntegral $ length us)
    let weightsVec = normalizeVector $ V.fromList [ _D1 i | i<-indexAll ]
    
--     let baseModel = defKNN {k=1} :: KNN Bool
    let baseModel = modelConf params
    model0 <- train (baseModel) ls []
    let _y1 i = if isLabeled i
                    then fst $ ls !! i
                    else classify model0 $ _x i
    let labelsVec = V.fromList [ _y1 i | i<-indexAll ]
    
    nextrand <- getRandom
    model1 <- genModelFromDistribution nextrand params $ TrainingVec 
        { weights=weightsVec
        , labels=labelsVec
        , dataPoints=dataVec
        , numLabels=length indexLabeled
        , _F_vec = undefined
        , _f_vec = undefined
        , curItr = undefined
        , lastModel = undefined
        , lastEnsemble = undefined
        }
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
        }
        
    where
        indexAll=indexLabeled++indexUnlabeled
        indexLabeled=[0..(length ls)-1]
        indexUnlabeled=[(length ls)..(length ls)+(length us)-1]
        beta=0.9
        
        isLabeled i = i<length ls
        
    
trainItr :: (BoostAlg alg, ClassifyModel model Bool)=>BoostParams model alg -> TrainingVec Bool model alg -> LogAI (Ensemble model alg)
trainItr params tv = do
    let stepSize = boostStepSize tv
    let ensemble' = Ensemble params $ (stepSize,lastModel tv):(funcL $ lastEnsemble tv)
    
    let _F'generic = weightedClassify ensemble'
    let _F' i = _F'generic $ _x i
        
    let _y' i = if isLabeled i
                    then _y i
                    else num2bool $ _F' i
    let labelsVec = V.fromList [ _y' i | i <- indexAll]
                    
    let tv' = tv { labels = labelsVec
                 , _F_vec = V.fromList [ _F' i | i <- indexAll]
                 }
{-    let www1=V.fromList [boostNewWeight tv' i | i<- indexAll]
    let www2=trace ("\n\n"++show www1) $ normalizeVector www1
    let weightsVec = trace ("\n\n"++show www2) www2-}
    let weightsVec = normalizeVector $ V.fromList [boostNewWeight tv' i | i<- indexAll]
    
    nextrand <- getRandom
    model' <- genModelFromDistribution nextrand params (tv {weights=weightsVec,labels=labelsVec})
--     let model' = genModelFromDistribution (curItr tv) params weightsVec labelsVec (dataPoints tv)
    let _f' i = classify model' $ _x i
    
    -----------------------------------
    -- debug messages
    
    let classifier_test = genConfusionMatrix (num2bool . _F'generic) [ (_y i,_x i) | i <- indexAll]
        
    logAI "print" $ "trainItr."++(modelName $ lastEnsemble $ tv)
          ++ ": " ++ (show $ curItr tv)
          ++ "  --  l/u="++(show $ length indexLabeled)++"/"++(show $ length $ indexAll)
          ++ "  --  "++(show $ classifier_test)
          ++ "  --  "++(show $ stepSize)
--           ++ "  --  "++(show $ boostErr tv)
          ++ "  --  "++(show $ errorRate classifier_test)


    let aveMargin = (sum [ (bool2num $ _y i)*(_F i) | i<-indices tv])/(fromIntegral $ length $ indices tv)
    logAI "aveMargin" $ show $ aveMargin

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
        _F i = (_F_vec tv) V.! i
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
        wds = zip (V.toList $ weights $ tv) (zip (V.toList $ labels $ tv) (VB.toList $ dataPoints $ tv))
        sampledist = sample (mkStdGen rseed) numSamples wds
        model = train (modelConf param) sampledist []
        
        numSamples = case (sampleRate param) of
                          Absolute x -> x
                          Relative x -> floor $ x*(fromIntegral $ (V.length $ labels tv))
                          Auto -> numLabels tv
                          
normalizeVector :: (Fractional a, Ord a, V.Unbox a) => V.Vector a -> V.Vector a
normalizeVector v = {-V.map resize $ -}V.map (\x -> x/normFactor) v
    where
        normFactor = V.sum v
--         resize x = 
--             if x < 0.00001
--                then 0.00001
--                else x
