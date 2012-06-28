{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds, RankNTypes, FlexibleInstances, FlexibleContexts, TypeSynonymInstances #-}

module AI.Classification 
    where
      
import Control.Monad.Writer
import Control.Monad.Random
import Control.Monad.State
import System.Random
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- classifiers

class ClassifyModel model label | model -> label where
    modelName :: model -> String
    
    train :: model -> TrainingData label -> UnlabeledData -> LogAI model
--     train m ls us = liftM snd $ trainVerbose m ls us
--     trainVerbose :: model -> TrainingData label -> UnlabeledData -> LogAI (verbose,model)

    probClassify :: model -> DataPoint -> [(label,Prob)]
    
    classify :: model -> DataPoint -> label
    classify model dp = fst $ argmaxBy compare snd $ probClassify model dp

data ClassifyContainer label = forall model . (ClassifyModel model label) =>
        CC { fetchClassifyModel :: model }

instance ClassifyModel (ClassifyContainer label) label {-| (ClassifyContainer label) -> label-} where
    modelName (CC model) = modelName model
    train (CC model) td ud = liftM CC $ train model td ud
    probClassify (CC model) = probClassify model

-------------------------------------------------------------------------------
-- data types
      
data DataItem = Discrete String
              | Continuous Double
              | Missing
    deriving (Show,Eq)

instance Ord DataItem where
    compare (Continuous a) (Continuous b) = compare a b
    compare (Discrete a)   (Discrete b)   = compare a b
    compare (Continuous a) (Discrete b) = LT
    compare (Discrete a)   (Continuous b) = GT
    compare _ Missing = LT
    compare Missing _ = GT

class DataItemConverter di where
    fromDataItem :: DataItem -> di
    toDataItem :: di -> DataItem
    
instance DataItemConverter String where
    fromDataItem (Discrete di) = di
    fromDataItem x = error $ "DIC.String: non-exhaustive patterns: "++show x
    toDataItem di = Discrete di
    
instance DataItemConverter Double where
    fromDataItem (Continuous di) = di
    fromDataItem x = error $ "DIC.Double: non-exhaustive patterns: "++show x
    toDataItem di = Continuous di

-- convenience types

data DataRate = Relative Double | Absolute Int | Auto
    deriving Show

type DataPoint = [DataItem]

type Trainer labelType model = [(labelType,DataPoint)] -> model
type Classifier labelType model = model -> DataPoint -> labelType

type Prob = Double

-- TrainingData

type TrainingData labelType = [(labelType,DataPoint)]
type UnlabeledData = [DataPoint]

getAttrType :: TrainingData labelType -> Int -> DataItem
getAttrType [] attrI = Missing -- error "getAttrType: empty TrainingData.  Either your data has Missing values in every row for this column, or something has gone terribly wrong."
getAttrType td attrI = 
    case attr of
         Missing -> getAttrType (tail td) attrI
         otherwise -> attr
    where 
        attr=(snd $ head td)!!attrI 

-- Binary functions

toBinaryData :: (Eq labelType) => labelType -> TrainingData labelType -> TrainingData Bool
toBinaryData l ds = fmap (\(l',dp) -> (l'==l,dp)) ds

toBinaryClassifier :: (Eq labelType) => labelType -> Classifier labelType model -> Classifier Bool model
toBinaryClassifier label classifier = \model -> \dp -> classifier model dp == label
                                                          
-------------------------------------------------------------------------------
-- Binary/Integer conversion utilities
     
bool2num :: (Num a) => Bool -> a
bool2num b = 
    if b
        then 1
        else -1
                 
num2bool :: (Num a,Ord a) => a -> Bool
num2bool n = 
    if n>0
        then True
        else False
     
indicator :: (Num a) => Bool -> a
indicator bool = 
    if bool
        then 1
        else 0

-------------------------------------------------------------------------------
-- Performance measuring

data ConfusionMatrix a = CM (Map.Map a (Int,Int))
    deriving Show

genConfusionMatrix :: (Ord a) => (DataPoint -> a) -> [(a,DataPoint)] -> ConfusionMatrix a
genConfusionMatrix c ds = CM $ foldl (Map.unionWith uFunc) Map.empty $ map (\(l,dp) -> Map.singleton l $ func $l==c dp) ds
    where
        uFunc (a1,b1) (a2,b2) = (a1+a2,b1+b2)
        func True = (1,0)
        func False= (0,1)

errorRate :: ConfusionMatrix a -> Double
errorRate (CM xs) = err $ foldl add (0,0) $ map (\(l,(t,f))->(t,f)) $ Map.toList xs
    where 
        add (t1,f1) (t2,f2) = (t1+t2,f1+f2)
        err (t,f) = (fromIntegral f)/(fromIntegral $ t+f)

-------------------------------------------------------------------------------
-- Logging

-- type LogAIRand a g = (RandomGen g) => RandT g (Writer [String]) a
-- type LogAI a = RandT StdGen (Writer [String]) a
-- 
-- logAI :: String -> LogAI ()
-- logAI str = tell [str]
-- 
-- runAI :: Int -> LogAI a -> (a,[String])
-- runAI seed m = runWriter $ evalRandT m (mkStdGen seed)




type LogAIRand a g = (RandomGen g) => RandT g (Writer [String]) a
type LogAI a = RandT StdGen (State LogType) a

type LogType = Map.Map String [String]

-- data LogType = LogMap (Map.Map (String,Int) (LogType))
--              | LogString String
--     deriving Show

emptyLogAI = {-LogMap $ -}Map.empty

-- startContext :: String -> LogAI ()
-- startContext context = do
--     history <- get
--     put $ Map.insert context Map.empty history
-- 
-- endContext :: LogAI ()
-- endContext = return ()



logAI :: String -> String -> LogAI ()
logAI key value = do
    history <- get
    put $ Map.insertWith (\x y-> y++x) key [value] history

runAI :: Int -> LogAI a -> (a,LogType)
runAI seed m = runState (evalRandT m (mkStdGen seed)) emptyLogAI

testFunc = do
--     startContext "testFunc"
    logAI "food" "1"
    logAI "food" "2"
    logAI "food" "3"

--     endContext