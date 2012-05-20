module Classification 
    where
      
import Control.Monad.Writer
import Database.HDBC

import qualified Data.Map as Map

-- data types
      
type Label = String
type Prob = Double

instance Ord SqlValue where
    compare (SqlDouble a) (SqlDouble b) = compare a b
    compare (SqlString a) (SqlString b) = compare a b
    compare (SqlDouble a) (SqlString b) = LT
    compare (SqlString a) (SqlDouble b) = GT

type DataPoint = [SqlValue]
type TrainingData = [(Label,DataPoint)]

type Trainer a b = [(a,DataPoint)] -> b
type Classifier a b = b -> DataPoint -> a

type BoolClassifier b = b -> [SqlValue] -> Bool

-- Binary functions

toBinaryData :: Label -> TrainingData -> [(Bool,DataPoint)]
toBinaryData l ds = map (\(l',dp) -> (l'==l,dp)) ds

toBinaryClassifier :: (Eq a) => a -> Classifier a b -> BoolClassifier b
toBinaryClassifier label classifier = \model -> \dp -> classifier model dp == label
                                                          
-- toBinaryClassifier model l classifier = \x -> (classifier model x)==l
                                           
-- toBinaryTrainer :: [(Label,[SqlValue])] -> NBayes
     
bool2num :: (Num a) => Bool -> a
bool2num b = if b
                 then 1
                 else -1
                 
num2bool :: (Num a,Ord a) => a -> Bool
num2bool n = if n>0
                then True
                else False
     
indicator :: (Num a) => Bool -> a
indicator bool = if bool
                    then 1
                    else 0
     
-- Performance measuring

data PerformanceDesc a = PD (Map.Map a (Int,Int))
    deriving Show

eval :: (Ord a) => ([SqlValue] -> a) -> [(a,[SqlValue])] -> PerformanceDesc a
eval c ds = PD $ foldl (Map.unionWith uFunc) Map.empty $ map (\(l,dp) -> Map.singleton l $ func $l==c dp) ds
    where
          uFunc (a1,b1) (a2,b2) = (a1+a2,b1+b2)
          func True = (1,0)
          func False= (0,1)

errorRate :: PerformanceDesc a -> Double
errorRate (PD xs) = err $ foldl add (0,0) $ map (\(l,(t,f))->(t,f)) $ Map.toList xs
    where add (t1,f1) (t2,f2) = (t1+t2,f1+f2)
          err (t,f) = (fromIntegral f)/(fromIntegral $ t+f)

-- Logging

type LogAI a = Writer [String] a

logAI str = tell [str]