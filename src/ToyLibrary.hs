{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, NoMonomorphismRestriction #-}

module ToyLibrary
    where

data DataItem = Discrete String | Continuous Double | Missing
type DataPoint = [DataItem]
type TrainingData labelType = [(labelType,DataPoint)]

data DecisionTree label = DecisionTree
    { maxDepth :: Int
    -- more stuff that's not relevant
    }
    deriving Show

class ClassifyModel model label | model -> label where 
    modelName :: model -> String
    train :: model -> TrainingData label -> model
    classify :: model -> DataPoint -> label
    
instance ClassifyModel (DecisionTree label) label where
    modelName dtree = "DecisionTree-"++(show $ maxDepth dtree)
    train = undefined
    classify = undefined
    
dTree = DecisionTree 1 -- :: DecisionTree String
