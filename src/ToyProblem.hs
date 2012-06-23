{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction #-}
--
module Main
    where

import ToyLibrary

{-

This problem is an experiment in type classes

-}


wontcompile :: String
wontcompile = modelName dTree

-- wontcompile2 :: (ClassifyModel model label) => model -> TrainingData label -> model
-- wontcompile2 model td = train model td
wontcompile2 = train dTree [("string",[])]

willcompile model datapoint = classify model datapoint