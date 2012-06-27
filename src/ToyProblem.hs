{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction #-}
--
module Main
    where

import ToyLibrary

import qualified Data.Number.LogFloat as LF


cost' z = -2*(exp $ -2*z)/(log $ 1+(exp $ -2*z))

costLF z' = LF.fromLogFloat $ -2*(exp $ -2*z)/(log $ 1+(exp $ -2*z))
    where
        z = LF.logFloat (z' ::Double)
{-

This problem is an experiment in type classes

-}


wontcompile :: String
wontcompile = modelName dTree

-- wontcompile2 :: (ClassifyModel model label) => model -> TrainingData label -> model
-- wontcompile2 model td = train model td
wontcompile2 = train dTree [("string",[])]

willcompile model datapoint = classify model datapoint