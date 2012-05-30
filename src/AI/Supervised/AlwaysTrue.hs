module AI.Supervised.AlwaysTrue
    where

import Data.List
import Data.List.Extras.Argmax
import qualified Data.Map as Map
import Database.HDBC
import Debug.Trace

import AI.Classification

-- Data types

data ATrue = ATrue
    deriving Show

-- Training

train :: [(a,[SqlValue])] -> ATrue
train xs = ATrue

-- classification

classify :: ATrue -> [SqlValue] -> Bool
classify nb sqlL = False
