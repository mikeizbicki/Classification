module AI.Supervised.KNN
    where

import Data.List.Extras
import Database.HDBC

import AI.Classification

-- Data types

data KNN a = KNN

-- Training

train :: (Ord a) => [(a,[SqlValue])] -> KNN a
train xs = KNN

-- classification

classify :: (Ord a) => KNN a -> [SqlValue] -> a
classify nb sqlL = fst $ argmaxBy compare snd $ probList nb sqlL

probList :: (Ord a) => KNN a -> [SqlValue] -> [(a,Prob)]
probList (KNN) sqlL = []
    
----

dataset = [("male",[6.0,180,12])
          ,("male",[5.92,190,11])
          ,("male",[5.58,170,12])
          ,("male",[5.92,165,10])
          ,("male",[5.5,130,7])
          ,("female",[5.5,150,8])
          ,("female",[5.42,130,7])
          ,("female",[5.75,150,9])
          ]

dataset' = [("male",[6.0,180,12])
          ]

sqldata = map (\(label,xs) -> (label,map (toSql::Double->SqlValue) xs)) dataset
bdataset = toBinaryData "male" sqldata

trained = train sqldata

-- sample = [toSql (5::Double), toSql (130::Double), toSql (8::Double)]