module AI.Classification.Boosting.AdaBoost
    where

import AI.Classification.Boosting

---------------------------------------
-- AdaBoost

data AdaBoost = AdaBoost

instance BoostAlg AdaBoost where
    boostAlgName _ = "AdaBoost"
    boostUseUnlabeledDataPoints _ = False
