module AI.Classification.Boosting.ASSEMBLE
    where

import AI.Classification.Boosting

---------------------------------------
-- ASSEMBLE

data ASSEMBLE = ASSEMBLE

instance BoostAlg ASSEMBLE where
    boostAlgName _ = "ASSEMBLE"
    boostUseUnlabeledDataPoints _ = True
