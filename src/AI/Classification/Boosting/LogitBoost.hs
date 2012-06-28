module AI.Classification.Boosting.LogitBoost
    where
          
import AI.Classification
import AI.Classification.Boosting

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

---------------------------------------
-- LogitBoost

data LogitBoost = LogitBoost

instance BoostAlg LogitBoost where
    boostAlgName _ = "LogitBoost"
    boostUseUnlabeledDataPoints _ = False

    indices tv = [0..(numLabels tv)-1]

    boostNewWeight tv' i = (cost' $ margin i)
        where
            cost  z = log $ 1+(exp $ -2*z)
--             cost' z = {-trace ("z="++show z) $ -}-2*(exp $ -2*z)/(log $ 1+(exp $ -2*z))
            cost' z = -2*(exp $ -2*z)/(1+(exp $ -2*z))
            
            margin i = (bool2num $ _y' i)*(_F' i)

            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i
