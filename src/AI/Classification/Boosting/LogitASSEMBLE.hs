module AI.Classification.Boosting.LogitASSEMBLE
    where
          
import AI.Classification
import AI.Classification.Boosting

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

---------------------------------------
-- ASSEMBLE.LogitBoost

data LogitASSEMBLE = LogitASSEMBLE

instance BoostAlg LogitASSEMBLE where
    boostAlgName _ = "ASSEMBLE.LogitBoost"
    boostUseUnlabeledDataPoints _ = True

    boostNewWeight tv' i = (alpha tv' i)*(cost' $ margin i)
        where
            cost  z = log $ 1+(exp $ -z)
--             cost' z = {-trace ("z="++show z) $ -}-2*(exp $ -2*z)/(log $ 1+(exp $ -2*z))
            cost' z = -2*(exp $ -2*z)/(1+(exp $ -2*z))
            
            alpha tv i =
                if i<numLabels tv
                   then 1
                   else (fromIntegral $ numLabels tv')/(fromIntegral $ V.length $ labels tv')

            margin i = (bool2num $ _y' i)*(_F' i)

            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i
