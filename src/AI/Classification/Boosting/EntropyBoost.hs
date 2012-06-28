module AI.Classification.Boosting.EntropyBoost
    where
          
import AI.Classification
import AI.Classification.Boosting

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

---------------------------------------
-- EntropyBoost (See "Information Theoretic Regularization for Semi-Supervised Boosting," Zheng, Wang, Liu.

data EntropyBoost = EntropyBoost

instance BoostAlg EntropyBoost where
    boostAlgName _ = "EntropyBoost"
    boostUseUnlabeledDataPoints _ = True

    boostNewWeight tv' i =
        if i<(numLabels tv')
           then (cost_label' $ margin i)
           else gamma*(sum [(cost_unlabel' $ (bool2num y)*(_F' i)) | y <- [True, False]])
           
        where
            
            cost_label  z = log $ 1+(exp $ -2*z)
            cost_label' z = -2*(exp $ -2*z)/(1+(exp $ -2*z))
            
            cost_unlabel  z = (log $ 1+(exp $ -z))/(1+(exp $ -z))
            cost_unlabel' z = (-1 + (log $ 1+(exp $ -z)))/(1+(exp $ -z)) * (cost_label' z)
            
            margin i = (bool2num $ _y' i)*(_F' i)
            
            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i
            
            gamma = 0.01
