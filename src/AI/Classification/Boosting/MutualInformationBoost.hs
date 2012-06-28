module AI.Classification.Boosting.MutualInformationBoost
    where
          
import AI.Classification
import AI.Classification.Boosting

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

---------------------------------------
-- MutualInformationBoost (See "Information Theoretic Regularization for Semi-Supervised Boosting," Zheng, Wang, Liu.

data MutualInformationBoost = MutualInformationBoost

instance BoostAlg MutualInformationBoost where
    boostAlgName _ = "MutualInformationBoost"
    boostUseUnlabeledDataPoints _ = True

--     boostStepSize tv = 1

    boostNewWeight tv' i =
        if i<(numLabels tv')
           then (cost_label' $ margin i)
           else -unlabeled_weight 
--            else gamma*(sum [(cost_unlabel' $ (bool2num y)*(_F' i)) | y <- [True, False]])
           
        where
            margin i = (bool2num $ _y' i)*(_F' i)
            
            cost_label  z = log $ 1+(exp $ -2*z)
            cost_label' z = -2*(exp $ -2*z)/(1+(exp $ -2*z))
            
            cost_unlabel  z = (log $ 1+(exp $ -z))/(1+(exp $ -z))
            cost_unlabel' z = (-1 + (log $ 1+(exp $ -z)))/(1+(exp $ -z)) * (cost_label' z)
            
            unlabeled_weight = gamma*(sum [(cost_label' $ margin i)*(
                    -(log numUnlabeled)
                    +(-1 + (log $ 1+(exp $ -(bool2num y)*(_F' i))))
                    -(log $ sum 
                        [ 1+1/(1+(exp $ -(bool2num y)*(_F' j))) | j<-filter (>=numLabels tv') $ indices tv']
                        )
                )/(1+(exp $ -(bool2num y)*(_F' i)))
                | y<- [True,False]
                ])

            numUnlabeled = fromIntegral $ (V.length $ labels tv')-(numLabels tv')
            
            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i
            
            gamma = 0.01
