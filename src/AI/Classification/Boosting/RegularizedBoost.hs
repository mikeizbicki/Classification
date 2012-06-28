module AI.Classification.Boosting.RegularizedBoost
    where
          
import AI.Classification
import AI.Classification.Boosting

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB

import Debug.Trace

---------------------------------------
-- RegularizedBoost

data RegularizedBoost = RegularizedBoost

instance BoostAlg RegularizedBoost where
    boostAlgName _ = "RegularizedBoost"
    boostUseUnlabeledDataPoints _ = True
                    
    boostErr tv = err_normal+err_reg
        where
              
            err_normal = sum [ (_D i) | i <- filter (\i -> _y i/=_f i) $ indices tv ]
            err_reg = (sum [ -(beta tv i)*(_R 1 tv i) | i <- filter (\i -> _y i==_f i) $ indices tv ])
                    / normalizingFactor
            
            normalizingFactor = (boostNewWeight tv 1)/(_D 1)

            _f i = (_f_vec tv) V.! i
            _F i = (_F_vec tv) V.! i
            _x i = (dataPoints tv) VB.! i
            _y i = (labels tv) V.! i
            _D i = (weights tv) V.! i
                        
            
    boostNewWeight tv' i = 
--         trace (
--         "ada = "++(show $ cost' $ margin i)++"\n"++
--         "_R sigma=0.8 - "++(show $ _R 0.8 tv' i)++"\n"++
--         "_R sigma=0.9 - "++(show $ _R 0.9 tv' i)++"\n"++
--         "_R sigma=1 - "++(show $ _R 1 tv' i)++"\n"++
--         "_R sigma=2 - "++(show $ _R 2 tv' i)++"\n"++
--         "_R sigma=3 - "++(show $ _R 3 tv' i)++"\n"++
--         "_R sigma=4 - "++(show $ _R 4 tv' i)++"\n"++
--         "_R sigma=5 - "++(show $ _R 5 tv' i)++"\n"++
--         "\n"
--         ) $
        (alpha tv' i)*(cost' $ margin i) - (beta tv' i)*(_R 1 tv' i)
        where
            margin i = (bool2num $ _y' i)*(_F' i)

            _F' i = (_F_vec tv') V.! i
            _y' i = (labels tv') V.! i
            
----------------------------------------
-- algorithm

_R sigma tv i = sum 
    [ (similarity sigma (_x i) (_x j))*(cost_tilde $ -compatibility i j) 
    | j<-indices tv
    ]
    where 
        compatibility i j = abs $ (bool2num $ _y i) - (bool2num $ _y j)
        _y i = (labels tv) V.! i
        _x i = (dataPoints tv) VB.! i
   
--params
cost  z =  (exp $ -z)
cost' z = -(exp $ -z)
cost_tilde z = (cost z)-1
            
alpha tv i = -- 1
    if i<numLabels tv
        then 1
        else 0.15
    
beta tv i = -- 1/2
    if i<numLabels tv
        then 1
        else 0.15/2



-- debug

-- _R2 (y1,x1) (y2,x2) = (similarity x1 x2)*(cost_tilde $ -(abs $ (bool2num y1)-(bool2num y2)))
-- x1 = (True,[Continuous 5, Continuous 4])
-- x2 = (False,[Continuous 4, Continuous 5])

----------------------------------------
-- helpers
        
similarity :: Double -> DataPoint -> DataPoint -> Double
similarity sigma dp1 dp2 = res
    where res = exp $ (-diff / (2*sigma^2))
          diff = sum [ (dataminus x1 x2)^2 | (x1,x2) <- zip dp1 dp2] :: Double
          
          
dataminus :: DataItem -> DataItem -> Double
dataminus (Continuous x1) (Continuous x2) = x1 - x2
dataminus (Discrete x1) (Discrete x2) = 
    if (x1==x2)
       then 0
       else 1

-- instance BoostAlg RegularizedBoost where
--     boostAlgName _ = "RegularizedBoost"
--     boostUseUnlabeledDataPoints _ = True
-- 
--     boostErr tv = trace ("left="++show left++" >> right="++show right) left+right
-- 
--         where
--             left =   sum [ (indicator $ _y i /= _f i)*(_D i) | i <- indices tv ]
--             right= -(sum [ (indicator $ _y i == _f i)*(betaR tv i) | i <- indices tv ]
--                     /sum [ (cost' $ margin i) - (betaR tv i)| i<- indices tv]
--                     )
--             _F i = (_F_vec tv) V.! i
--             _f i = (_f_vec tv) V.! i
--             _x i = (dataPoints tv) VB.! i
--             _y i = (labels tv) V.! i
--             _D i = (weights tv) V.! i
--             
--             cost  z =  (exp $ -z)
--             cost' z = -(exp $ -z)
--             margin i = (bool2num $ _y i)*(_F i)
-- 
--     boostNewWeight tv' i = (alpha i)*(cost' $ margin i) - (betaR tv' i)
--         where
--             alpha i = 1
--             margin i = (bool2num $ _y' i)*(_F' i)
--             
--             cost  z =  (exp $ -z)
--             cost' z = -(exp $ -z)
--             
--             _F' i = (_F_vec tv') V.! i
--             _y' i = (labels tv') V.! i
-- 
-- betaR tv i = (beta i)*(_R i)
--     where
--         beta i = 0.5
--         _R i = sum [(similarity (_x i) (_x j))*(cost_tilde $ (-1)*(compatability i j)) 
--                    | j <- filter (i/=) $ indices tv
--                    ]
--         
--         cost  z =  (exp $ -z)
--         cost' z = -(exp $ -z)
--         cost_tilde z = (cost z)-1
--         
--         compatability i j = abs $ (bool2num $ _y i)-(bool2num $ _y j)
--         
--         _F i = (_F_vec tv) V.! i
--         _y i = (labels tv) V.! i
--         _x i = (dataPoints tv) VB.! i
