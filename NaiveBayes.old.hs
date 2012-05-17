
import Data.Array
import qualified Data.Map as Map
import Data.Typeable
import Database.HDBC
import Debug.Trace

-- test :: (Typeable a) => [a] -> a
-- test (x:xs) = x
-- 
-- data Attribute = AttrDisc String Int | AttrReal String Double


type Var = Double
type Prob = Double

type DataPoint = (Label,[SqlValue])

data Attr = Attr String deriving Show
data Label = Label String deriving (Show,Eq,Ord)

data BStore = BayesDiscrete {} | BayesContinuous { m_k :: Double, s_k :: Double, k :: Double  }
    deriving Show

data BayesModel = BayesModel { totalSamples :: Int
                             , classSamples :: Map.Map Label Int
                             , calc :: Map.Map Label [BStore]
                             , labelList :: [Label]
                             }
                             deriving (Show)

train :: [DataPoint] -> BayesModel
train (x:xs) = foldl trainItr model xs
    where model = BayesModel { totalSamples = 1
                             , classSamples = Map.singleton (fst x) 1
                             , calc = Map.singleton (fst x) $ map (\m -> BayesContinuous ((fromSql m)::Double) 0 1) (snd x)
                             , labelList = [Label "male",Label "female"]
                             }

trainItr :: BayesModel -> DataPoint -> BayesModel
-- trainItr m []  = m
trainItr m (x) = BayesModel { totalSamples = (totalSamples m)+1
                            , classSamples = Map.insertWith' (+) (fst x) 1 (classSamples m)
                            , calc = Map.insert (fst x) (map updateCalc $ zip (snd x) (oldcalc)) (calc m)
                            , labelList = labelList m
                            }
                                where oldcalc = case (Map.lookup (fst x) $ calc m) of
                                                    Nothing -> Map.empty --error "trainItr"
                                                    Just x -> x
                                      
                                      
-- This method of computing a running std-dev is from Knuth's AOCP, V2, pg 232
updateCalc :: (SqlValue,BStore) -> BStore
updateCalc (a, BayesContinuous m s k) = --trace (show m') $ 
                                        BayesContinuous m' s' (k+1)
    where x = fromSql a
          m' = m + (x-m) / k
          s' = s + (x-m) * (x-m')
                           
variance :: BStore -> Double
variance (BayesContinuous m s k) = (s / (k - 1))
                           
-- createModel :: DataPoint -> BayesModel
-- createModel (label,values) = BayesModel { totalSamples = 1
--                                         , classSamples = Map.singleton label 1
--                                         , calc = []
--                                         }

condProb :: BStore -> SqlValue -> Prob
condProb (BayesContinuous m s k) val = 0

labelProb :: BayesModel -> Label -> Prob
labelProb model label = (fromIntegral samples) / fromIntegral (totalSamples model)
    where samples = case (Map.lookup label (classSamples model)) of
                        Nothing -> error "labelProb?!"
                        Just x -> x

attrProb :: (BStore, SqlValue) -> Prob
attrProb (bstore,sql) = case bstore of
                        (BayesContinuous m s k) -> 1 / sqrt (2 * pi * v) * exp ((  (fromSql sql) - m)^2 / (2 * v))
    where v = variance bstore

classify :: BayesModel -> [SqlValue] -> [(Label,Prob)]
classify model xs = [(l
                     ,(labelProb model l) * (f l)
                     ) 
                    | l <- labelList model
                    ]
                        where f l = (foldl (*) 1 $ map (attrProb) (zip (calclist l) xs))
                              calclist l = case (Map.lookup l (calc model)) of
                                              Just x -> x
{-
sex
height (feet)
weight (lbs)
foot size(inches)
male  6  180  12
  male  5.92 (5'11")  190  11
  male  5.58 (5'7")  170  12
  male  5.92 (5'11")  165  10
  female  5  100  6
  female  5.5 (5'6")  150  8
  female  5.42 (5'5")  130  7
  female  5.75 (5'9")  150  9
  -}

dataset = [(Label "male",[6.0,180,12])
          ,(Label "male",[5.92,190,11])
          ,(Label "male",[5.58,170,12])
          ,(Label "male",[5.92,165,10])
          ,(Label "female",[5,100,6])
          ,(Label "female",[5.5,150,8])
          ,(Label "female",[5.42,130,7])
          ,(Label "female",[5.75,150,9])
          ]

dataset' = [(Label "male",[6.0,180,12])
          ]

sqldata = map (\(label,xs) -> (label,map (toSql::Double->SqlValue) xs)) dataset

