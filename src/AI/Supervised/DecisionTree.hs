{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FlexibleContexts #-}

{-

This is a modified version of the C4.5 decision tree introduced by Ross Quinlan. 
   
-}

module AI.Supervised.DecisionTree
    ( train
    , classify
    , probClassify
    , DTree(..)
    , defDTree
    )
    where

import AI.Classification
import AI.Ensemble

import Data.List
import Data.List.Extras
import Data.Maybe
import Debug.Trace

import qualified Data.Map as Map

-------------------------------------------------------------------------------
-- data types

data DTree labelType = 
    DTree 
        { splitAttr :: Int
        , splitCond :: DataItem
        , children :: Map.Map DataItem (DTree labelType) -- (Map.Map labelType Int)
        , tmpSplits :: [TrainingData labelType]
        
        -- config items
        , maxDepth :: Int
        }
    | DLeaf 
        { labelMap :: Map.Map labelType Int
        }
    deriving Show

defDTree = DTree
    { splitAttr = error "splitAttr undefined"
    , splitCond = error "splitCond undefined"
    , children = error "children undefined"
    , tmpSplits = error "tmpSplits undefined"
    , maxDepth = 4
    }

fetchAllLeaves :: DTree labelType -> [Map.Map labelType Int]
fetchAllLeaves (DLeaf l) = [l]
fetchAllLeaves dt = concat $ map fetchAllLeaves $ map snd $ Map.toList $ children dt

type SplitPoint = (Int,DataItem)

-------------------------------------------------------------------------------
-- ClassifyModel definition

instance (Ord label) => ClassifyModel (DTree label) label where
    modelName (DLeaf _) = "DLeaf"-- ++(show $ maxDepth dt)
    modelName dt = "DTree-"++(show $ maxDepth dt)

    train dt td ud = return $ trainDT (maxDepth dt) td

    probClassify = probClassifyDT

-------------------------------------------------------------------------------
-- training
    
trainDT depth xs = 
    case trainMaybe depth xs of
        Just dt -> dt
        Nothing -> DLeaf $ foldl' (Map.unionWith (+) ) Map.empty $ map (\(l,dp)->Map.singleton l 1) xs

-- train :: (Ord labelType) => Int -> TrainingData labelType -> DTree labelType
-- trainDT depth xs = 
--     case trainMaybe depth xs of
--          Just dt -> dt
--          Nothing -> DLeaf $ foldl' (Map.unionWith (+) ) Map.empty $ map (\(l,dp)->Map.singleton l 1) xs
--             error $ "DecisionTree.train: unable to create decision tree; training data="

trainMaybe :: (Ord labelType) => Int -> TrainingData labelType -> Maybe (DTree labelType)
trainMaybe depth xs = do
    old <- chooseSplit xs
    return $ old { tmpSplits=[]
        , children=calcChildren old depth $ tmpSplits old
        }
    
calcChildren :: (Ord labelData) => DTree labelType -> Int -> [TrainingData labelData] -> Map.Map DataItem (DTree labelData)
calcChildren old depth xs = Map.fromList $ map (\x -> ((snd $ head x)!!(splitAttr old),calcChild depth x)) xs

calcChild :: (Ord labelType) => Int -> TrainingData labelType -> DTree labelType
calcChild depth xs@(x1:x2:xrest) = 
    if depth==1||isNothing branchingTree
        then DLeaf $ foldl' (Map.unionWith (+) ) Map.empty $ map (\(l,dp)->Map.singleton l 1) xs
        else fromJust branchingTree
    where
        branchingTree=trainMaybe (depth-1) xs
calcChild depth xs = DLeaf $ foldl' (Map.unionWith (+) ) Map.empty $ map (\(l,dp)->Map.singleton l 1) xs

chooseSplit :: (Ord labelType) => TrainingData labelType -> Maybe (DTree labelType)
chooseSplit ([]) = error "chooseSplit only 0 arg; you must have >= 2 datapoints in your training data"
chooseSplit (x:[]) = error "chooseSplit only 1 arg; you must have >= 2 datapoints in your training data"
chooseSplit xs = 
    if (length splitpointList>0)
        then Just $ argmax (\x -> infoGain xs $ tmpSplits x) splitpointList 
        else Nothing
    where splitpointList=catMaybes [splitData xs n | n <- [0..(length $ snd $ head xs)-1]]

splitData :: (Ord labelType) => TrainingData labelType -> Int -> Maybe (DTree labelType)
splitData td attrI = -- trace ("td.length="++(show $ length td)++"; attrI="++(show attrI)) $
    case getAttrType td attrI of
        Continuous a -> splitDataDouble td attrI
        Discrete a -> splitDataString td attrI
        Missing -> Nothing

splitDataDouble :: (Ord labelType) => TrainingData labelType -> Int -> Maybe (DTree labelType)
splitDataDouble xs attrI = 
    if (length splitList == 0)
       then Nothing
       else Just $ DTree 
            { splitAttr = attrI
            , splitCond = (snd $ head $ splits!!1) !!attrI
            , children = Map.empty
            , tmpSplits = splits
            , maxDepth = undefined
            }
    where
        splits = argmax (\x -> infoGain xs x) $ splitList
        splitList = map (\x->[concat $ fst x, concat $ snd x]) splitPair
        splitPair = [splitAt n grouped | n <- [1..(length grouped)-1]]
        grouped = groupBy sameAttrI $ sortBy dpOrd xs
        sameAttrI (l1,dp1) (l2,dp2) = (dp1!!attrI)==(dp2!!attrI)
        dpOrd (l1,dp1) (l2,dp2) = compare (dp1!!attrI) (dp2!!attrI)

splitDataString :: TrainingData labelType -> Int -> Maybe (DTree labelType)
splitDataString xs attrI = 
    Just $ DTree 
        { splitAttr = attrI
        , splitCond = Discrete ""
        , children = Map.empty
        , tmpSplits = splits
        , maxDepth = error "Need to initialize depth somehow"
        }
    where 
        splits = Map.elems $ splitDataMap xs attrI Map.empty
        xs_trans = map (\(a,dps)->(a, map (fromDataItem::DataItem->String) dps)) xs

splitDataMap :: TrainingData labelType -> Int -> Map.Map DataItem (TrainingData labelType) -> Map.Map DataItem (TrainingData labelType)
splitDataMap []     attrI m = m
splitDataMap (x:xs) attrI m = splitDataMap xs attrI $ Map.insertWith (++) k [x] m
    where k = (snd x)!!attrI

-------------------------
-- information gain utils

infoGain :: (Ord labelType) => TrainingData labelType -> [TrainingData labelType] -> Double
infoGain xs ys = (info xs) - (sum $ map weightedInfo ys)
    where weightedInfo zs = (fromIntegral $ length zs)/(fromIntegral $ length xs)*(info zs)
              
info :: (Ord labelType) => TrainingData labelType -> Double
info xs = infoInt $ Map.elems $ labelCount xs Map.empty

labelCount :: (Ord labelType) => TrainingData labelType -> Map.Map labelType Int -> Map.Map labelType Int
labelCount []     m = m
labelCount (x:xs) m = labelCount xs $ Map.insertWith (+) (fst x) 1 m

infoInt :: [Int] -> Double
infoInt xs = sum $ map (\x -> -(x/tot)*(lg $ x/tot)) xs'
    where xs' = map (\x -> 0.000001+fromIntegral x) xs
          tot = sum xs'

lg :: Double -> Double -- base 2 logarithm
lg x = log x / log 2

-------------------------------------------------------------------------------
-- classification

probClassifyDT dt sample =
    labelCount2labelProb $ fetchLabelCount dt sample
    where 
        attr = sample!!(splitAttr dt)

labelCount2labelProb :: Map.Map labelType Int -> [(labelType,Prob)]
labelCount2labelProb m = map (\(l,x) -> (l,(fromIntegral x)/(fromIntegral $ tot))) $ Map.toList m
    where tot = sum $ map (\(l,c)->c) $ Map.toList m

-- fetchLabelCount :: (Ord labelType) => DTree labelType -> DataPoint -> Map.Map labelType Int
fetchLabelCount (DLeaf l) _ = l
fetchLabelCount dt dp = fetchFromAttr attr
    where
        attr = dp!!(splitAttr dt)
          
        fetchFromAttr (Continuous attr) = 
            if Missing==splitCond dt
                then defaultLabelCount dt
                else if attr<(fromDataItem $ splitCond dt :: Double)
                    then fetchLabelCount (snd $ head $ Map.toList $ children dt) dp
                    else fetchLabelCount (snd $ last $ Map.toList $ children dt) dp
        fetchFromAttr (Discrete attr) = 
            case Map.lookup (Discrete attr) $ children dt of
                Nothing -> defaultLabelCount dt
                Just x -> fetchLabelCount x dp
        fetchFromAttr Missing = defaultLabelCount dt
--          FIXME: we're probably not handling missing values very well here
--                 for example, in the Continuous case, we should have 3 separate entries in our Map
--                 one for less than, one for greater than, and one for Missing
--                 I don't really know what's going on right now, but it seems to come up very infrequently
--                 because the alg still gives good enough results
--             if splitCond dt==Missing
--                then error $ show $ Map.lookup Missing $ children dt
--                else defaultLabelCount dt

defaultLabelCount :: (Ord labelType) => DTree labelType -> Map.Map labelType Int
defaultLabelCount (DLeaf l) = l
defaultLabelCount dt = 
    foldl'
        (Map.unionWith (+))
        (Map.empty)
        (map (defaultLabelCount . snd) $ Map.toList $ children dt)

-------------------------------------------------------------------------------
-- debug

sqldata = [(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (1::Double),toDataItem "(1::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ,(True,[toDataItem (2::Double),toDataItem "(2::Int)"])
          ]
          
t=train (defDTree {maxDepth=2}) sqldata [] :: LogAI (DTree Bool)
example = [toDataItem (2::Double),toDataItem "(1::Int)"]

{-dt=DTree 
    { splitAttr = 3
    , splitCond = Discrete ""
    , children = Map.fromList 
        [(Discrete "A40", Map.fromList 
            [ (False,95)
            , (True,69)
            ])
        ,(Discrete "A41", Map.fromList 
            [(False,20)
            ,(True,7)])
        ,(Discrete "A410",Map.fromList 
            [(True,6)])
        ,(Discrete "A42",Map.fromList 
            [(False,29)
            ,(True,41)])
        ,(Discrete "A43",Map.fromList 
            [(False,45)
            ,(True,86)])
        ,(Discrete "A44",Map.fromList 
            [(True,1)])
        ,(Discrete "A45",Map.fromList 
            [(False,13)
            ,(True,8)])
        ,(Discrete "A46",Map.fromList 
            [(False,21)
            ,(True,1)])
        ,(Discrete "A49",Map.fromList 
            [(False,34)
            ,(True,24)])
        ]
    , tmpSplits = []
    }-}

td = 
    [(False,[Discrete "A11",Continuous 24.0,Discrete "A31",Discrete "A42",Continuous 3349.0,Discrete "A63",Discrete "A72",Continuous 4.0,Discrete "A93",Discrete "A101",Continuous 4.0,Discrete "A124",Continuous 30.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 2.0,Discrete "A192",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 6.0,Discrete "A31",Discrete "A46",Continuous 1198.0,Discrete "A61",Discrete "A75",Continuous 4.0,Discrete "A92",Discrete "A101",Continuous 4.0,Discrete "A124",Continuous 35.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 6.0,Discrete "A31",Discrete "A46",Continuous 1198.0,Discrete "A61",Discrete "A75",Continuous 4.0,Discrete "A92",Discrete "A101",Continuous 4.0,Discrete "A124",Continuous 35.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 6.0,Discrete "A31",Discrete "A46",Continuous 1198.0,Discrete "A61",Discrete "A75",Continuous 4.0,Discrete "A92",Discrete "A101",Continuous 4.0,Discrete "A124",Continuous 35.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 6.0,Discrete "A31",Discrete "A46",Continuous 1198.0,Discrete "A61",Discrete "A75",Continuous 4.0,Discrete "A92",Discrete "A101",Continuous 4.0,Discrete "A124",Continuous 35.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 6.0,Discrete "A31",Discrete "A46",Continuous 1198.0,Discrete "A61",Discrete "A75",Continuous 4.0,Discrete "A92",Discrete "A101",Continuous 4.0,Discrete "A124",Continuous 35.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 6.0,Discrete "A31",Discrete "A46",Continuous 1198.0,Discrete "A61",Discrete "A75",Continuous 4.0,Discrete "A92",Discrete "A101",Continuous 4.0,Discrete "A124",Continuous 35.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(True,[Discrete "A14",Continuous 6.0,Discrete "A31",Discrete "A43",Continuous 1750.0,Discrete "A63",Discrete "A75",Continuous 2.0,Discrete "A93",Discrete "A101",Continuous 4.0,Discrete "A122",Continuous 45.0,Discrete "A141",Discrete "A152",Continuous 1.0,Discrete "A172",Continuous 2.0,Discrete "A191",Discrete "A201"])
    ,(True,[Discrete "A14",Continuous 6.0,Discrete "A31",Discrete "A43",Continuous 1750.0,Discrete "A63",Discrete "A75",Continuous 2.0,Discrete "A93",Discrete "A101",Continuous 4.0,Discrete "A122",Continuous 45.0,Discrete "A141",Discrete "A152",Continuous 1.0,Discrete "A172",Continuous 2.0,Discrete "A191",Discrete "A201"])
    ,(True,[Discrete "A14",Continuous 6.0,Discrete "A31",Discrete "A43",Continuous 1750.0,Discrete "A63",Discrete "A75",Continuous 2.0,Discrete "A93",Discrete "A101",Continuous 4.0,Discrete "A122",Continuous 45.0,Discrete "A141",Discrete "A152",Continuous 1.0,Discrete "A172",Continuous 2.0,Discrete "A191",Discrete "A201"])
    ,(True,[Discrete "A14",Continuous 6.0,Discrete "A31",Discrete "A43",Continuous 1750.0,Discrete "A63",Discrete "A75",Continuous 2.0,Discrete "A93",Discrete "A101",Continuous 4.0,Discrete "A122",Continuous 45.0,Discrete "A141",Discrete "A152",Continuous 1.0,Discrete "A172",Continuous 2.0,Discrete "A191",Discrete "A201"])
    ,(True,[Discrete "A14",Continuous 6.0,Discrete "A31",Discrete "A43",Continuous 1750.0,Discrete "A63",Discrete "A75",Continuous 2.0,Discrete "A93",Discrete "A101",Continuous 4.0,Discrete "A122",Continuous 45.0,Discrete "A141",Discrete "A152",Continuous 1.0,Discrete "A172",Continuous 2.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 12.0,Discrete "A31",Discrete "A43",Continuous 2149.0,Discrete "A61",Discrete "A73",Continuous 4.0,Discrete "A91",Discrete "A101",Continuous 1.0,Discrete "A124",Continuous 29.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 12.0,Discrete "A31",Discrete "A43",Continuous 2149.0,Discrete "A61",Discrete "A73",Continuous 4.0,Discrete "A91",Discrete "A101",Continuous 1.0,Discrete "A124",Continuous 29.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ,(False,[Discrete "A11",Continuous 12.0,Discrete "A31",Discrete "A43",Continuous 2149.0,Discrete "A61",Discrete "A73",Continuous 4.0,Discrete "A91",Discrete "A101",Continuous 1.0,Discrete "A124",Continuous 29.0,Discrete "A143",Discrete "A153",Continuous 1.0,Discrete "A173",Continuous 1.0,Discrete "A191",Discrete "A201"])
    ]

samplebreak=[Discrete "A12",Continuous 6.0,Discrete "A34",Discrete "A48",Continuous 932.0,Discrete "A65",Discrete "A74",Continuous 1.0,Discrete "A92",Discrete "A101",Continuous 3.0,Discrete "A122",Continuous 39.0,Discrete "A143",Discrete "A152",Continuous 2.0,Discrete "A172",Continuous 1.0,Discrete "A191",Discrete "A201"]