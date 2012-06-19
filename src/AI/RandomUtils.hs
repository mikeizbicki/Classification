module AI.RandomUtils
    where
          
   
import qualified Data.Map as Map
import qualified Data.Set as S

import System.Random
import Data.List

-------------------------------------------------------------------------------
-- Sampling from weighted lists, used by boosting

sample :: (Show a,Eq a) => StdGen -> Int -> [(Double,a)] -> [a]
sample rgen n xs = {-trace ("fst xs: "++(show $ map fst xs)) $-} sampleWalk 0 xs randL
    where 
          totalWeights = sum $ map fst xs
          randL =  sort $ randList rgen n (0,totalWeights)

sampleWalk :: (Show a) => Double -> [(Double,a)] -> [Double] -> [a]
sampleWalk tally [] _  = []
sampleWalk tally _  [] = []
sampleWalk tally (x:xs) (y:ys) = 
    if not sanity
       then error $ "sample: One of the sampling weights is either NaN or Infinity:" -- ++(show xs) ++ " -- "++(show ys)
       else if ((fst x)+tally)>(y)
               then (snd x):(sampleWalk tally (x:xs) $ ys)
               else sampleWalk (tally+(fst x)) (xs) (y:ys)
    where 
        sanity = (isNumber $ fst x) && (isNumber y)
        diff = (fst x)+tally-y

isNumber :: Double -> Bool
isNumber x = if x/=x -- x is NaN
                then False
                else if x==x+1 -- x is +/- Infinity
                        then False
                        else True

randList :: (Random a, Eq a) => StdGen -> Int -> (a,a) -> [a]
randList rgen 0 interval = []
randList rgen n interval = if r==r
                              then r:(randList rgen' (n-1) interval)
                              else error "randList: r/=r --> r==NaN"
    where (r,rgen') = randomR interval rgen


sampleTest2 = map length $group $ sampleTest
sampleTest = sample (mkStdGen 210) 50 [(1,n) | n <- [1..100]]


-------------------------------------------------------------------------------
-- below stuff used by testing

randSplit:: StdGen -> Double -> [a] -> ([a],[a])
randSplit rgen factor xs = randSplitWalk 0 (length xs - 1) xs is ([],[])
    where is=randList2 rgen (floor $ factor*(fromIntegral $ length xs)) (length xs-1) S.empty

randSplitWalk :: Int -> Int -> [a] -> [Int] -> ([a],[a]) -> ([a],[a])
randSplitWalk itr stop xs     []     (s1,s2) = (s1,xs++s2)
randSplitWalk itr stop (x:xs) (y:ys) (s1,s2) = 
    if itr>stop
       then (s1,s2)
       else if itr==y
               then randSplitWalk (itr+1) stop xs ys     (x:s1,s2)
               else randSplitWalk (itr+1) stop xs (y:ys) (s1,x:s2)

randList2 :: StdGen -> Int -> Int -> S.Set Int -> [Int]
randList2 rgen total m set = 
    if S.size set == total
       then S.toList set
       else randList2 rgen' total m (S.insert r set)
           where (r,rgen') = randomR (0, m) rgen

s2ss:: StdGen -> Double -> [(Bool,a)] -> ([(Bool,a)],[a])
s2ss rgen factor xs = (l1, map snd l2)
    where (l1,l2)=randSplit rgen factor xs

--

-- s2ssX=s2ss
s2ssX:: StdGen -> Int -> [(Bool,a)] -> ([(Bool,a)],[a])
s2ssX rgen count ds = (fst xs,map snd $ snd xs)
    where 
        is=chooseIndices rgen count ds
        xs=splitIndices ds is

splitIndices ::[(Bool,a)] -> [Int] -> ([(Bool,a)],[(Bool,a)])
splitIndices ds is = 
    splitIndicesWalker ds (sort is) 0 [] []
    
splitIndicesWalker (d:[]) []     count s1 s2 = (s1,d:s2)
splitIndicesWalker (d:[]) (i:[]) count s1 s2 = (d:s1,s2)
splitIndicesWalker (d:ds) []     count s1 s2 = (s1,(d:ds)++s2)
splitIndicesWalker (d:ds) (i:is) count s1 s2 = 
    if i==count
       then splitIndicesWalker ds (is) (count+1) (d:s1) s2
       else splitIndicesWalker ds (i:is) (count+1) s1 (d:s2)

itest = [(True,1),(True,20),(True,3),(True,4),(False,10),(False,11),(False,-12),(False,13)]

chooseIndices :: StdGen -> Int -> [(Bool,a)] -> [Int]
chooseIndices rgen count ds = chooseIndicesItr rgen count ds S.empty S.empty

chooseIndicesItr :: StdGen -> Int -> [(Bool,a)] -> S.Set Int -> S.Set Int -> [Int]
chooseIndicesItr rgen count ds ts fs = 
    if S.size ts==count && S.size fs==count
       then (S.toList ts)++(S.toList fs)
       else if (fst $ ds !! r)
               then if (S.size ts<count)
                       then chooseIndicesItr rgen' count ds (S.insert r ts) fs 
                       else chooseIndicesItr rgen' count ds ts fs
               else if (S.size fs<count)
                       then chooseIndicesItr rgen' count ds ts (S.insert r fs)
                       else chooseIndicesItr rgen' count ds ts fs
    where  
        (r,rgen')=randomR (0,(length ds)-1) rgen
