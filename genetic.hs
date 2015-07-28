--This all needs to be changed! Wrong algorithm!

--{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Random
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Applicative ((<*>), (<$>))
import qualified Data.Vector as V

import Debug.Trace

main :: IO()
main = print 3
--main = choosePrograms 100 >>= print
--main = take 40 $ map (+3) [1..]

data OPType = Add | Sub | Mult | Div deriving (Show, Eq)

type Size = Int
type Depth = Int
type Fitness = Float
type NumValues = (Int, Float)
type DataSet = [[Float]]

data Node = OP OPType Node Node Depth Size | Const Float Depth Size | Var Char Depth Size deriving (Eq)
data Program = Program Node Fitness deriving (Eq, Show)
data NodeInfo = This | L | R

--n1 = randNode
--n2 = randNode

instance Ord Program where
    (Program _ f1) <= (Program _ f2) = f1 <= f2

--Best viewed when autoformatted as lisp
instance Show Node where
    show (OP opType left right depth size) = "\n(OP " ++ show opType ++ " " ++ show depth ++ " " ++ show size ++ " " ++ show left ++" " ++ show right ++ ")"
    show (Const val depth size) = "\n(Const " ++ show val ++" "++ show depth ++")"
    show (Var val depth size) = "\n(Var " ++ show val ++" "++ show depth ++")"

--Pregeneration: check score of best program
--Overwrite worst 2 with best 2
--mutate
--iterate over vector for n crossovers.

--crossoverPrograms :: V.Vector Program -> Int -> IO (V.Vector Program)
--crossoverPrograms vec n = iterateM cr

--pipe var values to fitness funciton
--pipe max vars to mutation, gen

--get data
--create vector
--iterate through generations
--make iterate to be usable for takewhile, return program instead of new vector.
evolve :: IO()
--evolve = do


getValues :: IO (DataSet, NumValues)
getValues = do
    file <- readFile "problem.dat"
    ds <- return $ formatData file
    return (tail ds, (floor $ (head ds)!!0, (head ds!!1)))
    

formatData :: String -> DataSet
formatData = map (toWords . words) . lines
  where toWords = map read

endGenetic :: V.Vector Program -> Bool
endGenetic vec = (getFitness . V.maximum) vec > threshold

generation :: V.Vector Program -> DataSet -> NumValues-> IO (V.Vector Program)
generation rawVec ds nv = do
    vec <- return $ V.map (fitnessFunction ds) rawVec
    best <- return $ V.maximum vec
    oVec <- ((mutatePrograms nv) <=< crossoverPrograms) vec
    return $ oVec V.// [(V.minIndex oVec, best)]


fitnessFunction :: DataSet -> Program-> Program 
fitnessFunction ds (Program n _ ) = Program n $ foldr (\a b-> b + evalFunction n a) 0 ds

--assume [vars ... result]
evalFunction :: Node -> [Float] -> Float
evalFunction n vv = (((-1) *) . abs) ((fitFunc n) - (last vv))
    where fitFunc (OP Add left right _ _) = (fitFunc left) + (fitFunc right)
          fitFunc (OP Sub left right _ _) = (fitFunc left) - (fitFunc right)
          fitFunc (OP Mult left right _ _) = (fitFunc left) * (fitFunc right)
          fitFunc (OP Div left right _ _)
            |rVal == 0 = (fitFunc left)
            |otherwise = (fitFunc left) / rVal
            where rVal = fitFunc right
          fitFunc (Const val _ _) = val
          fitFunc (Var var _ _) = vv !! (ord var - 65)

mutatePrograms :: NumValues -> V.Vector Program -> IO (V.Vector Program)
mutatePrograms nv vec= V.mapM (mutateProgram nv) vec

mutateProgram :: NumValues -> Program -> IO Program
mutateProgram nv (Program n fit) = Program <$> (treeMapM (mutateNode nv) n) <*> return fit

crossoverPrograms :: V.Vector Program -> IO (V.Vector Program)
crossoverPrograms vec = (iterateM crossoverProgram (return vec)) !! crossoverRounds

--dont bother fitnessing: check fitness on start of each generation
crossoverProgram :: V.Vector Program -> IO (V.Vector Program)
crossoverProgram vec = do 
    (a, b) <- tourney vec ((V.length vec), (V.length vec))
    (newA, newB) <- getCrossover (getNode $ vec V.! a, False) (getNode $ vec V.! b, False)
    return $ vec V.// [(a, Program newA $ -999999), (b, Program newB $ -9999999)]

choosePrograms :: V.Vector Program -> IO (Int, Int)
choosePrograms vec = (iterateM (tourney vec) (tuple <$> rand (V.length vec) <*> rand (V.length vec))) !! tourneyRounds
    where tuple a b = (a, b)

--a>b
--Fix, currently based off sorted list. 
tourney :: V.Vector Program -> (Int, Int) -> IO (Int, Int)
tourney vec (a, b) = choose <$> (rand (V.length vec) :: IO Int)
    where choose n 
            |getFitnessN vec n > getFitnessN vec a = (n, a)
            |getFitnessN vec n == getFitnessN vec a = (a, a-1)
            |getFitnessN vec n > getFitnessN vec b = (a, n)
            |otherwise = (a, b)

iterateM :: (Monad m) => (a -> m a) -> m a -> [m a]
iterateM f = iterate (f =<<)

getCrossover :: (Node, Bool) -> (Node, Bool) -> IO (Node, Node)
getCrossover (n1, True) (n2, True) = return (n2, n1)
getCrossover (n1, b1) (n2, b2) = do
                                    t1 <- getNext n1 b1
                                    t2 <- getNext n2 b2
                                    (child1, child2) <- getCrossover (infoToNode n1 t1) (infoToNode n2 t2)
                                    return ((rebuildTree n1 child1 t1), (rebuildTree n2 child2 t2))
    where infoToNode p (This, cont) = (p, cont)
          infoToNode (OP _ left _ _ _) (L, cont) = (left, cont)
          infoToNode (OP _ _ right _ _) (R, cont) = (right, cont)
          rebuildTree p c (This, _) = c
          rebuildTree (OP t _ right d s) c (L, _) = (OP t (updateDepth c (d+1)) right d (updateSize c right))
          rebuildTree (OP t left _ d s) c (R, _) = (OP t left (updateDepth c (d+1)) d (updateSize c left))

getNext :: Node -> Bool -> IO (NodeInfo, Bool)
getNext n@(OP t l r d s) b = if not b then ((rand 1 :: IO Float) >>= chooseNode) else return (This, True)
            where chooseNode a
                    |a < 0.3 = return (This, True)
                    |a - 0.15 < (fromIntegral (getSize l) / fromIntegral (getSize l + getSize r)) = return (L, False)
                    |otherwise = return (R, False)
getNext n b = return (This, True)

updateDepth :: Node -> Int -> Node
updateDepth (OP t l r d s) depth = OP t (updateDepth l $ depth + 1) (updateDepth r $ depth + 1) depth s
updateDepth (Const v d s) depth = Const v depth s
updateDepth (Var v d s) depth = Var v depth s

--pipe here
genRandomTree :: NumValues -> Int -> IO Node
genRandomTree nv@(v, m) depth
    |depth < 3 = (rand 1 :: IO Float) >>= gen
    |otherwise = (rand 0.3 :: IO Float) >>= gen
    where gen n
            |n > 0.3 = OP <$> randType <*> tree1 <*> tree2 <*> return depth <*> (updateSize <$> tree1 <*> tree2)
            |n > 0.15 = Const <$> ((\x -> x - m) <$> (rand (2 * m) :: IO Float)) <*> return depth <*> return 1
            |otherwise = Var <$> randChar (chr (v + 64)) <*> return depth <*> return 1
          tree1 = genRandomTree nv (depth + 1)
          tree2 = genRandomTree nv (depth + 1)

updateSize :: Node -> Node -> Size
updateSize n1 n2 = 1 + getSize n1 + getSize n2

getSize :: Node -> Size
getSize (OP _ _ _ _ s) = s
getSize (Const _ _ s) = s
getSize (Var _ _ s) = s

--pipe here
mutate :: NumValues -> Node -> IO Node
mutate _ (OP opType left right depth size) = OP <$> randType <*> return left <*> return right <*> return depth <*> return size
mutate (_, m) (Const _ depth size) = Const <$> ((\x -> x - m) <$> (rand (2 * m) :: IO Float)) <*> return depth <*> return size
mutate (v, _)(Var _ depth size) = Var <$> randChar (chr (v + 64)) <*> return depth <*> return size

mutateNode :: NumValues -> Node -> IO Node
mutateNode nv node = (mutateProb <$> (rand 1 :: IO Float)) >>= (\x -> if x then (mutate nv node) else return node)

mutateProb :: Float -> Bool
mutateProb n 
    |n < 0.05 = True
    |otherwise = False

--treeMapM :: Monad m => (Node -> m Node) -> Node -> m Node
treeMapM f n = (isOP <$> node) >>= (\x -> if x then recur else node)
    where node = f n
          recur = node >>= (treeMapMHelper f)

treeMapMHelper f (OP opType left right depth size) = OP <$> return opType <*> tree1 <*> tree2 <*> return depth <*> (updateSize <$> tree1 <*> tree2)
    where tree1 = treeMapM f left
          tree2 = treeMapM f right

treeMap :: (Node -> Node) -> Node -> Node
treeMap f n
    |isOP node = recur node
    |otherwise = node
    where node = f n
          recur (OP opType left right depth size) = OP opType (treeMap f left) (treeMap f right) depth size

getFitnessN :: V.Vector Program -> Int -> Float
getFitnessN vec n = getFitness $ vec V.! n

getNode :: Program -> Node
getNode (Program n _) = n

getFitness :: Program -> Float
getFitness (Program _ f) = f

isOP :: Node -> Bool
isOP (OP _ _ _ _ _) = True
isOP _ = False

isConst :: Node -> Bool
isConst (Const _ _ _) = True
isConst _ = False

isVar :: Node -> Bool
isVar (Var _ _ _) = True
isVar _ = False

rand :: (Num a,Random a) => a -> IO a
rand k = newStdGen >>= return . fst . randomR (0, k)

randChar :: Char -> IO Char
randChar k = newStdGen >>= return . fst . randomR ('A', k)

randType :: IO OPType
randType = getType <$> (rand 3 :: IO Int)

getType :: Int -> OPType
getType n = [Add, Sub, Mult, Div] !! n

tourneyRounds = 4

crossoverRounds = 10

threshold :: Float
threshold = 1e-2

randNode nv = genRandomTree nv 0
