--{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Random
import Data.List
import Control.Monad
import Control.Applicative ((<*>), (<$>))

import GHC.Prim
import Control.Monad.Primitive as V

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Algorithms.Intro as V
import Debug.Trace

main :: IO()
main = choosePrograms 100 >>= print
--main = do
--    one <- n1
--    two <- n2
--    writeFile "genetic.txt" $ show one ++ "\n\n" ++ show two
--    (c1, c2) <- getCrossover (one, False) (two, False)
--    writeFile "GeneticCrossover.txt" $ show c1 ++ "\n\n" ++ show c2

--main = do
--    tree <- randNode
--    writeFile "genetic.txt" $ show tree
--    mutatedNode <- mutate tree
--    writeFile "geneticMutate.txt" $ show mutatedNode
--    mutatedTree <- treeMapM mutate tree
--    writeFile "geneticMutateTree.txt" $ show mutatedTree

data OPType = Add | Sub | Mult | Div deriving (Show, Eq)

type Size = Int
type Depth = Int
type Fitness = Float

data Node = OP OPType Node Node Depth Size | Const Float Depth Size | Var Char Depth Size
data Program = Program Node Fitness deriving (Show)
data NodeInfo = This | L | R

--n1 = return $ OP Add (Const 1.1231 1 1) (Const  2.678 1 1) 0 3
--n2 = return $ OP Sub (Const 3.1231 1 1) (Const  4.678 1 1) 0 3
n1 = randNode
n2 = randNode
--n1 = return $ Var 'X' 0 1
--n2 = return $ Var 'Y' 0 1

instance Ord Program where
    (Program _ f1) <= (Program _ f2) = f1 <= f2

--Best viewed when autoformatted as lisp
instance Show Node where
    show (OP opType left right depth size) = "\n(OP " ++ show opType ++ " " ++ show depth ++ " " ++ show size ++ " " ++ show left ++" " ++ show right ++ ")"
    show (Const val depth size) = "\n(Const " ++ show val ++" "++ show depth ++")"
    show (Var val depth size) = "\n(Var " ++ show val ++" "++ show depth ++")"

--note that vector has an update field
--runGeneration :: V.Vector Program -> Either IO (V.Vector Program) IO()
--runGeneration programs = do
--    n <- rand 1 :: IO Float
--    if n < 0.5 then
--        Left $ mapM (treeMapM (mutate . getNode)) programs
--        else
--            do
--                (p1, p2) <- choosePrograms (-1 + V.length programs)
--                (n1, n2) <- getCrossover (p1, False) (p2, False)
--                Left $ V.update programs <(p1, n1), (p2, n2)>

mutatePrograms :: (V.PrimMonad m, V.MVector v e) => v (V.PrimState m) e -> m()
mutatePrograms vec = V.forM_ $ V.write vec (treeMapM (mutate . getNode))

choosePrograms :: Int -> IO (Int, Int)
choosePrograms l = (iterateM (tourney l) (tuple <$> rand l <*> rand l)) !! 4
    where tuple a b = (a, b)

--a>b
tourney :: Int -> (Int, Int) -> IO (Int, Int)
tourney range (a, b) = choose <$> (rand range :: IO Int)
    where choose n 
            |n > a = (n, a)
            |n == a = (a, a-1)
            |n > b = (a, n)
            |otherwise = (a, b)

iterateM :: (Monad m) => (a -> m a) -> m a -> [m a]
iterateM f = iterate (f =<<)

--LOSES INFORMATION, DOES NOT REBUILD TREE
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

genRandomTree :: Int -> IO Node
genRandomTree depth
    |depth < 3 = (rand 1 :: IO Float) >>= gen
    |otherwise = (rand 0.3 :: IO Float) >>= gen
    where gen n
            |n > 0.3 = OP <$> randType <*> tree1 <*> tree2 <*> return depth <*> (updateSize <$> tree1 <*> tree2)
            |n > 0.15 = Const <$> (rand 10 :: IO Float) <*> return depth <*> return 1
            |otherwise = Var <$> randChar 'Z' <*> return depth <*> return 1
          tree1 = genRandomTree (depth + 1)
          tree2 = genRandomTree (depth + 1)

updateSize :: Node -> Node -> Size
updateSize n1 n2 = 1 + getSize n1 + getSize n2

getSize :: Node -> Size
getSize (OP _ _ _ _ s) = s
getSize (Const _ _ s) = s
getSize (Var _ _ s) = s

mutate :: Node -> IO Node
mutate (OP opType left right depth size) = OP <$> randType <*> return left <*> return right <*> return depth <*> return size
mutate (Const _ depth size) = Const <$> (rand 10 :: IO Float) <*> return depth <*> return size
mutate (Var _ depth size) = Var <$> randChar 'Z' <*> return depth <*> return size

mutateNode :: Node -> IO Node
mutateNode node = (mutateProb <$> (rand 1 :: IO Float)) >>= (\x -> if x then (mutate node) else return node)

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

randNode = genRandomTree 0