-- DO NOT MODIFY THIS FILE

import qualified Graph
import qualified Assignment4

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install QuickCheck

import Test.QuickCheck -- see https://hackage.haskell.org/package/QuickCheck for
                       -- documentation if you want to write your own tests

import Test.QuickCheck.Monadic

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install silently

import System.IO.Silently(capture_)

import Control.Exception(bracket,finally)
import Control.Monad(liftM)
import Data.Either(lefts,rights)
import Data.List(intercalate,nub,sort)
import GHC.IO.Handle(hClose,hDuplicate,hDuplicateTo)
import System.IO(hPutStrLn,IOMode(ReadMode),stdin,withFile)

-- Tests

-- Exercise 1

prop_Exercise1 = not Assignment4.answer11 &&
                 Assignment4.answer12 &&
                 not Assignment4.answer13 &&
                 not Assignment4.answer14 &&
                 Assignment4.answer15

-- Exercise 2

_ = Assignment4.reverseIO :: IO ()

prop_Exercise2 :: [ASCIIString] -> Property
prop_Exercise2 ss = monadicIO $ do
  let ss' = filter (\s -> not (null s) && not ('\n' `elem` s)) $ map getASCIIString ss
  run $ writeFile "input.txt" $ unlines (ss' ++ [""])
  output <- run $ withFile "input.txt" ReadMode $ \handle -> bracket
    (hDuplicate stdin)
    (\h -> hDuplicateTo h stdin `finally` hClose h)
    (const $ hDuplicateTo handle stdin >> capture_ Assignment4.reverseIO)
  assert (ss' == reverse (lines output))

-- Exercise 3

_ = Graph.empty :: Graph.Graph a
graphAddVertex :: Eq a => Graph.Graph a -> a -> Graph.Graph a
graphAddVertex = Graph.addVertex
graphAddEdge :: Eq a => Graph.Graph a -> (a,a) -> Graph.Graph a
graphAddEdge = Graph.addEdge
graphVertices :: Eq a => Graph.Graph a -> [a]
graphVertices = Graph.vertices
graphNeighbors :: Eq a => Graph.Graph a -> a -> [a]
graphNeighbors = Graph.neighbors

{- When building an "arbitrary" graph, we make sure that
     1. vertices are added before any edges that contain them, and
     2. no edge is of the form (u,u).
   Duplicate vertices and edges, as well as edges of the form (u,v) and (v,u),
   are permitted in `GraphBuilder'.
 -}
data GraphBuilder a = GraphBuilder [Either a (a,a)]

buildGraph :: Eq a => GraphBuilder a -> Graph.Graph a
buildGraph (GraphBuilder xs) = foldl (\g x ->
                                        case x of
                                          Left v -> Graph.addVertex g v
                                          Right edge -> Graph.addEdge g edge) Graph.empty xs

instance Show a => Show (GraphBuilder a) where
  show (GraphBuilder xs) = "[" ++ (intercalate ", " $ "Graph.empty" : map (\x ->
                                                                             case x of
                                                                               Left v -> "addVertex " ++ show v
                                                                               Right edge -> "addEdge " ++ show edge) xs) ++ "]"

instance (Arbitrary a, Eq a) => Arbitrary (GraphBuilder a) where
  arbitrary =
    sized arbitrarySizedGraphBuilder

arbitrarySizedGraphBuilder :: (Arbitrary a, Eq a) => Int -> Gen (GraphBuilder a)
arbitrarySizedGraphBuilder m = do
  n <- chooseInt (0, m)  -- (upper bound on the) number of vertices
  e <- chooseInt (0,  n * (n-1) `div` 2)  -- (upper bound on the) number of edges
  vs <- liftM nub (vectorOf n arbitrary)
  let es = [ (u,v) | u <- vs, v <- vs, u /= v ]
  xs <- shuffleVerticesEdges vs es n e []
  return $ GraphBuilder xs

shuffleVerticesEdges :: Eq a => [a] -> [(a,a)] -> Int -> Int -> [a] -> Gen [Either a (a,a)]
shuffleVerticesEdges _ _ 0 0 _ = return []
shuffleVerticesEdges vs es n e seen = do
  let es' = filter (\(u,v) -> u `elem` seen && v `elem` seen) es
  let vs_es' = (if n > 0 then map Left vs else []) ++ (if e > 0 then map Right es' else [])
  if null vs_es'
    then return []
    else do
      x <- elements vs_es'
      liftM (x:) $ case x of
        Left v -> shuffleVerticesEdges vs es (n-1) e (v:seen)
        Right _ -> shuffleVerticesEdges vs es n (e-1) seen

graphBuilderVertices :: Eq a => GraphBuilder a -> [a]
graphBuilderVertices (GraphBuilder xs) = nub $ lefts xs

graphBuilderNeighbors :: Eq a => GraphBuilder a -> a -> [a]
graphBuilderNeighbors (GraphBuilder xs) v = nub $ foldl collectNeighbor [] (rights xs) where
  collectNeighbor ns (a,b) | a==v = b:ns
                           | b==v = a:ns
                           | otherwise = ns

prop_Exercise3_vertices :: GraphBuilder Int -> Bool
prop_Exercise3_vertices gb =
  let g = buildGraph gb
  in
    sort (Graph.vertices g) == sort (graphBuilderVertices gb)

prop_Exercise3_neighbors :: GraphBuilder Int -> Bool
prop_Exercise3_neighbors gb =
  let g = buildGraph gb
  in
    all (\v -> sort (Graph.neighbors g v) == sort (graphBuilderNeighbors gb v)) (graphBuilderVertices gb)

-- Exercise 4

a4connected :: Eq a => Graph.Graph a -> a -> a -> Bool
a4connected = Assignment4.connected

graphBuilderConnected :: Eq a => GraphBuilder a -> a -> a -> Bool
graphBuilderConnected gb u v = bfs [u] []
  where
    bfs [] _ = False
    bfs (x:xs) seen | x==v = True
                    | x `elem` seen = bfs xs seen
                    | otherwise = bfs (xs ++ graphBuilderNeighbors gb x) (x : seen)

prop_Exercise4 :: GraphBuilder Int -> Bool
prop_Exercise4 gb =
  let g = buildGraph gb
      vs = graphBuilderVertices gb
  in
    all (\(u,v) -> Assignment4.connected g u v == graphBuilderConnected gb u v) [ (u,v) | u <- vs, v <- vs ]

-- testing on a graph with two components, each of which is the complete graph K_n
prop_Exercise4_KK :: Positive Int -> Bool
prop_Exercise4_KK (Positive n) =
  let vs1 = [0..n-1]
      es1 = [ (u,v) | u <- vs1, v <- vs1, u < v ]
      vs2 = [n..2*n-1]
      es2 = [ (u,v) | u <- vs2, v <- vs2, u < v ]
      gb = GraphBuilder $ map Left (vs1 ++ vs2) ++ map Right (es1 ++ es2)
      g = buildGraph gb
  in
    Assignment4.connected g 0 (2*n-1) == graphBuilderConnected gb 0 (2*n-1)

-- main

main = do
  putStrLn "Exercise 1:"
  quickCheck prop_Exercise1
  putStrLn "Exercise 2:"
  quickCheck prop_Exercise2
  putStrLn "Exercise 3:"
  putStr "vertices: "
  quickCheck (withMaxSuccess 50 prop_Exercise3_vertices)
  putStr "neighbors: "
  quickCheck (withMaxSuccess 50 prop_Exercise3_neighbors)
  putStrLn "Exercise 4:"
  putStr "Random graphs: "
  quickCheck (withMaxSuccess 50 prop_Exercise4)
  putStr "K_n + K_n: "
  quickCheck prop_Exercise4_KK
