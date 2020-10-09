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
     1. each vertex is added only once;
     2. each edge is added only once;
     3. vertices are added before any edges that contain them;
     4. at most one of the edges (u,v) and (v,u) is added;
     5. no edge is of the form (u,u).
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
  let esSymm = [ (u,v) | u <- vs, v <- vs, u /= v ]
  es' <- removeSymm esSymm  -- keep only one of (u,v) and (v,u)
  es <- liftM (take e) (shuffle es')
  xs <- shuffleVerticesEdges [] vs es
  return $ GraphBuilder xs
    where
      removeSymm :: Eq a => [(a,a)] -> Gen [(a,a)]
      removeSymm [] = return []
      removeSymm ((u,v):es) = do
        let es' = filter (\edge -> edge /= (u,v) && edge /= (v,u)) es
        bit <- arbitrary
        liftM ((if bit then (u,v) else (v,u)) :) (removeSymm es')
      shuffleVerticesEdges :: Eq a => [a] -> [a] -> [(a,a)] -> Gen [Either a (a,a)]
      shuffleVerticesEdges _ vs [] = return $ map Left vs
      shuffleVerticesEdges _ [] es = return $ map Right es
      shuffleVerticesEdges seen vs es = do
        let es' = filter (\(u,v) -> u `elem` seen && v `elem` seen) es
        x <- elements (map Left vs ++ map Right es')
        liftM (x:) $ case x of
          Left v -> shuffleVerticesEdges (v:seen) (filter (/=v) vs) es
          Right e -> shuffleVerticesEdges seen vs (filter (/=e) es)

graphBuilderVertices :: GraphBuilder a -> [a]
graphBuilderVertices (GraphBuilder xs) = lefts xs

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

-- main

main = do
  putStrLn "Exercise 1:"
  quickCheck prop_Exercise1
  putStrLn "Exercise 2:"
  quickCheck prop_Exercise2
  putStrLn "Exercise 3:"
  putStr "vertices: "
  quickCheck prop_Exercise3_vertices
  putStr "neighbors: "
  quickCheck prop_Exercise3_neighbors
  putStrLn "Exercise 4:"
  quickCheck (withMaxSuccess 50 prop_Exercise4)
