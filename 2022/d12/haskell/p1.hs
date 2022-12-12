import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO

main = do
  input <-getInput
  let posE = findInMatrix 'E' input 0
  let posS = findInMatrix 'S' input 0
  let graph = toMapOfList (toArc input) $ (Map.fromList [])
  let distances = (bfs [posS] Map.empty 0 graph)
  print (distances Map.! posE)

findInMatrix c (m:ms) row
  | isJust p = (row, fromJust p)
  | otherwise = findInMatrix c (ms) (row+1)
    where p = elemIndex c m

bfs q e d g 
  | null q = e
  | otherwise = do
    let newQ = Set.toList (Set.difference (Set.unions (map (g Map.!) q)) (Map.keysSet e))
    let newE = Map.union (e) (Map.fromList ((map (\n -> (n, d)) q)))
    bfs newQ newE (d+1) (g)

toMapOfList [] m = m
toMapOfList ((k, v):ls) m 
  | Map.member k m = toMapOfList ls (Map.adjust (Set.insert v) k m)
  | otherwise = toMapOfList ls (Map.insert k (Set.fromList [v]) m)

toArc m = concat [getArc row col | row <- [0..(length m)-1], col <- [0..(length (m!!row))-1]]
  where 
    getArc row col = (getLeftArc row col) ++ (getRightArc row col) ++ (getUpArc row col) ++ (getDownArc row col)
    getUpArc row col
      | row > 0 = reachable (row-1, col) (row, col)
      | otherwise = []
    getDownArc row col
      | row < (length m) - 1 = reachable (row+1, col) (row, col)
      | otherwise = []
    getLeftArc row col
      | col > 0 = reachable (row, col-1) (row, col)
      | otherwise = []
    getRightArc row col
      | col > (length (m!!row)) - 1 = reachable (row, col+1) (row, col)
      | otherwise = []
    reachable (rowA, colA) (rowB, colB) =
      (if isAtMostOneStepAbove (m!!rowA!!colA, m!!rowB!!colB) then [((rowA, colA), (rowB, colB))] else [])
      ++ (if isAtMostOneStepAbove (m!!rowB!!colB, m!!rowA!!colA) then [((rowB, colB), (rowA, colA))] else [])
    isAtMostOneStepAbove (c1, c2) = (ord (parseChar c1) + 1) >= ord (parseChar c2)
      where 
        parseChar 'S' = 'a'
        parseChar 'E' = 'z'
        parseChar c = c 


getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)