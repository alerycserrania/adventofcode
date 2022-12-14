import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let rocks = concat . map (fillPath . map getCoord . splitOn " -> ") $ input
  print (length (dropSands (500, 0) rocks []))

dropSands s rocks sands
  | isResting = dropSands s rocks (pos : sands)
  | otherwise = sands
    where (pos, isResting) = dropSand s rocks sands

dropSand (sx, sy) rocks sands 
  | isBelow (sx, sy) rocks = ((sx, sy), False)
  | not (elem (sx, sy + 1) rocks || elem (sx, sy + 1) sands) = dropSand (sx, sy + 1) rocks sands
  | not (elem (sx - 1, sy + 1) rocks || elem (sx - 1, sy + 1) sands) = dropSand (sx - 1, sy + 1) rocks sands
  | not (elem (sx + 1, sy + 1) rocks || elem (sx + 1, sy + 1) sands) = dropSand (sx + 1, sy + 1) rocks sands
  | otherwise = ((sx, sy), True)

isBelow (sx, sy) rocks = all (\(rx, ry) -> sy >= ry) rocks

fillPath (a:[]) = []
fillPath (a:b:ps) = (fillPath' a b) ++ (fillPath (b:ps))
  where 
    fillPath' (x1, y1) (x2, y2)
      | x1 < x2 = map (\x -> (x, y1)) [x1..x2]
      | x1 > x2 = map (\x -> (x, y1)) [x2..x1]
      | y1 < y2 = map (\y -> (x1, y)) [y1..y2]
      | otherwise =  map (\y -> (x1, y)) [y2..y1]


getCoord str = (read x :: Int, read y :: Int)
  where (x:y:_) = splitOn "," str

splitOn sub str 
  | isJust mpos = take pos str : splitOn sub (drop (pos + length sub) str)
  | otherwise = [str]
  where mpos = findString sub str
        pos = fromJust mpos

findString search str = findIndex (isPrefixOf search) (tails str)

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)