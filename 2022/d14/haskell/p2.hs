import Data.List
import Data.Maybe
import qualified Data.Set 
import System.IO

main = do
  input <- getInput
  let rocks = Data.Set.fromList . concat . map (fillPath . map getCoord . splitOn " -> ") $ input
  let fl = 2 + (maximum . Data.Set.map snd $ rocks)
  print (length (dropSands (500, 0) rocks (Data.Set.empty) fl))

dropSands s rocks sands fl
  | s /= pos = dropSands s rocks (Data.Set.insert pos sands) fl
  | otherwise = Data.Set.insert pos sands
    where pos = dropSand s rocks sands fl

dropSand (sx, sy) rocks sands fl
  | sy + 1 == fl = (sx, sy)
  | not (Data.Set.member (sx, sy + 1) rocks || Data.Set.member (sx, sy + 1) sands) = dropSand (sx, sy + 1) rocks sands fl
  | not (Data.Set.member (sx - 1, sy + 1) rocks || Data.Set.member (sx - 1, sy + 1) sands) = dropSand (sx - 1, sy + 1) rocks sands fl
  | not (Data.Set.member (sx + 1, sy + 1) rocks || Data.Set.member (sx + 1, sy + 1) sands) = dropSand (sx + 1, sy + 1) rocks sands fl
  | otherwise = (sx, sy)

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