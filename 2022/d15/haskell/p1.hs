
import System.IO

main = do
  input <- getInput
  let sandb = map parseLine input
  let left = minimum . map (\(l, r, u, d) -> l) . map limitRect $ sandb
  let right = maximum . map (\(l, r, u, d) -> r) . map limitRect $ sandb
  
  let collisions = map (\x -> any (\(s, b) -> collision (x, 2_000_000) s b) sandb ) [left..right]
  print (length . filter (id) $ collisions)


limitRect sandb = (limitLeft, limitRight, limitUp, limitDown)
  where 
    limitLeft = ((fst . fst) sandb) - mdist
    limitRight = ((fst . fst) sandb) + mdist
    limitUp = ((snd . fst) sandb) - mdist
    limitDown = ((snd . fst) sandb) + mdist
    mdist = (uncurry manhattan) sandb


collision (px, py) (sx, sy) (bx, by) = 
  manhattan (px, py) (sx, sy) <= manhattan (bx, by) (sx, sy)
  && (px, py) /= (bx, by)
  && (px, py) /= (sx, sy)

manhattan (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

parseLine line = do
  let a =  dropWhile (/='x') $ line
  let sensor = parsePosition (takeWhile (/=':') $ a)
  let beacon = parsePosition (dropWhile (/='x') . drop 1 $ a)
  (sensor, beacon)

parsePosition pos = do
  let x = takeWhile (/=',') . drop 2 $ pos
  let y = drop 2 . dropWhile (/='y') $ pos
  (read x :: Int, read y :: Int)

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)