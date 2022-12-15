import System.IO

main = do
  input <- getInput
  let sandb = map parseLine input
  let borders =  concat . map (uncurry border) $ sandb
  let outsider = filter (\pos -> all (not . uncurry (collision pos)) sandb) borders
  print (outsider)


border (sx, sy) (bx, by) = do
  let mdist = manhattan (sx, sy) (bx, by)
  let left = (sx - mdist - 1, sy)
  let right = (sx + mdist + 1, sy)
  let down = (sx, sy + mdist + 1)
  let up = (sx, sy - mdist - 1)
  (diag left up) ++ (diag up right) ++ (diag right down) ++ (diag down left)

diag (x1, y1) (x2, y2) = zip (fill x1 x2) (fill y1 y2)

fill a b 
  | a < b = [(max a 0)..(min (b-1) 4_000_000)]
  | otherwise = reverse $ [(max (b+1) 0)..(min a 4_000_000)]

collision (px, py) (sx, sy) (bx, by) = manhattan (px, py) (sx, sy) <= manhattan (bx, by) (sx, sy)

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