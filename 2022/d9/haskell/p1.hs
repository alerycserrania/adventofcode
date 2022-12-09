import qualified Data.Set as S
import System.IO


main = do
  input <- getInput
  let moves = concat (map parseInput input)
  let d = S.fromList . map snd . scanl move ((0, 0), (0, 0)) $ moves
  print (length d)

move (h, t) dir = (nh, moveTail t nh dir)
  where nh = moveHead h dir

moveHead (hx, hy) 'L' = (hx-1, hy)
moveHead (hx, hy) 'R' = (hx+1, hy)
moveHead (hx, hy) 'U' = (hx, hy+1)
moveHead (hx, hy) 'D' = (hx, hy-1)

moveTail (tx, ty) (hx, hy) dir
  | isAdjacent (tx, ty) (hx, hy) = (tx, ty)
  | otherwise = follow (hx, hy) dir
    where 
      follow (x, y) 'L' = (x+1, y)
      follow (x, y) 'R' = (x-1, y)
      follow (x, y) 'U' = (x, y-1)
      follow (x, y) 'D' = (x, y+1)

isAdjacent (x1, y1) (x2, y2) = elem x1 [x2-1, x2, x2+1] && elem y1 [y2-1, y2, y2+1]


parseInput (dir:' ':num) = map (\_ -> dir) (replicate (read num :: Int) 1) 

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)