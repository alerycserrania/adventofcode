import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import System.IO


main = do
  input <- getInput
  let moves = concat (map parseInput input)
  let knots = (replicate 10 (0, 0))
  let d = S.fromList . map (!!9) . scanl move knots $ moves
  print (length d)


move (h:ts) dir = nh : moveTails nh ts
  where nh = moveHead h dir 

moveTails _ [] = []
moveTails h ts = nh : moveTails nh (tail ts) 
  where nh = moveTail (head ts) h

moveHead (hx, hy) 'L' = (hx-1, hy)
moveHead (hx, hy) 'R' = (hx+1, hy)
moveHead (hx, hy) 'U' = (hx, hy+1)
moveHead (hx, hy) 'D' = (hx, hy-1)

moveTail (tx, ty) (hx, hy)
  | isAdjacent (tx, ty) (hx, hy) = (tx, ty)
  | fst d == 0 = (tx, ty+try)
  | snd d == 0 = (tx+trx, ty)
  | otherwise = (tx+trx, ty+try)
  where 
    d = diff (hx, hy) (tx, ty)
    trx = if fst d > 0 then 1 else -1
    try = if snd d > 0 then 1 else -1

diff (x1, y1) (x2, y2) = (x1-x2, y1-y2)

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