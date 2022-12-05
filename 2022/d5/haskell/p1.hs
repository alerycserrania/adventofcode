import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let (stacks, moves) = parseAll . splitInTwo ("") $ input
  print (map head . foldl applyMove stacks $ moves)

applyMove stacks (m, from, to) = [applyMove' (i, x) |  (i, x) <- indexed stacks]
  where
    applyMove' (i, x) 
      | i == from = drop m x 
      | i == to = (reverse (take m (stacks!!(from-1)))) ++ x 
      | otherwise = x

indexed xs = indexed' xs 1
  where 
    indexed' [] n = []
    indexed' ys n = (n, head ys) : indexed' (tail ys) (n+1)

parseAll (s, m) = (parseAllStacks s, parseAllMoves m) 

parseAllMoves [] = []
parseAllMoves (m:ms) = parseMove m : parseAllMoves ms 

parseMove m = (read (s!!1), read (s!!3), read (s!!5)) 
  where s = split ' ' m

split c [] = []
split c ls = fst : split c snd
  where (fst, snd) = splitInTwo c ls

splitInTwo c ls = (take d ls, drop (d+1) ls)
  where dm = (elemIndex c ls)
        d = fromMaybe (length ls) dm

parseAllStacks = map parseCrate . transpose . parseStacks

parseStacks (x:xs) 
  | elem '1' x = []
  | otherwise = parseStacksLine x : parseStacks xs

parseCrate xs
  | null xs = []
  | head xs == "   " = parseCrate (tail xs)
  | otherwise = (head xs)!!1 : parseCrate (tail xs)

parseStacksLine [] = []
parseStacksLine sl = take 3 sl : parseStacksLine (drop 4 sl)

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)