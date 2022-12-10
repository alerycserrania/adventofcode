import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput 
  let cycles = foldl (\c i -> c ++ (instr (fst i) (last c) (snd i))) [1] . map parseInput $ input
  let visibility = map (\(p, x) -> (p >= x - 1) && (p <= x + 1)) . map (\(p, x) -> (mod p 40, x)) $ zip [0..] cycles
  let drawing = map (\x -> if x then '#' else '.') visibility
  putStr (unlines (chunksOf 40 drawing))

chunksOf n [] = []
chunksOf n ls = take n ls : chunksOf n (drop n ls)

parseInput i 
  | isJust p = (f, [read s :: Int])
  | otherwise = (i, [])
    where p = (elemIndex ' ' i)
          (f, s) = splitAt (fromJust p) i

instr "addx" = addx
instr "noop" = noop
addx x args = [x, x + (head args)]
noop x _ = [x]

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)