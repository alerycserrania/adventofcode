import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput 
  let cycles = foldl (\c i -> c ++ (instr (fst i) (last c) (snd i))) [1] . map parseInput $ input
  let strength = sum 
                  . map (uncurry (*)) 
                  . filter ((==0). (`mod` 40) . (20-) . fst) 
                  $ zip [1..220] cycles
  print (strength)

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