import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  print (length . filter overlap . map toNum . map parseInput $ input)

overlap (a, b) = overlap' (a, b) || overlap' (b, a)
overlap' ((a1, a2), (b1, b2)) = a1 <= b1 && a2 >= b2 

toNum ((a1, a2), (b1, b2)) = ((na1, na2), (nb1, nb2))
  where na1 = read a1 :: Int
        na2 = read a2 :: Int
        nb1 = read b1 :: Int
        nb2 = read b2 :: Int

parseInput s = (splitPair '-' p1, splitPair '-' p2)
  where (p1, p2) = splitPair ',' s

splitPair c s = (take pc s, drop (pc+1) s)
  where pc = fromJust (elemIndex c s)

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)