import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let listOfCalories = map listStrToInt (splitOn "" input)
  print $ sum $ nMaximum 3 $ map sum listOfCalories

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c l = do
    let d = length (takeWhile (/= c) l)
    let (fst, scd) = splitAt d l
    fst : if scd == [] then [] else splitOn c (tail scd)

listStrToInt l
  | null l = []
  | otherwise = (read (head l) :: Int) : listStrToInt (tail l)

nMaximum 0 _ = []
nMaximum _ [] = []
nMaximum n l = 
  m : nMaximum (n-1) (removeAt d l)
  where 
    m = maximum l
    d = fromJust (elemIndex m l)

removeAt i l = left ++ right
  where (left, _:right) = splitAt i l

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)
