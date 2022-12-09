import Data.Char
import Data.List
import System.IO

main = do
  input <- getInput
  let m = map (\x -> map digitToInt x) input
  let views = ([map toVisible m,
               transpose . map toVisible $ (transpose m),
               map (reverse . toVisible . reverse) m,
               transpose . map (reverse . toVisible . reverse) $ (transpose m)])

  print (length . filter id . concat . foldl1 orMatrix $ views)


toVisible xs = toVisible' xs (-1)
  where
    toVisible' [] _ = []
    toVisible' (t:ts) highest 
      | t > highest = True : toVisible' ts t
      | otherwise = False : toVisible' ts highest

orMatrix [] [] = []
orMatrix (x:xs) (y:ys) = orList x y : orMatrix xs ys

orList [] [] = []
orList (x:xs) (y:ys) = (x || y) : orList xs ys

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)