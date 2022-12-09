import Data.Char
import Data.List
import System.IO

main = do
  input <- getInput
  let m = map (\x -> map digitToInt x) input
  let views = [[(nbVisible (dir d row col m) (m!!row!!col)) | d <- ["up", "down", "left", "right"]] | row <- [0..(length m)-1], col <- [0..(length (m!!row)-1)]]
  print (maximum (map product views))

dir "up" row col mat = dir "left" col row (transpose mat)
dir "down" row col mat = dir "right" col row (transpose mat)
dir "left" row col mat = reverse . map snd . takeWhile ((<col) . fst) . enumerate $ (mat!!row)
dir "right" row col mat = map snd . dropWhile ((<=col) . fst) . enumerate $ (mat!!row)

enumerate xs = enumerate' xs 0
  where 
    enumerate' xs' i
      | null xs' = []
      | otherwise = (i, head xs') : enumerate' (tail xs') (i+1)

nbVisible xs n 
  | null xs = 0
  | n > (head xs) = 1 + nbVisible (tail xs) n
  | otherwise = 1

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)