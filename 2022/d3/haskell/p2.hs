import Data.Char
import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let groups = groupsOf 3 input
  let commons = map (\c -> (head . head) c) . map commonCharsInList $ groups
  let priorities = (map (+1)) . (map fromJust) . (map priority) $ commons
  print (sum priorities)

priority c = elemIndex c (['a'..'z'] ++ ['A'..'Z'])

groupsOf n [] = []
groupsOf n l = (take n l) : (groupsOf n (drop n l))

commonCharsInList l 
  | length l == 1 = l
  | otherwise = commonCharsInList ((commonChars pair) : rest) 
    where (start, rest) = splitAt 2 l
          pair = (head start, last start)

commonChars (s1, s2) 
  | null s2 || null s1 = []
  | elem first s2 = (first : commonChars (rest, s2)) 
  | otherwise = commonChars (rest, s2)
  where (first : rest) = s1

splitInTwo l = splitAt (div (length l) 2) l

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)