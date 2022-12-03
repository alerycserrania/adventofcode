import Data.Char
import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let compartments = map splitInTwo input
  let commons = map commonChar compartments
  let priorities  = map (+1) . map fromJust . map priority . map fromJust $ commons
  print (sum priorities)

priority c = elemIndex c (['a'..'z'] ++ ['A'..'Z'])

commonChar (s1, s2) 
  | null s2 = Nothing
  | elem first s2 = Just first
  | otherwise = commonChar (rest, s2)
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