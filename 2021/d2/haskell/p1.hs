import Data.List
import Data.Maybe
import System.IO

main = do
  input <- getInput
  let final = foldl applyCommand (0, 0) . map parseCommand $ input
  print (result final) 

result (p, d) = p * d

applyCommand (p, d) ("forward", x)  = (p + x, d)
applyCommand (p, d) ("down", x)  = (p, d + x)
applyCommand (p, d) ("up", x) = (p, d - x)

parseCommand c = (fst, read snd :: Int)
  where (fst, snd) = splitAt (fromJust (elemIndex ' ' c)) c

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)