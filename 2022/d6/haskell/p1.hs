import Data.Maybe
import Data.List (null, take)
import Data.Set (empty, member, insert)
import System.IO

main = do
  input <- getInput
  print (map (posPacketMarker) input)

posPacketMarker = posMarker 4

posMarker n s = posMarker' s 0
  where
    posMarker' s i
      | length s < n = Nothing
      | noDup (take n s) = Just (i + n)
      | otherwise = posMarker' (tail s) (i+1)

noDup l = noDup' l empty
  where
    noDup' l u
      | null l = True
      | member (head l) u = False
      | otherwise = noDup' (tail l) (insert (head l) u)

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)