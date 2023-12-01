import Data.Char
import System.IO

getCalibrationValues lines
  | null lines = []
  | otherwise = getCalibrationValue (getDigits (head lines)) : getCalibrationValues (tail lines)

getDigits :: [Char] -> [Char]
getDigits l
  | null l = []
  | isDigit (head l) = (head l) : getDigits (tail l)
  | otherwise = getDigits (tail l)

getCalibrationValue l = 10 * (digitToInt (head l)) + (digitToInt (last l))

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)

main = do
  input <- getInput
  print $ sum $ getCalibrationValues input
