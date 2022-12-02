import System.IO

main = do
  input <- getInput
  print ((sum . map score . map parseInput) input)

score (h1, h2) = (beats h2 h1) + (shapeValue h2)

data Hand = R | P | S deriving (Show, Eq)

beats x y 
  | x == y = 3
  | (winAgainst x) == y = 0
  | (loseAgainst x) == y = 6

winAgainst R = P
winAgainst P = S
winAgainst S = R

loseAgainst R = S
loseAgainst P = R
loseAgainst S = P

shapeValue R = 1
shapeValue P = 2
shapeValue S = 3

charToHand 'A' = R
charToHand 'X' = R
charToHand 'B' = P
charToHand 'Y' = P
charToHand 'C' = S
charToHand 'Z' = S

parseInput s = (charToHand (head s), charToHand (last s))

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)