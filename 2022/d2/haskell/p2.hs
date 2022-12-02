import System.IO

main = do
  input <- getInput
  print ((sum . map score . map parseInput) input)

score (h1, h2) = (beats h2 h1) + (shapeValue h2)

data Hand = R | P | S deriving (Show)

beats R P = 0
beats P R = 6
beats R S = 6
beats S R = 0
beats P S = 0
beats S P = 6
beats _ _ = 3

shapeValue R = 1
shapeValue P = 2
shapeValue S = 3

charToHand 'A' = R
charToHand 'X' = R
charToHand 'B' = P
charToHand 'Y' = P
charToHand 'C' = S
charToHand 'Z' = S

winAgainst R = P
winAgainst P = S
winAgainst S = R

loseAgainst R = S
loseAgainst P = R
loseAgainst S = P

handToChoose x 'Y' = x
handToChoose x 'X' = loseAgainst x
handToChoose x 'Z' = winAgainst x

parseInput s = (h, handToChoose h (last s))
  where h = charToHand (head s)

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)