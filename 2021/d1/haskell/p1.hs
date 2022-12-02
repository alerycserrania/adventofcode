import System.IO

main = do
  input <- getInput
  let depths = map parseInput input
  let diffs = map (uncurry subtract) $ zip depths (tail depths)
  print (length $ filter (>0) diffs)

parseInput d = read d :: Int

getInput = do
  eof <- isEOF
  if eof
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)