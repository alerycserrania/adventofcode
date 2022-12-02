import System.IO

main = do
    input <- getInput
    let depths = map parseInput input
    let windows = map (\(a,b,c) -> a+b+c) $ (zip3 depths (drop 1 depths) (drop 2 depths))
    let diffs = map (uncurry subtract) $ zip windows (tail windows)
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