import Data.Maybe
import Data.List
import System.IO
import System.Environment

main = do 
  input <- getInput
  args <- getArgs
  let available = read (args!!0) :: Int
  let required = read (args!!1) :: Int
  let paths = map reverse . parseFileSystem input $ []
  let disk = (addSize (foldl insertPath (head . listToTree . head $ paths) paths))
  let dirs = (filter ((=='d') . first) (flatten disk))
  let unused = available - (third (value disk))
  print (minimum . filter ((>required) . ((+)unused)) . map third $ dirs)
  

data Tree a = Node a [Tree a] deriving (Show)
value (Node v _) = v
children (Node _ c) = c
flatten (Node v c) 
  | null c = [v]
  | otherwise = [v] ++ (flatten (head c)) ++ concat (map flatten (tail c))

addSize (Node (name, size) children)
  | isJust size = (Node ('f', name, fromJust size) cr)
  | otherwise = (Node ('d', name, sum l) cr)
    where l = (map (\t -> third . value $ t) cr)
          cr = (map addSize children)

first (a, _, _) = a
third (_, _, c) = c

listToTree [] = []
listToTree xs = [Node (head xs) (listToTree' (tail xs))]
  where 
    listToTree' ns
      | null ns = []
      | otherwise = [Node (head ns) (listToTree' (tail ns))]

insertPath node path = (Node (head path) (mergeChildren (children node) (tail path)))
  where 
    mergeChildren c p
      | null c = listToTree p
      | (value (head c)) == (head p) = insertPath (head c) (p) : tail c
      | otherwise = (head c) : mergeChildren (tail c) p

parseFileSystem input wd
  | null input = []
  | cmd == "$ cd" = cd rest wd (tail input)
  | cmd == "$ ls" = files ++ next 
    where 
      cmd = take 4 (head input)
      rest = drop 5 (head input)
      files = map (:wd) . map (toFile) . takeWhile ((/='$') . head) $ (tail input)
      next = parseFileSystem (dropWhile ((/='$') . head) $ (tail input)) wd

cd name wd next 
  | name == ".." = parseFileSystem (next) (tail wd)
  | otherwise = (dir : wd) : parseFileSystem next (dir : wd)
    where dir = (name, Nothing)

toFile f 
  | t == "dir" = (n, Nothing)
  | otherwise = (n, Just (read t :: Int))
    where (t, n) = (take sp f, drop (sp+1) f)
          sp = fromJust (elemIndex ' ' f)

getInput = do
  done <- isEOF
  if done
    then return []
    else do
      line <- getLine
      rest <- getInput
      return (line : rest)