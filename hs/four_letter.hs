-- Pull out few four letter words from dict/words.

import System.IO
import System.Random
import System.Environment

fourLetterWords :: IO (Int, [String])
fourLetterWords = do
  contents <- readFile "/usr/share/dict/words"
  let ws = filter (\x -> ((length x) == 4)) (lines contents)
      len = length ws
  return (len, ws)

getFewWords :: (Int, [String]) -> Int -> IO [String]
getFewWords ws n = do
  gen <- getStdGen
  return $ map (\x -> (snd ws !! x)) (take n $ (randomRs (0, fst ws -1) gen))

doStuff :: String -> IO ()
doStuff x = do
  let n = read x :: Int
  ws <- fourLetterWords
  sel <- getFewWords ws n
  putStrLn $ unwords sel
  return ()

main = do
  getArgs >>= (\x:xs -> doStuff x)
