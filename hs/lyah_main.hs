-- Most Stuff from http://learnyouahaskell.com
-- CC BY-NC-SA-3.0

import Control.Monad
import Data.Char

myPutStr :: String -> IO ()
myPutStr [] = return ()
myPutStr (x:xs) = do
  putChar x
  myPutStr xs

myPutStrLn :: String -> IO ()
myPutStrLn xs = do
  myPutStr xs
  putChar '\n'

myPrint :: Show a => a -> IO ()
myPrint = myPutStrLn . show

mapM' :: Monad m => (a -> m b) -> [a] -> m ()
mapM' x y = do
  mapM x y
  return ()

ex2 = do
  contents <- getContents
  mapM putStrLn (map (\x -> map toUpper x) (lines contents))
  return ()

ex3 = do
  contents <- getContents
  mapM putStrLn (filter (\x -> (length x) < 10) (lines contents))
  return ()

main = interact (\x -> (unlines.map (\y -> if y == (reverse y) then "Pal" else "NoPal") $ (lines x)))
