-- Most Stuff from http://learnyouahaskell.com
-- CC BY-NC-SA-3.0

module MyModule 
(
myPart',
Point (..),
Shape (..)
) 

where

myPart' :: (a -> Bool) -> [a] -> ([a],[a])
myPart' p xs = foldr sortPred ([],[]) xs 
    where
      sortPred x a
          | (p x) = (x:(fst a), snd a)
          | otherwise = (fst a, x:(snd a))

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rect Point Point deriving (Show)
