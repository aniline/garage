-- Most Stuff while going through from http://learnyouahaskell.com
-- CC BY-NC-SA-3.0

import Data.List
import Data.Char
import Data.Monoid
import qualified Data.ByteString as B
import Control.Applicative

sayMe :: (Show a, Integral a) => a -> String
sayMe 1 = "Uno"
sayMe 2 = "Dos"
sayMe x = show x

fact :: (Integral a) => a -> a
fact 0 = 1
fact x = x * fact (x-1)

len' :: (Integral b) => [a] -> b
len' [] = 0
len' (_:xs) = 1 + len' xs

bTell :: (Integral a) => a -> String
bTell x
  | x == 1 = "One"
  | x == 2 = "Two"
  | otherwise = "Others"

tell' :: (Show a) => [a] -> String
tell' xs = case xs of [] -> "Empty"
                      [x] -> "One"
                      [x,y] -> "Two"
                      (x:y:_) -> "More"

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

max' :: (Ord a) => [a] -> a
max' [] = error "Empty list"
max' [x] = x
max' (x:xs) 
    | pmax > x = pmax
    | otherwise = x
    where pmax = max' xs

cmp' :: (Ord a) => a -> a -> Ordering
a `cmp'` b | a > b = GT | a < b = LT | otherwise = EQ

init' :: String -> String -> String
init' fn ln = let f f' l' = [f' !! 0] ++ ". " ++ [l' !! 0] ++ "." in f fn ln

-- fib :: (Integral a, Integral b) => a -> b
-- fib 0 = [1]
-- fib 1 = [1, 1]
-- fib x = let pxs = fib (x-1) in 

rep' :: Int -> Int -> [Int]
rep' 0 _ = []
rep' n x = (x:(rep' (n-1) x))

take' :: Int -> [Int] -> [Int]
take' n _
    | n <=0 = []
take' _ [] = []
take' n xs = let (x:ys) = xs in (x: take' (n-1) ys)

repinf' :: Int -> [Int]
repinf' x = x:repinf' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | x == a = True
    | otherwise = elem' a xs

qsort' :: (Ord a) => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' [ y | y <- xs, y <= x ] ++ [x] ++ qsort' [ y | y <- xs, y > x ]

---

cMult a b c = a * b * c
cCmp100 :: (Num a, Ord a) => a -> Ordering
cCmp100 = compare 100

isUpper':: Char -> Bool
isUpper' = (`elem` ['A'..'Z'])

doTwice' :: (a -> a) -> a -> a
doTwice' f x = f (f x)

collCh :: (Integral a) => a -> [a]
collCh 1 = [1] 
collCh n 
       | odd n = n:collCh (n*3 + 1)
       | even n = n:collCh (div n 2)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

vow' :: Char -> Bool
vow' x = x `elem` "AEIOUaeiou"

-- area :: Shape -> Float
-- area (Circle _ r) = pi * r^2
-- area (Rect (Point l t) (Point r b)) = (*) (abs (r - l)) (abs (b - t))

-- xlat :: Shape -> Float -> Float -> Shape
-- xlat (Circle (Point x y) r) a b  = Circle (Point (x + a) (y + b)) r

data Person = Person {
      fn :: String,
      ln :: String,
      age :: Int,
      height :: Float, 
      phone :: String,
      flavor :: String } deriving (Show)

data Vec a = Vec a a a deriving (Show)

vplus :: (Num t) => Vec t -> Vec t -> Vec t
vplus (Vec x y z) (Vec a b c) = Vec (a Prelude.+ x) (b Prelude.+ y) (c Prelude.+ z)

data Frank a b = Frank { ff :: b a } deriving (Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- Functor instance is definition of "fmap function Stuff -> Stuff
-- Functor array is fmap taking funciton and array giving an array

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust c x) = CJust (c+1) (f x)

-- Functors: Partially applied functions, arrays, Maybe
-- Applicative functors --

newtype CharList = CharList { getCharList :: [Char] } deriving (Show, Eq)

newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

-- data CoolBool = CoolBool { getCoolBool :: Bool } deriving Show
-- Possible multiple data constructors.

newtype CoolBool = CoolBool { getCoolBool :: Bool } deriving Show

hiCoolBool :: CoolBool -> String
hiCoolBool (CoolBool _) = "hello"

-- isBigGang :: Int -> Bool
-- isBigGang x = x > 9

isBigGang :: Int -> (Bool, [Char])
isBigGang x
    | x > 9 = (True, "Compared to 9")
    | otherwise = (False, "Compared to 9")

-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog (x, log) f = (\(a, b) -> (a, log ++ b)) (f x)

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = (\(a, b) -> (a, mappend log b)) (f x)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  

-- 
