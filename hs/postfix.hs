-- Most Stuff from http://learnyouahaskell.com
-- CC BY-NC-SA-3.0

import System.IO
import System.Environment
import Control.Exception

doRpn :: Num a => [String] -> Maybe a 
doRpn [] = Nothing

data RpnOp = Plus | Minus | Divide | Multiply deriving (Eq)

instance Show RpnOp where
    show Plus = "+"
    show Minus = "-"
    show Divide = "/"
    show Multiply = "*"

toOp :: String -> Maybe RpnOp
toOp [] = Nothing
toOp x 
    | x == "+" = Just Plus
    | x == "-" = Just Minus
    | x == "/" = Just Divide
    | x == "*" = Just Multiply
    | otherwise = Nothing

unFuse :: Read a => String -> [Either (Either RpnOp a) String]
unFuse [] = []
unFuse (x:xs) = if op == Nothing
                then (let nv = reads (x:xs) in
                      if ((length nv) == 0) 
                      then [Right ""]
                      else ([(Left (Right (fst (nv !! 0))))] ++ (unFuse (snd (nv !! 0)))))
                else [Left (Left (maybe Plus id op))] ++ (unFuse xs)
    where op = toOp [x]

rpnTokens :: Read a => [String] -> [Either (Either RpnOp a) String]
rpnTokens xs = foldr (\f r -> (unFuse f) ++ r) [] xs

wordS :: [String] -> [String]
wordS = foldr (\f r -> (words f) ++ r) []

main = do
  xs <- getArgs
  print (rpnTokens (wordS xs) :: [Either (Either RpnOp Int) String])
  return ()
