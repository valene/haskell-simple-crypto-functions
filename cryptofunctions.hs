module Cryptofunctions ( rlEncode,
                         rlDecode,
                         hammingDistance
                        )
  where

import Data.List (group,groupBy,sortBy)
import Data.Char (isDigit, isAlpha,digitToInt)
import Data.Map (toList, fromListWith)

rlEncode :: String -> String
rlEncode "" = ""
rlEncode xs = concat . map(\x -> rlencode' x) . group $ xs
  where
    rlencode' xss = if length xss == 1 then [head xss] else (show . length $ xss) ++ [head xss]


rlDecode :: String -> String
rlDecode "" = ""
rlDecode xs = rldecode' 0 xs
  where
    rldecode' _ [] = []
    rldecode' 0 (x:xs) | isAlpha x = x : rldecode' 0 xs
    rldecode' n (x:xs) | isAlpha x = (replicate n x) ++ rldecode' 0 xs
    rldecode' n (x:xs) | isDigit x = rldecode' (n*10 + digitToInt x) xs

hammingDistance :: Integral a => String -> String -> Either String a
hammingDistance [] [] = Right 0
hammingDistance _ [] = Left "unequal lengths"
hammingDistance [] _ = Left "unequal lengths"
hammingDistance (x:xs) (y:ys) | x /= y = (+ 1) <$> hammingDistance xs ys
                              | otherwise = hammingDistance xs ys


sortCountTuple :: Integral a => String -> [(Char,a)]
sortCountTuple = sortBy(\(_,x) (_,y) -> compare y x) . toList . fromListWith(+) . map(\x -> (x,1))

sortCountN_ :: Fractional a=> String -> Either String [(Char,a)]
sortCountN_ [] = Left "Empty string"
sortCountN_ xs = case totCountN_' xs of
                      0 -> Left "No alpha chars"
                      totlen -> Right (map(\(x,y) -> (x,(fromIntegral y/ fromIntegral totlen))) $ sortCountTuple xs)
  where
    totCountN_' xs = foldr (\(a,x) y -> if isAlpha a then x+y else y) 0 $ sortCountTuple  xs


indexOfCoin :: Fractional a => String -> Either String a
indexOfCoin [] = Left "Empty String Provided"
indexOfCoin xs = case totCountN_' xs of
                      0 -> Left "No Alpha chars found in String"
                      totlen -> Right (foldr (\(_,x) y -> y+(fromIntegral (x*(x-1))/ fromIntegral (totlen*(totlen-1)))) 0 . filter (\(a,_) -> isAlpha a) $ sortCountTuple xs) 
  where
    totCountN_' xs = foldr (\(a,x) y -> if isAlpha a then x+y else y) 0 $ sortCountTuple xs

