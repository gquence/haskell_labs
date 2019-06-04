module SimpleEncryption where

import Data.Maybe
import Data.Char
import Data.List

encryptWord :: String -> String
encryptWord [] = " "
encryptWord [x]
   | x == ' ' = [x]
   | otherwise = show (ord x)
encryptWord (x : xs)
   | x == ' ' = [x] ++ encryptWord xs
   | length xs == 1 = show (ord x) ++ xs 
   | length xs == 2 =  show (ord x) ++ [last xs] ++ [head xs]
   | otherwise = show (ord x) ++ [last xs] ++ init (tail xs) ++ [head xs]

splitDoubleTuple :: (a, a) -> Int -> a
splitDoubleTuple (x, y) numb
   | numb == 1 = x
   | numb == 2 = y

encryptThis :: String -> String
encryptThis [] = []
encryptThis [x] = encryptWord [x]
encryptThis (x: xs)
   | (elemIndex ' ' (x:xs)) == Nothing = encryptWord (x : xs)
   | (elemIndex ' ' (x:xs)) == Just 0 = [x] ++ encryptThis xs
   | elemIndex ' ' (x:xs) /= Nothing = encryptWord (splitDoubleTuple (splitAt (fromJust (elemIndex ' ' (x : xs))) (x: xs)) 1) ++ encryptThis (splitDoubleTuple (splitAt (fromJust (elemIndex ' ' (x : xs))) (x : xs)) 2)
