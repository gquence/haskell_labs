import Data.List ( elemIndex )
import Data.Maybe ( fromJust )

{--
    Vigenere Encoder
    Example usage: 
    vigenere "abcdefghigklmnop" [1,2,3,4,5] "hello"
    vigenere "abcdefghigklmnop" [-1,2,3,4,5] "hello"
    Example output:
    "igopd"
    "ggopd"
--}

vigenere :: [Char] -> [Int] -> String -> String
vigenere [] [] [] = []
vigenere xs [y] [z] = [ vigenere_elem xs y z ]
vigenere xs (y:ys) (z:zs) = vigenere_elem xs y z : vigenere xs ys zs

vigenere_elem :: [Char] -> Int -> Char -> Char
vigenere_elem [] _ _ = '\0'
vigenere_elem xs y z = cycle xs !! target_index
    where
        target_index = get_index_gt_zero xs (fromJust (elemIndex z xs) + y)

{--
    Vigenere Decoder
    Example usages: 
    unvigenere "abcdefghigklmnop" [1,2,3,4,5] "igopd"
    unvigenere "abcdefghigklmnop" [-1,2,3,4,5] "ggopd"
    Example outputs:
    "hello"
    "hello"
--}

unvigenere :: [Char] -> [Int] -> String -> String
unvigenere [] [] [] = []
unvigenere xs [y] [z] = [ unvigenere_elem xs y z ]
unvigenere xs (y:ys) (z:zs) = unvigenere_elem xs y z : unvigenere xs ys zs

unvigenere_elem :: [Char] -> Int -> Char -> Char
unvigenere_elem [] _ _ = '\0'
unvigenere_elem xs y z = cycle xs !! target_index
    where
        target_index = get_index_gt_zero xs (fromJust (elemIndex z xs) - y)

-- common function for index modulation by xs-length
get_index_gt_zero :: [Char] -> Int -> Int
get_index_gt_zero xs y
    | y < 0 = get_index_gt_zero xs (y + length xs)
    | otherwise = y

