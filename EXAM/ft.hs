import Data.List

ft_sinInt :: Double -> Double -> Double
ft_sinInt strt end
    | strt < end = (+) ((*) ((/) ((+) (sin strt) ((+) (sin strt) 0.000001)) (2)) (0.000001)) (ft_sinInt ((+) strt 0.000001) end)
    | strt == end = 0
    | otherwise = 0

ft_deln :: Ord a => Int -> [a] -> [a]
ft_deln n [] = error "empty"
ft_deln n (x : xs)
    | n > 1 = [x] ++ ft_deln (n - 1) xs
    | n == 0 = error "Error!!! n = 0"
    | otherwise = xs

ft_rev_fl :: [a] -> [a]
ft_rev_fl [] = error "empty" --exception for one elem is in "last"func
ft_rev_fl (x : xs) = [last xs] ++ (init xs) ++ [x]

ft_part :: [a] -> Int -> Int -> ([a],[a])
ft_part [] _ _ = error "empty"
ft_part (xs) m n
    | ((+) m n) == (length xs) = (take m xs, drop ((-) (length xs)n) xs)
    | otherwise = error "Enter the valid values!!! (m + n = length)"

ft_translate :: Int -> [Char]
ft_translate n
    | n == 0 = ""
    | (mod n 10) == 0 = ft_translate (div n 10)++ "zero "
    | (mod n 10) == 1 = ft_translate (div n 10)++ "one "
    | (mod n 10) == 2 = ft_translate (div n 10)++ "two "
    | (mod n 10) == 3 = ft_translate (div n 10)++ "three "
    | (mod n 10) == 4 = ft_translate (div n 10)++ "four "
    | (mod n 10) == 5 = ft_translate (div n 10)++ "five "
    | (mod n 10) == 6 = ft_translate (div n 10)++ "six "
    | (mod n 10) == 7 = ft_translate (div n 10)++ "seven "
    | (mod n 10) == 8 = ft_translate (div n 10)++ "eight "
    | (mod n 10) == 9 = ft_translate (div n 10)++ "nine "

ft_double_repeat :: Ord a => [a] -> [a]
ft_double_repeat [] = []
ft_double_repeat [x] = [x]++[x]
ft_double_repeat (x : xs) =
    [x]++ [x]++ ft_double_repeat (xs)

ft_insert_elem :: Ord a => [a] -> a -> Int -> [a]
ft_insert_elem [] _ _ = error "empty"
ft_insert_elem [x] y pos
   | pos == 1 = [x]++ [y]
   | pos == 0 = [y]++ [x]
   | otherwise = error "Enter the valid values!!!"
ft_insert_elem (x : xs) y pos
   | ((+) (length xs) 1) < pos = error "Enter the valid values!!!"
   | pos == 0 = [y]++ [x]++ xs
   | otherwise = [x]++ ft_insert_elem xs y ((-) pos 1)
