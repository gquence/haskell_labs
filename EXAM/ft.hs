import Data.List



-- функции для использования: ft_Int, ft_rev_fl, ft_allex, ft_translate, ft_double_repeat, ft_delf, ft_part , ft_insert_elem



ft_Int :: (Double -> Double) -> Double -> Double -> Double -> Double
ft_Int func strt end step
    | strt < end  = ((/) (end - strt) step) * ((((func strt) / 2) + ((func end) / 2)) + (ft_sum func strt end ((end - strt) /step)))
    | strt == end = 0
    | strt > end = 0

ft_sum :: (Double -> Double) -> Double -> Double -> Double -> Double
ft_sum func start end step
    | (start) < end = (func  start) + (ft_sum func (start + step) end step)
    | otherwise = 0

ft_cube :: Double -> Double
ft_cube x = x * x * x

ft_line :: Double -> Double
ft_line x = x

ft_delf :: [a] -> Int -> [a]
ft_delf [] _ = []
ft_delf xs n
    | n > 0 = (ft_first_part xs ((-) n 1)) ++ (ft_delf(ft_start_from xs n) n)
    | otherwise = []


ft_rev_fl :: [a] -> [a]
ft_rev_fl (x : xs) = [ft_end (xs)] ++ ft_allex (xs) ++ [x]

ft_allex :: [a] -> [a]
ft_allex [x] = []
ft_allex [x, y] = [x]
ft_allex (x : xs) = [x] ++ (ft_allex xs)

ft_end :: [a] -> a
ft_end [x] = x
ft_end (x : xs) = ft_end xs

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

ft_part :: [a] -> Int -> Int -> ([a],[a])
ft_part [] _ _ = ([],[])
ft_part xs a b
   | ((+) a b) == (length (xs)) = ((ft_first_part xs a), (ft_start_from xs ((-) (length xs) b)))
   | otherwise = error "error!!! Wrong values of lengths"

ft_start_from :: [a] -> Int -> [a]
ft_start_from [] _ = []
ft_start_from (x : xs) len
   | len > 0 = ft_start_from xs ((-) len 1)
   | len == 0 = (x : xs)
   | otherwise = error "error!!! Wrong value of length"

ft_first_part :: [a] -> Int -> [a]
ft_first_part [] _ = []
ft_first_part (x : xs) len
   | len > 0 = [x] ++ (ft_first_part xs ((-) len 1))
   | len == 0 = []
   | otherwise = error "error!!! Wrong value of length"
