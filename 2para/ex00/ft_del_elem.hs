{-# LANGUAGE MultiWayIf #-}

deln ::Ord a =>  Int -> [a] -> [a]
deln n [] = error "empty arr"
deln n (x : xs)
    |n > 1  = [x]++ deln (n - 1) xs
    |n == 0 = error "err"
    |otherwise = xs


delf :: Ord a => a -> [a]->[a]
delf n [] = error "empty arr"
delf n (x : xs)
    | n /= x = [x]++ delf n xs
    | otherwise = xs

ft_find_max_min :: Ord a => [a] -> [a]
ft_find_max_min [] = error "empty arr"
ft_find_max_min (x : y : z : xs)
    | x > y = ft_find_max_min (y : x : z : xs)
    | x > z = ft_find_max_min (z : y : xs)
    | z > y = ft_find_max_min (x : z : xs)
    | otherwise = ft_find_max_min (x : y : xs)
ft_find_max_min [x, y]
    | x > y = [y, x]
    | otherwise = [x, y]
ft_find_max_min [x] = [x]


ft_quick_sort :: Ord a => Int -> [a] -> [a]
ft_quick_sort n [] = error "empty arr"
ft_quick_sort n (x : y : xs) =
    if n > 1 then
        if  x > y then [x]++ ft_quick_sort (n - 1)(y : x : xs)
        else [x]++ ft_quick_sort (n - 1)(x : y : xs)
    else xs
