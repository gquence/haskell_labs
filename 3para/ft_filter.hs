import Data.Maybe
import Data.List

ft_filter ::  (a -> Bool) -> [a] -> Maybe a
ft_filter _ [] = error"noway"
ft_filter f [x]
    | f x == True = Just x
    | otherwise = Nothing
ft_filter f (x : xs)
    | f x == True = Just x
    | f x == False = ft_filter f xs

ft_list_filter1 :: (a -> Bool) -> [a] -> ([a],[a])
ft_list_filter1 _ [] = ([],[])
ft_list_filter1 f xs = partition f xs

ft_list_filter2 :: (a -> Bool) -> [a] -> ([a],[a])
ft_list_filter2 _ [] = ([],[])
ft_list_filter2 f xs = (ft_valid f xs, ft_notvalid f xs)

ft_notvalid :: (a -> Bool) -> [a] -> [a]
ft_notvalid _ [] = []
ft_notvalid f (x : xs)
    | f x == False = [x]++ ft_notvalid f xs
    | otherwise = ft_notvalid f xs

ft_valid :: (a -> Bool) -> [a] -> [a]
ft_valid _ [] = []
ft_valid f (x : xs)
    | f x == True = [x]++ ft_valid f xs
    | otherwise = ft_valid f xs

ft_part :: [a] -> ([a],[a])
ft_part [] = ([],[])
ft_part xs
    | (mod  (length xs)) 2 == 0  = splitAt (div (length xs) 2) xs
    | otherwise = (take (div (length xs) 2) xs, drop ((+) (div (length xs) 2) 1) xs)
