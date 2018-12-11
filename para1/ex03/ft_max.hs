ft_max ::  Ord a => a -> a -> a
ft_max x y
          | x <= y = y
          | otherwise = x
