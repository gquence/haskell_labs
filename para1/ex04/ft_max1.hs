ft_max1 :: Ord a => a -> a -> a -> a
ft_max1 x y z
             |(x >= y) && (x >= z) = x
             |(y >= x) && (y >= z) = y
             |(z >= x) && (z >= y) = z
