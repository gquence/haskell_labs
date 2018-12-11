ft_rec_max ::  Ord a => [a] -> a
ft_rec_max [] = error"NULL values"
ft_rec_max [x] = x
ft_rec_max (x : xs) = max x (ft_rec_max xs)
