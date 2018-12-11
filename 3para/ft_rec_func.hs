ft_rec :: (a -> b) -> (b -> a) -> [a] -> [a]
ft_rec _ _ [] = []
ft_rec f g (x : xs) = g (f x) : ft_rec f g xs
