ft_nbr :: Int -> String
ft_nbr x
        | 100 > x = "less 100"
        | 100 < x = "over 100"
        | 100 == x = "equal 100"
        | otherwise = "out of range"
