ft_alph :: [Char] -> [Char]
ft_alph a = [b | b <- a,  elem b ['A' .. 'Z']]
