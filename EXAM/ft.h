ft_translate :: Int -> [Char]
ft_translate _ = "that's all, folks"
ft_translate n
    | n mod 10 == 0 = "zero " ft_translate (n div 10) 
    | n mod 10 == 1 = "one " ft_translate (n div 10)
    | otherwise = "other"
