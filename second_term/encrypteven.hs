import Data.List

encrypt :: String -> Int -> String
encrypt [] _ = []
encrypt [x] i = [x]
encrypt xs i
    | i > 0 = encrypt ([(!!) xs i | i <- [0, 1 .. ((length xs) - 1)], even i == False] ++ [(!!) xs i | i <- [0, 1 .. ((length xs) - 1)], even i]) (i - 1)
    | otherwise = xs


decrypt_word :: String -> String -> String
decrypt_word [] [] = []
decrypt_word [] [y] = [y]
decrypt_word [x] [y] = [y] ++ [x]
decrypt_word (x : xs) (y : ys) = [y] ++ [x] ++ decrypt_word xs ys

decrypt :: String -> Int -> String
decrypt [] _ = []
decrypt [x] i = [x]
decrypt xs i
    | i <= 0 = xs
    | otherwise = decrypt (decrypt_word xs1 xs2) (i - 1)
    where 
      half_len = div (length xs) 2
      xs1 = fst (splitAt half_len xs)
      xs2 = snd (splitAt half_len xs)
{--
decrypt1 [] _ = []
decrypt1 [x] i = [x]
decrypt1 xs i
    | i == 0 = xs
    | half_len == length xs2 = [++[((!!) (xs2) i)] ++[((!!) (xs1) i)] | i <- [0.. (half_len - 1)]]
    | otherwise = []
    where 
      half_len = div (length xs) 2
      xs1 = fst (splitAt half_len xs)
      xs2 = snd (splitAt half_len xs)--}
