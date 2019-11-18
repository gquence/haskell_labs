dict_v1 :: String -> Int -> Either Int String
dict_v1 y x
  | x == 1 && y == "" = Right "One"
  | x == 2 && y == "" = Right "Two"
  | y == "One" = Left 1
  | y == "Two" = Left 2
  | otherwise = Right "No One No Two"
