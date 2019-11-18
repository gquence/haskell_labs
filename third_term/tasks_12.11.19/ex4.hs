trip_max :: Int -> Int -> Int -> Int
trip_max x y z
  | (x >= y) && (x >= z) = x
  | (y >= z) = y
  | otherwise = z
