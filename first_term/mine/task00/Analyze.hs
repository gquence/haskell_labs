{-# LANGUAGE MultiWayIf #-}

ft_analyze :: Int -> String
ft_analyze x
       | x == 999 = "999 standart!"
       | x == 750 = "750 standart!"
       | x == 585 = "585 standart!"
       | otherwise = "Not enough info about this standart!"

ft_case_analyze :: Int -> String
ft_case_analyze x = 
    case x of
        999 -> "999 standart!"
        750 -> "750 standart!"
        585 -> "585 standart!"
        _   -> "not enough info"

ft_calc_time :: Int -> Int
ft_calc_time timeIns
    | timeIns >= threshold = timeIns + correction
    | otherwise = timeIns + correction + delta
    where
      threshold  = 40
      correction = 120
      delta      = 8
