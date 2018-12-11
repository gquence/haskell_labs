{-# LANGUAGE MultiWayIf #-}

ft_choise_let :: Int -> Int
ft_choise_let time =
    let
       delta = 33
       expr  = 40
    in
    if |time >= expr -> time + delta
       |otherwise -> expr - time

ft_choise_where :: Int -> Bool
ft_choise_where time = time >= expr
    where
      expr = 40
