import Data.Char

squareDigit :: Int -> Int
squareDigit x
    | x == 0 = x
    | x < 0 = (-1) * (squareDigit $ (-1) * x)
    | otherwise = read $ (show (squareDigit xd)) ++ (show $ (^) xm 2) :: Int
    where
      xm = mod x 10
      xd = div x 10
