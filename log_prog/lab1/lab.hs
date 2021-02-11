{-# LANGUAGE MultiWayIf #-}
--task 2
area x = 3.14 * (x * x)
double x = x * 2

-- for terminal input in ghci
--let x = 7; y = 10 in x + y

--task 4
sqFive = x * x
    where
        x = 5

x_and_y = x * y
    where
        x = 5
        y = 6

x_and_three = x + 1
    where
        x = 5
        y = 1000

weird_value = x * 5
    where
        y = 10
        x = 10 * 5 + y

another_weird_value = z / x + y
    where
        x = 7
        y = negate x
        z = y * 10

myAbs :: Integer -> Integer
myAbs x = 
    if x >= 0 then x
    else negate x