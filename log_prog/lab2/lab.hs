--task 2
-- 12 / fromIntegral(length [1,2,3]) :: Float
-- 12 / toRational(length [1,2,3])

--task 3
--2+3 == 5
--true

--task 4
--x = 3
--x + 3 == 7
--false

--task 5
--length [1,2,3] == 2
--работает. значения одного типа, включённого в класс Eq

--length [1, 'a', 2, 'b']
--не работает. в хаскелл char неявно не переводится в число

--[1, 2.0, 3.1]
--  работает, значения Int входят в класс Num, 
--  а Float/Double в Fractional, который является подклассом Num

--length [1,2] + length [2,3]
--работает

--(3==3) && ('b' < 'a')
-- работает. сравниваемые значения одних типов + булева операция

--(8 == 9) && 0
--ОН ВАМ НЕ СИ! 0 /= False

--task 6

isPalindrome :: String -> Bool
isPalindrome x
    | x == (reverse x) = True
    | otherwise = False

--task 7
f :: (a, b) -> (c, d) ->  ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a , fst b))