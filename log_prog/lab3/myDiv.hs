
myDiv :: Integer -> Integer -> (Integer, Integer)
myDiv x y
    | y == 0 = error "divide by zero"
    | (x < 0) && (y > 0) && ((abs x) >= (abs y)) = ((-fst_res), (-sec_res))
    | (x < 0) && (y < 0) && ((abs x) >= (abs y)) = (fst_res, (-sec_res))
    | (x > 0) && (y < 0) && ((abs x) >= (abs y)) = ((-fst_res), sec_res) 
    | (x >= y) && ((abs x) >= (abs y)) = (fst_res, sec_res)
    | otherwise = (0, x)
    where
        res_tuple = myDiv ((abs x) - (abs y)) (abs y)
        fst_res = fst(res_tuple) + 1
        sec_res = snd(res_tuple)
    
        
