
myDiv :: Integer -> Integer -> (Integer, Integer)
myDiv x y
    | (x < 0) && (y < 0) = myDiv (-x) (-y)
    | (x < 0) || (y < 0) = ((-fst_res), sec_res)
    | x >= y = (fst_res, sec_res)
    | otherwise = (0, x)
    where
        res_tuple = myDiv ((abs x) - y) y
        fst_res = fst(res_tuple) + 1
        sec_res = snd(res_tuple)
    
        
