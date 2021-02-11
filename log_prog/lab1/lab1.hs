myDiv :: Int -> Int -> (Int, Int)
myDiv x y
    | x >= y = (fst_res, sec_res)
    | otherwise = (0, x)
    where
        res_tuple = myDiv (x - y) y
        fst_res = fst(res_tuple) + 1
        sec_res = snd(res_tuple)

        
