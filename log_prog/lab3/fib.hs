fibN_naive :: Integer -> Integer
fibN_naive x
    | (x == 1) || (x == 2) = 1
    | x > 2 = fibN_naive (x - 1) + fibN_naive (x - 2)
    | otherwise = error "fibN_naive: Input number must be lower than 0"

fibN :: Integer -> Integer
fibN x | x < 0 = error "fibN: Input number must be lower than 0"
fibN x = fibn x
  where
    fibn 1 = 1
    fibn 2 = 1
    fibn n  = (fibn (n-1)) + (fibn (n-2))

fib_map :: [Integer]
fib_map = map fibN [1, 2 ..]

fib_zip :: [Integer]
fib_zip = 0 : 1 : zipWith (+) fib_zip (tail fib_zip)