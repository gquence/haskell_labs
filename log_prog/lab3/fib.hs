fibN_naive :: Integer -> Integer
fibN_naive x
    | (x == 1) || (x == 2) = 1
    | x > 2 = fibN_naive (x - 1) + fibN_naive (x - 2)
    | otherwise = error "fibN_naive: Input number must be lower than 0"

fibN :: Integer -> Integer
fibN x | x < 0 = error "fibN: Input number must be lower than 0"
fibN x = tail_fib 1 1 x

tail_fib :: Integer ->  Integer ->  Integer ->  Integer
tail_fib first second iter
  | iter < 1 = first
  | iter == 2 = second
  | otherwise = tail_fib second (first + second) (iter - 1)

fib_map :: [Integer]
fib_map = map fibN [1, 2 ..]

fib_zip :: [Integer]
fib_zip = 0 : 1 : zipWith (+) fib_zip (tail fib_zip)