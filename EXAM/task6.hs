import Data.List

ft_eq :: [Int] -> [Char]
ft_eq [x,y]
    | x == y    = (show x) ++ (" = ") ++ (show y)
    | otherwise = "!"
ft_eq (x : xs)
    | last (ft_equation (x : xs)) == '!' = "wrong values"
    | otherwise = (show x) ++ (ft_equation (x : xs))

ft_equation :: [Int] -> [Char]
ft_equation [x,y]
    | x == y    = (" = ") ++ (show y)
    | otherwise = "!"
ft_equation (x : y : xs)
    | last (ft_equation ((+) x y : xs)) /= '!' = {-(show x) ++-} (" + ") ++ (show y) ++ (ft_equation (((+) x y ): xs))
    | last (ft_equation ((-) x y : xs)) /= '!' = {-(show x) ++-} (" - ") ++ (show y) ++ (ft_equation (((-) x y) : xs))
    | last (ft_equation ((*) x y : xs)) /= '!' = {-(show x) ++-} (" * ") ++ (show y) ++ (ft_equation (((*) x y) : xs))
    | last (ft_equation (y : xs)) /= '!'       ={- (show x) ++ -}(" = ") ++ (show y) ++ (ft_equation (y : xs))
--    | last (ft_equation ((/) x y : xs)) /= '!' = (show x) ++ (" / ") ++ (show y) ++ (ft_equation ((/) x y : xs))
    | otherwise = "!"
