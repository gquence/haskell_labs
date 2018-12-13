import Data.List
--  только при условии, что "=" в конце
ft_eq :: [Double] -> [Char]
ft_eq [] = "wrong values"
ft_eq [x,y]
    | x == y    = (show x) ++ (" = ") ++ (show y)
    | otherwise = "!"
ft_eq (x : xs)
    | last (ft_equation (x : xs)) == '!' = "wrong values"
    | otherwise = (show x) ++ (ft_equation (x : xs))

ft_equation :: [Double] -> [Char]
ft_equation [x, y, z]
    | x == y && y == z = (" = ") ++ (show y) ++ (" = ") ++ (show z)
    | ((+) x y) == z =  (" + ") ++ (show y) ++ (" = ") ++ (show z)
    | ((-) x y) == z =  (" - ") ++ (show y) ++ (" = ") ++ (show z)
    | ((*) x y) == z =  (" * ") ++ (show y) ++ (" = ") ++ (show z)
    | ((/) x y) == z =  (" / ") ++ (show y) ++ (" = ") ++ (show z)
    | otherwise = "!"
ft_equation (x : y : xs)
    | last (ft_equation ((+) x y : xs)) /= '!' =  (" + ") ++ (show y) ++ (ft_equation (((+) x y) : xs))
    | last (ft_equation ((-) x y : xs)) /= '!' = (" - ") ++ (show y) ++ (ft_equation (((-) x y) : xs))
    | last (ft_equation ((*) x y : xs)) /= '!' = (" * ") ++ (show y) ++ (ft_equation (((*) x y) : xs))
    | last (ft_equation ((/) x y : xs)) /= '!' = (" / ") ++ (show y) ++ (ft_equation (((/) x y) : xs))
    | (x == y) && last (ft_equation (y : xs)) /= '!' = (" = ") ++ (show y) ++ (ft_equation (y : xs))
    | otherwise = "!"
