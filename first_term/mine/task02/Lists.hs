{-# LANGUAGE MultiWayIf #-}

import Data.List

ft_list_length :: String -> String
ft_list_length row
    | size == 2 = composeTwoOptionsFrom row
    | size == 3 = composeThreeOptionsFrom row
    | otherwise = invalidRow row
    where
      size = length row
