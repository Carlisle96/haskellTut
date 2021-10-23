module Golf where

localMaxima list = map (\(x,y,z) -> x) $ 
    filter (\(x,y,z) -> x>y && x>z) $ 
    zip3 (drop 1 list) list (drop 2 list)
