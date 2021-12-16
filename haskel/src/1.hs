soma :: [Int] -> [Int] -> [Int]
soma [] b = zipWith (+) (repeat 0) b 
soma a [] = zipWith (+) a (repeat 0) 
soma (a:as) (b:bs) | any (>9) l = soma (map (`mod` 10) l) (0 : map (`div` 10) l)
                    | otherwise = a + b : soma as bs
                    where l = a + b : soma as bs
