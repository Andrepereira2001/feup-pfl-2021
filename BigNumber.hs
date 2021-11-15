module BigNumber
(BigNumber) where

data BigNumber = BN [Int] deriving (Show)

reverseBN :: BigNumber -> BigNumber
reverseBN (BN a) = BN (reverse a)

soma :: [Int] -> [Int] -> [Int]
soma [] [] = []
soma [] (b:[]) | div b 10 == 0 = [mod b 10]
               | otherwise = mod b 10: [div b 10]
soma [] (b:bs) = mod b 10: soma [] ((head bs) + (div b 10) : tail bs)
soma (a:[]) [] | div a 10 == 0 = [mod a 10]
               | otherwise = mod a 10: [div a 10]
soma (a:as) [] = mod a 10: soma [] ((head as) + (div a 10) : tail as)
soma (a:[]) (b:[]) | a+b >= 10 = mod (a+b) 10: soma [(div (a+b) 10)] []
                   | otherwise = a+b  : soma [] []   
soma (a:[]) (b:bs) | a+b >= 10 = mod (a+b) 10: soma [] ((head bs) + (div (a+b) 10) : tail bs)
                   | otherwise = a+b  : soma [] bs                                  
soma (a:as) (b:bs) | a+b >= 10 = mod (a+b) 10: soma ((head as) + (div (a+b) 10) : tail as) bs
                   | otherwise = a+b  : soma as bs 

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN (BN a) (BN b) = reverseBN ( BN ( soma (reverse a) (reverse b) ))



--mulBN :: BigNumber -> BigNumber -> BigNumber
--mulBN (BN a) (BN b) = BN ( [x*y | x<-a, y<-b]) 
