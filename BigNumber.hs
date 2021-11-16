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
somaBN (BN a) (BN b) | head a /= 0 && head b /= 0 = ( BN (reverse( soma (reverse a) (reverse b) )))
                     | head a == 0 && head b /= 0 = subBN (BN b) (BN (tail a))
                     | head a /= 0 && head b == 0 = subBN (BN a) (BN (tail b))
                     | head a == 0 && head b == 0 = ( BN (0:reverse( soma (reverse (tail a)) (reverse (tail b)) )))


biggest :: [Int] -> [Int] -> Bool
biggest [] [] = False
biggest _ [] = True
biggest [] _ = False
biggest (a:as) (b:bs) | length as > length bs = True
                      | length as < length bs = False
                      | a > b = True
                      | b > a = False
                      | otherwise = biggest as bs 

sub :: [Int] -> [Int] -> [Int]
sub [] [] = []
sub (a:[]) [] | div a 10 == 0 = [mod a 10]
               | otherwise = mod a 10: [div a 10]
sub (a:as) [] = mod a 10: sub ((head as) + (div a 10) : tail as) []
sub (a:as) (b:bs) | a-b<0 = mod (a-b) 10 : sub ((head as) + (div (a-b) 10) : tail as) bs
                  | otherwise = a-b: sub as bs                  


subBN :: BigNumber -> BigNumber -> BigNumber
subBN (BN a) (BN b) | head a /= 0 && head b /= 0 && biggest a b = ( BN ( dropWhile (==0) (reverse (sub (reverse a) (reverse b)))))
                    | head a /= 0 && head b /= 0 && (not (biggest a b)) = ( BN (0: dropWhile (==0) (reverse (sub (reverse b) (reverse a)))))
                    | head a == 0 && head b /= 0 = BN ( 0:reverse (soma ((reverse (tail a))) (reverse (b))))
                    | head a /= 0 && head b == 0 = somaBN (BN a) (BN (tail b))
                    | head a == 0 && head b == 0 && biggest (tail a) (tail b) = ( BN ( 0:dropWhile (==0) (reverse (sub (reverse (tail a)) (reverse (tail b))))))
                    | head a == 0 && head b == 0 && (not (biggest (tail a) (tail b))) = ( BN ( dropWhile (==0) (reverse (sub (reverse (tail b)) (reverse (tail a))))))

mul :: [Int] -> [Int] -> [Int]
mul [] [] = []
mul _ [0] = [0]
mul [0] _ = [0]
mul a (b:[]) = soma (map (b*) a) []
mul a (b:bs) = soma (map (b*) a) (0:mul a bs)

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN (BN a) (BN b) | head a == 0 && head b == 0 = BN (reverse (mul (reverse (tail a)) (reverse (tail b)))) 
                    | head a == 0 && head b /= 0 = BN (0:reverse (mul (reverse (tail a)) (reverse b))) 
                    | head a /= 0 && head b == 0 = BN (0:reverse (mul (reverse a) (reverse (tail b)))) 
                    | head a /= 0 && head b /= 0 = BN (reverse (mul (reverse a) (reverse b))) 
