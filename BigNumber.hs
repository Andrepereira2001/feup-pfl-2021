module BigNumber
(BigNumber, somaBN, subBN, mulBN, divBN, getValueBN, listBN, equalBN) where

import Data.Char(digitToInt, intToDigit)

type BigNumber = [Int]

scanner :: String -> BigNumber
scanner "0" = []
scanner ('-':l) = 0:(scanner l)
scanner l = [digitToInt(x) | x <- l]

output :: BigNumber -> String
output [] = "0"
output (0:l) = '-':(output l)
output l = [intToDigit(x) | x <- l]

bigger :: [Int] -> [Int] -> Bool
bigger [] [] = False
bigger _ [] = True
bigger [] _ = False
bigger (a:as) (b:bs) | length as > length bs = True
                      | length as < length bs = False
                      | a > b = True
                      | b > a = False
                      | otherwise = bigger as bs

biggerEqual :: [Int] -> [Int] -> Bool
biggerEqual [] [] = True
biggerEqual _ [] = True
biggerEqual [] _ = False
biggerEqual (a:as) (b:bs) | length as > length bs = True
                      | length as < length bs = False
                      | a > b = True
                      | b > a = False
                      | otherwise = biggerEqual as bs 

equalBN :: BigNumber -> BigNumber -> Bool
equalBN [] [] = True
equalBN (a:as) (b:bs) | a == b && length as == length bs = equalBN as bs
                      | otherwise = False

getValueBN :: [BigNumber] -> BigNumber -> BigNumber
getValueBN (l:ls) [] = l
getValueBN (l:ls) n = getValueBN ls (subBN n [1])  

listBN :: BigNumber -> [BigNumber]
listBN [] = [[]]
listBN n = listBN (subBN n [1]) ++ [n]

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
somaBN a b | (null a || head a /= 0) && (null b || head b /= 0) = reverse( soma (reverse a) (reverse b))
           | (not (null a) && head a == 0) && (null b || head b /= 0) = subBN b (tail a)
           | (null a || head a /= 0)  && (not (null b) && head b == 0) = subBN a (tail b)
           | head a == 0 && head b == 0 = (0:reverse( soma (reverse (tail a)) (reverse (tail b)) ))

sub :: [Int] -> [Int] -> [Int]
sub [] [] = []
sub (a:[]) [] | div a 10 == 0 = [mod a 10]
               | otherwise = mod a 10: [div a 10]
sub (a:as) [] = mod a 10: sub ((head as) + (div a 10) : tail as) []
sub (a:as) (b:bs) | a-b<0 = mod (a-b) 10 : sub ((head as) + (div (a-b) 10) : tail as) bs
                  | otherwise = a-b: sub as bs                  


subBN :: BigNumber -> BigNumber -> BigNumber
subBN a b | (null a || head a /= 0) && (null b || head b /= 0) && biggerEqual a b = ( dropWhile (==0) (reverse (sub (reverse a) (reverse b))))
          | (null a || head a /= 0) && (null b || head b /= 0) && (not (biggerEqual a b)) = (0: dropWhile (==0) (reverse (sub (reverse b) (reverse a))))
          | (not (null a) && head a == 0) && (null b || head b /= 0) = 0:reverse (soma ((reverse (tail a))) (reverse (b)))
          | (null a || head a /= 0) && (not (null b) && head b == 0)  = somaBN a (tail b)
          | head a == 0 && head b == 0 && bigger (tail a) (tail b) = (0:dropWhile (==0) (reverse (sub (reverse (tail a)) (reverse (tail b)))))
          | head a == 0 && head b == 0 && (not (bigger (tail a) (tail b))) = (dropWhile (==0) (reverse (sub (reverse (tail b)) (reverse (tail a)))))

mul :: [Int] -> [Int] -> [Int]
mul [] [] = []
mul _ [] = []
mul [] _ = []
mul a (b:[]) = soma (map (b*) a) []
mul a (b:bs) = soma (map (b*) a) (0:mul a bs)

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN a b | (null a || head a /= 0) && (null b || head b /= 0) = (reverse (mul (reverse a) (reverse b))) 
          | (null a || head a /= 0) && (not (null b) && head b == 0) = (0:reverse (mul (reverse a) (reverse (tail b)))) 
          | (not (null a) && head a == 0) && (null b || head b /= 0) = (0:reverse (mul (reverse (tail a)) (reverse b))) 
          | head a == 0 && head b == 0 = (reverse (mul (reverse (tail a)) (reverse (tail b))))


-- c counter
-- dd dividend
-- d divisor
-- v value

-- v = sum d c times 
-- return how much divisors fit in the dididend and the remainder
divi :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber)
divi c dd d v | bigger v dd = (subBN c [1], subBN dd (subBN v d)) 
              | otherwise = divi (somaBN c [1]) dd d (somaBN v d)

-- dd dividend
-- d divisor
-- c counter
-- r result

-- iterates through the didivend and starts to in the result value
divi2 :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber)
divi2 [] d c r | not (null r) && null (fst (divi [] c d [])) = (r ++ [0], snd (divi [] c d []))
               | otherwise = (r ++ ((fst (divi [] c d []))) , snd (divi [] c d []))
divi2 (x:[]) d c r | x == 0 && null c = divi2 [] d [] r
divi2 (x:dd) d c r | biggerEqual c d = divi2 (x:dd) d (snd (divi [] c d [])) (r ++ (fst (divi [] c d [])))
                   | x == 0 && null c = divi2 dd d [] (r ++ [0])
                   | otherwise = divi2 dd d (c ++ [x]) r

              
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN a b | null b = error "divide by zero" 
          | null a = ([], [])
          | head a /= 0 && head b /= 0 = divi2 a b [] []

          | head a /= 0 && head b == 0 && null (snd (divi2 a (tail b) [] [])) = (0:(fst (divi2 a (tail b) [] [])), [])
          | head a /= 0 && head b == 0 = (0:somaBN (fst (divi2 a (tail b) [] [])) [1], 0:subBN (tail b) (snd (divi2 a (tail b) [] [])))

          | head a == 0 && head b /= 0 && null (snd (divi2 (tail a) b [] [])) = (0:(fst (divi2 (tail a) b [] [])), []) 
          | head a == 0 && head b /= 0 = (0:somaBN (fst (divi2 (tail a) b [] [])) [1], subBN b (snd (divi2 (tail a) b [] []))) 

          | head a == 0 && head b == 0 && null (snd (divi2 (tail a) (tail b) [] [])) = ((fst (divi2 (tail a) (tail b) [] [])), [])
          | head a == 0 && head b == 0 = ((fst (divi2 (tail a) (tail b) [] [])), 0:(snd (divi2 (tail a) (tail b) [] [])))

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN a b | null b = Nothing
              | otherwise = Just (divBN a b)
