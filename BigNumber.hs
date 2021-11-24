module BigNumber
(BigNumber, somaBN, subBN, mulBN, divBN, getValueBN, equalBN) where

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

-- returns true if the first argument number is bigger than the second
-- as we only use this function to absolute BigNumbers the comparision with zero is always bigger
bigger :: BigNumber -> BigNumber -> Bool 
bigger [] [] = False  
bigger _ [] = True
bigger [] _ = False
bigger (a:as) (b:bs) | length as > length bs = True
                      | length as < length bs = False
                      | a > b = True
                      | b > a = False
                      | otherwise = bigger as bs

-- returns true if the first argument number is bigger or equal than the second
-- as we only use this function to absolute BigNumbers the comparision with zero is always bigger
biggerEqual :: BigNumber -> BigNumber -> Bool
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

-- search in a list of BigNumber the BigNumber in the position specified in the second argument
getValueBN :: [BigNumber] -> BigNumber -> BigNumber
getValueBN (l:ls) [] = l
getValueBN (l:ls) n = getValueBN ls (subBN n [1])  

-- no need to look it works!!!!!
soma :: BigNumber -> BigNumber -> BigNumber
soma [] [] = []

-- ensure that 2 digit numbers are removed([1,10,3])
-- append div to the end of the BigNumber
soma [] (b:[]) | div b 10 == 0 = [mod b 10]
               | otherwise = mod b 10: [div b 10] 
-- increment div to the next BigNumber element
soma [] (b:bs) = mod b 10: soma [] ((head bs) + (div b 10) : tail bs)

-- append div to the end of the BigNumber
soma (a:[]) [] | div a 10 == 0 = [mod a 10]
               | otherwise = mod a 10: [div a 10]
-- increment div to the next BigNumber element
soma (a:as) [] = mod a 10: soma [] ((head as) + (div a 10) : tail as)

-- both numbers have the same size append the div result in one of them 
soma (a:[]) (b:[]) | a+b >= 10 = mod (a+b) 10: soma [(div (a+b) 10)] []
                   | otherwise = a+b  : soma [] []

-- if no more numbers to increment in a, increment in b
soma (a:[]) (b:bs) | a+b >= 10 = mod (a+b) 10: soma [] ((head bs) + (div (a+b) 10) : tail bs)
                   | otherwise = a+b  : soma [] bs 

-- default increment div value to a                                 
soma (a:as) (b:bs) | a+b >= 10 = mod (a+b) 10: soma ((head as) + (div (a+b) 10) : tail as) bs
                   | otherwise = a+b  : soma as bs 



somaBN :: BigNumber -> BigNumber -> BigNumber
           -- positive added to positive
somaBN a b | (null a || head a /= 0) && (null b || head b /= 0) = reverse( soma (reverse a) (reverse b))

           -- negative added to positive
           | (not (null a) && head a == 0) && (null b || head b /= 0) = subBN b (tail a)
           
           -- positive added to negative
           | (null a || head a /= 0)  && (not (null b) && head b == 0) = subBN a (tail b)
           
           -- negative added to negative
           | head a == 0 && head b == 0 = (0:reverse( soma (reverse (tail a)) (reverse (tail b)) ))


sub :: BigNumber -> BigNumber -> BigNumber
sub [] [] = []
sub (a:[]) [] | div a 10 == 0 = [mod a 10]
               | otherwise = mod a 10: [div a 10]
                -- remove negative numbers in BigNumber ([4,-1,5])
sub (a:as) [] = mod a 10: sub ((head as) + (div a 10) : tail as) []
                -- if subtraction of numbers at the beginning of the BigNumbers is negative we subtract de excess from the next number
sub (a:as) (b:bs) | a-b < 0 = mod (a-b) 10 : sub ((head as) + (div (a-b) 10) : tail as) bs 
                  | otherwise = a-b: sub as bs                  


subBN :: BigNumber -> BigNumber -> BigNumber
          -- dropWhile used to remove leading zeros
          -- positive subtracted by positive
          -- number a bigger than b, a - b
subBN a b | (null a || head a /= 0) && (null b || head b /= 0) && biggerEqual a b = ( dropWhile (==0) (reverse (sub (reverse a) (reverse b))))
          -- number b bigger than a, b - a 
          | (null a || head a /= 0) && (null b || head b /= 0) && (not (biggerEqual a b)) = (0: dropWhile (==0) (reverse (sub (reverse b) (reverse a))))

          -- negative subtracted by positive
          | (not (null a) && head a == 0) && (null b || head b /= 0) = 0:(somaBN(tail a) b)

          -- positive subtracted by negative 
          | (null a || head a /= 0) && (not (null b) && head b == 0)  = somaBN a (tail b)

          -- negative subtracted by negative
          -- symmetric a bigger than symmetric b, symmetric a - symmetric b
          | head a == 0 && head b == 0 && bigger (tail a) (tail b) = (0:dropWhile (==0) (reverse (sub (reverse (tail a)) (reverse (tail b)))))
          -- symmetric b bigger than symmetric a, symmetric b - symmetric a
          | head a == 0 && head b == 0 && (not (bigger (tail a) (tail b))) = (dropWhile (==0) (reverse (sub (reverse (tail b)) (reverse (tail a)))))

mul :: BigNumber -> BigNumber -> BigNumber
mul [] [] = []
mul _ [] = []
mul [] _ = []
mul a (b:[]) = soma (map (b*) a) []
mul a (b:bs) = soma (map (b*) a) (0:mul a bs)

mulBN :: BigNumber -> BigNumber -> BigNumber
          -- positive multiplied by positive 
mulBN a b | (null a || head a /= 0) && (null b || head b /= 0) = (reverse (mul (reverse a) (reverse b)))
          -- positive multiplied by negative
          | (null a || head a /= 0) && (not (null b) && head b == 0) = (0:reverse (mul (reverse a) (reverse (tail b)))) 
          -- negative multiplied by positive
          | (not (null a) && head a == 0) && (null b || head b /= 0) = (0:reverse (mul (reverse (tail a)) (reverse b))) 
          -- negative multiplied by negative
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

-- iterates through the dividend to find a subnumber bigger then the divisor
-- find subnumber -> calculates division and append the result to the current result variabel the remainer stays in the counter
divi2 :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> (BigNumber, BigNumber)
divi2 [] d c r = (r ++ ((fst (divi [] c d []))) , snd (divi [] c d []))
divi2 (x:dd) d c r | biggerEqual (c ++ [x]) d = divi2 dd d (snd (divi [] (c ++ [x]) d [])) (r ++ (fst (divi [] (c ++ [x]) d [])))
                    -- ignore starting adding numbers
                   | null r = divi2 dd d (c ++ [x]) r 
                   -- if counter is [] <zero> mantain as [] <zero>
                   | x == 0 && null c = divi2 dd d [] (r ++ [0])
                   -- add 0 to result for each extra added number to counter
                   | otherwise = divi2 dd d (c ++ [x]) (r ++ [0])


-- handles with BigNumbers signals
divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
-- division by 0 is not allowed
divBN a b | null b = error "divide by zero" 
-- 0 divided by other number is 0
          | null a = ([], [])
-- both positive numbers 
          | head a /= 0 && head b /= 0 = divi2 a b [] []
-- positive divided by negative
          | head a /= 0 && head b == 0 && null (snd (divi2 a (tail b) [] [])) = (0:(fst (divi2 a (tail b) [] [])), [])
          | head a /= 0 && head b == 0 = (0:somaBN (fst (divi2 a (tail b) [] [])) [1], 0:subBN (tail b) (snd (divi2 a (tail b) [] [])))
-- negative divided by positive
          | head a == 0 && head b /= 0 && null (snd (divi2 (tail a) b [] [])) = (0:(fst (divi2 (tail a) b [] [])), []) 
          | head a == 0 && head b /= 0 = (0:somaBN (fst (divi2 (tail a) b [] [])) [1], subBN b (snd (divi2 (tail a) b [] []))) 
-- negative divided by negative
          | head a == 0 && head b == 0 && null (snd (divi2 (tail a) (tail b) [] [])) = ((fst (divi2 (tail a) (tail b) [] [])), [])
          | head a == 0 && head b == 0 = ((fst (divi2 (tail a) (tail b) [] [])), 0:(snd (divi2 (tail a) (tail b) [] [])))


safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN a b | null b = Nothing
              | otherwise = Just (divBN a b)
