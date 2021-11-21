import BigNumber

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n | n < 0 = error "Negative number not valid" 
         | otherwise = fibRec(n-1) + fibRec(n-2)

fibLista :: (Integral a) => a -> a
fibLista x | x < 0 = error "Negative number not valid" 
           | otherwise = last [calc n | n <- [0..x]]
             where calc 0 = 0
                   calc 1 = 1
                   calc n = calc (n-1) + calc (n-2)


fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n | n < 0 = error "Negative number not valid" 
                   | otherwise = fibs !! fromIntegral(n)
                                    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibRecBN :: BigNumber -> BigNumber
fibRecBN [] = []
fibRecBN [1] = [1]
fibRecBN b | head b == 0 = error "Negative number not valid" 
           | otherwise = somaBN (fibRecBN (subBN b [1])) (fibRecBN (subBN b [2]))

fibListaBN :: BigNumber -> BigNumber 
fibListaBN b | (not (null b)) && (head b == 0) = error "Negative number not valid" 
             | otherwise = last [calc n | n <- (listBN b)]
               where calc [] = []
                     calc [1] = [1]
                     calc n = somaBN (calc (subBN n [1])) (calc (subBN n [2]))


fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN n | (not (null n)) && (head n == 0) = error "Negative number not valid"
                     | otherwise = getValueBN fibs n
                                 where fibs = []:[1]:(zipWith (somaBN) fibs (tail fibs))
