import BigNumber

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n | n < 0 = error "Negative number not valid" 
         | otherwise = fibRec(n-1) + fibRec(n-2)

fibListaHelp :: (Integral a) => a -> a -> a -> a -> a
fibListaHelp 0 _ _ _ = 0
fibListaHelp 1 _ _ _ = 1
fibListaHelp n i a b | n == i = b
                     | otherwise = fibListaHelp n (i+1) b (a+b)

fibLista :: (Integral a) => a -> a
fibLista x | x < 0 = error "Negative number not valid" 
           | otherwise = fibListaHelp x 1 0 1


fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n | n < 0 = error "Negative number not valid" 
                   | otherwise = fibs !! fromIntegral(n)
                                    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibRecBN :: BigNumber -> BigNumber
fibRecBN [] = []
fibRecBN [1] = [1]
fibRecBN b | head b == 0 = error "Negative number not valid" 
           | otherwise = somaBN (fibRecBN (subBN b [1])) (fibRecBN (subBN b [2]))

fibListaHelpBN :: BigNumber -> BigNumber -> BigNumber -> BigNumber -> BigNumber
fibListaHelpBN [] _ _ _ = []
fibListaHelpBN [1] _ _ _ = [1]
fibListaHelpBN n i a b | equalBN n i = b
                     | otherwise = fibListaHelpBN n (somaBN i [1]) b (somaBN a b)

fibListaBN :: BigNumber -> BigNumber
fibListaBN b | (not (null b)) && (head b == 0) = error "Negative number not valid" 
             | otherwise = fibListaHelpBN b [1] [] [1]


fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN n | (not (null n)) && (head n == 0) = error "Negative number not valid"
                     | otherwise = getValueBN fibs n
                                 where fibs = []:[1]:(zipWith (somaBN) fibs (tail fibs))
