fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-1) + fibRec(n-2)

fibLista :: (Integral a) => a -> a
fibLista x = last [calc n | n <- [0..x]]
             where calc 0 = 0
                   calc 1 = 1
                   calc n = calc (n-1) + calc (n-2)


fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = fibs !! fromIntegral(n)
                    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

