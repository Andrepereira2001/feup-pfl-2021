fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec(n-1) + fibRec(n-2)

fibLista :: Int -> Int
fibLista n = fibs!!n
            where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)



fibListaInfinita :: Int -> Int
fibListaInfinita n = fibs!!n
                    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

