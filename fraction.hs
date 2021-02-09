approx :: Float -> [Integer]
approx x = [floor (x * fromIntegral (fraction (x - toEnum(floor x)) 0 1 1 1))
           ,fraction (x - toEnum(floor x)) 0 1 1 1]

-- returns denominator to use
fraction :: Float -> Integer -> Integer -> Integer -> Integer -> Integer
fraction x a b c d
 | x == mediant = b + d
 | x < mediant = fraction x a b (a + c) (b + d)
 | otherwise = fraction x (a + c) (b + d) c d
 where mediant = (fromIntegral (a + c))/(fromIntegral (b + d)) :: Float
