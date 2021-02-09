approx :: Double -> Int -> [Int]
approx x n = [ceiling $ x * fromIntegral (denominator n), denominator n]
 where denominator n = fraction (x - toEnum(floor x)) n 0 1 1 1

-- returns denominator
fraction :: Double -> Int -> Int -> Int -> Int -> Int -> Int
fraction x n a b c d
 | sigFig x n == sigFig mediant n = b + d
 | x < mediant = fraction x n a b (a + c) (b + d)
 | otherwise = fraction x n (a + c) (b + d) c d
 where mediant = (fromIntegral (a + c))/(fromIntegral (b + d)) :: Double

sigFig :: Double -> Int -> Double
sigFig x n
 | length (show (x + 1)) >= n + 2 = read $ take (n + 2) (show (x + 1))
 | otherwise = x
