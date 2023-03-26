gcd' :: Int -> Int -> Int
gcd' x y
    | y == 0    = x
    | otherwise = gcd' y (x `mod` y)

lcm' :: Int -> Int -> Int
lcm' x y = x * y `div` (gcd' x y)

main = do
  print (lcm 6 9)