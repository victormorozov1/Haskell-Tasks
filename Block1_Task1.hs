c n k | n < k = 0
      | n == k = 1
      | otherwise = c (n - 1) k * n `div` (n - k)  

main = do
    print (c 20 9)