nSum n | n == 0 = 0
       | prevNum < 5 = lastNum + prevNum * 2 + (nSum (n `div` 100))
       | otherwise = lastNum + prevNum * 2 - 9 + (nSum (n `div` 100))
  where lastNum = n `mod` 10
        prevNum = n `div` 10 `mod` 10

check n = (nSum n) `mod` 10 == 0

strToInt s = read s :: Integer

task s = check (strToInt s)

main = do
    print (task "5569490013253989")
