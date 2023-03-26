survivor n = if n == 1
                then 1
                else (survivor (n - 1) + 5 - 1) `mod` n + 1


getElementByIndex xs i = last (take (i + 1) xs)


main = do
  print (getElementByIndex elements ((survivor (length elements)) - 1))
  where
    elements = ["zero", "one", "two", "three", "four", "five", "six"]