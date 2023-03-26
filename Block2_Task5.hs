check list | length list == 0 = True
           | head list == last list = False
           | otherwise = check (tail (init list))

main = do
  print (check l)
  where l = [0, 0, 0, 0, 1, 1]