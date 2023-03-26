cross x1 y1 x2 y2 | x1 == x2 = True
                  | y1 == y2 = True
                  | abs(x1 - x2) == abs(y1 - y2) = True
                  | otherwise = False

crossMany x y list | (length list) == 0 = False
                   | (cross x y headX headY) = True
                   | otherwise = (crossMany x y (tail list))
  where headX = head (head list)
        headY = last (head list)

crossAllToAll list | length list <= 1 = False 
                   | (crossMany headX headY nextList) = True
                   | otherwise = crossAllToAll nextList
  where nextList = tail list
        headX = head (head list)
        headY = last (head list)

main = do
  print (crossAllToAll list)
  where 
    list = [[1, 2], [7, 7], [5, 8]]