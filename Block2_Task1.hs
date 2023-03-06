module Main where
import Data.List

addElementTo a b = a : b

addListTo a b | a == [] = b
              | otherwise = (tail a) `addListTo` ((head a) `addElementTo` b)

contains list element | list == [] = False
                      | head list == element = True
                      | otherwise = (tail list) `contains` element

getDirectChilds parent list | list == [] = []
                            | currentParent == parent = currentChild `addElementTo` childs
                            | otherwise = childs
  where currentParent = head (head list)
        currentChild = head (tail (head list))
        childs = parent `getDirectChilds` (tail list)

getDirectChildsFromManyParents parents list | parents == [] = []
                                            | list == [] = []
                                            | otherwise = (getDirectChilds (head parents) list) `addListTo` (getDirectChildsFromManyParents (tail parents) list)

getChilds parents list | list == [] = []
                       | parents == [] = []
                       | otherwise = currentDirectChilds `addListTo` (currentDirectChilds `getChilds` list)
  where currentDirectChilds = getDirectChildsFromManyParents parents list

main = print (getChilds parent list)
  where list = [["Kolya", "Petya"], ["Kolya", "Vasya"], ["Gosha", "Anton"], ["Petro", "Kolya"], ["Petro", "Gosha"], ["Petya", "Masha"], ["Petya", "Dasha"], ["Petya", "Spaik"]]
        a1 = [1, 2, 3]
        a2 = [4, 5, 6]
        parent = ["Anton"]
        parents = ["Petya", "Petro"]