module Main where
import Data.List

addElementTo a b = a : b

addListTo a b | a == [] = b
              | otherwise = (tail a) `addListTo` ((head a) `addElementTo` b)

getDirectChilds parent list | list == [] = []
                            | currentParent == parent = currentChild `addElementTo` childs
                            | otherwise = childs
  where currentParent = head (head list)
        currentChild = head (tail (head list))
        childs = parent `getDirectChilds` (tail list)

getDirectChildsByParents parents list | parents == [] = []
                                            | list == [] = []
                                            | otherwise = (getDirectChilds (head parents) list) `addListTo` (getDirectChildsByParents (tail parents) list)

getAllChildsByParents parents list | list == [] = []
                       | parents == [] = []
                       | otherwise = currentDirectChilds `addListTo` (currentDirectChilds `getAllChildsByParents` list)
  where currentDirectChilds = getDirectChildsByParents parents list

getAllChilds parent list = getAllChildsByParents [parent] list

main = print (getAllChilds "Petro" list)
  where list = [["Kolya", "Petya"], ["Kolya", "Vasya"], ["Gosha", "Anton"], ["Petro", "Kolya"], ["Petro", "Gosha"], ["Petya", "Masha"], ["Petya", "Dasha"], ["Petya", "Spaik"]]
        parent = ["Kolya"]