--Gabe Adams Practice Problems for Quiz 1 CIS 352

import Data.Char       
import Data.List       (nub)

--question 1 
insert :: Int -> a -> [a] -> [a]
insert n c ws = xs ++ c:ys
   where (xs,ys) = splitAt n ws 


--question 2 
shortest :: [[a]] ->[a]
shortest (xs:xss) = foldr (\xs acc -> if length xs < length acc then xs else acc) xs xss

--question 3 
data BinTree = Emp | Branch Int BinTree BinTree
				deriving (Eq, Show)

sqBinTree :: BinTree -> BinTree

sqBinTree Emp = Emp 
sqBinTree (Branch x ltree rtree) = Branch (x*x) (sqBinTree ltree) (sqBinTree rtree)

--question 4 
data Multi = Fork Int [Multi] deriving (Eq, Show)

sqMulti :: Multi -> Multi 

sqMulti (Fork x mtrees) = Fork (x*x) (map sqMulti mtrees)