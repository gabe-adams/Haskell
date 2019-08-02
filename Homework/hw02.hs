-- Written by: (Whoops, forgot my name. Mr. Grader take off 5 points.)
-- Version 3: 28 Jan 2016

import Test.QuickCheck
import System.Process(system)
import System.IO( hPutStrLn, withFile, IOMode(WriteMode) )
import Control.Monad( liftM, liftM2)
import Data.List
import Data.Char



------------------------------------------------------------------------
-- Binary trees
data BTree = Empty | Branch Char BTree BTree
            deriving (Show,Eq)

-- Multiway trees
data MTree = Node Char [MTree]
        deriving (Eq, Show)

------------------------------------------------------------------------
-- Examples 

t1 = Branch 'x' 
       (Branch 't' 
         (Branch 'a' Empty Empty) 
         Empty) 
       (Branch 'w' 
         (Branch 'm' Empty Empty) 
         (Branch 'q' Empty Empty))

t2 = Node 'u' 
       [Node 'c' [],
        Node 'q' [],
        Node 'n' 
          [Node 'm' [],
           Node 'g' [],
           Node 'j' []],
        Node 'y' 
          [Node 'z' []]]


-- Counting BTree Branch nodes
bcount :: BTree -> Int
bcount Empty = 0
bcount (Branch _ tl tr) = 1 + bcount tl + bcount tr

-- Counting MTree Nodes
mcount :: MTree -> Int
mcount (Node _ ts) = 1 + sum (map mcount ts)

--  preorder traversal for BTrees
preorder Empty = ""
preorder (Branch c tl tr) = c:(preorder tl ++ preorder tr)

-- inorder traversal for BTrees
inorder Empty = ""
inorder (Branch c tl tr) = inorder tl ++ [c] ++ inorder tr
------------------------------------------------------------------------
------------------------------------------------------------------------
fix = error "Please fix me!"
------------------------------------------------------------------------
-- Problem 1: BTree depth 
bmaxDepth :: BTree -> Int
bmaxDepth Empty = -1
bmaxDepth (Branch c tl tr) = 1 + max (bmaxDepth tl) (bmaxDepth tr)
------------------------------------------------------------------------
-- Problem 2: 
mmaxDepth :: MTree -> Int 
mmaxDepth (Node _ []) = 0
mmaxDepth (Node _ (ts)) = 1 + (maximum [mmaxDepth c | c<-ts])

------------------------------------------------------------------------
-- Problem 3: Collecting BTree leaves
bleaves :: BTree -> String
bleaves Empty =  []
bleaves (Branch x Empty Empty) = [x]
bleaves (Branch x tl tr) = bleaves tl ++ bleaves tr  
------------------------------------------------------------------------
-- Problem 4: Collecting MTree leaves
mleaves :: MTree -> String
mleaves (Node c []) = [c]
mleaves (Node c (ts)) = concat [mleaves p | p<-ts]

------------------------------------------------------------------------
-- Problem 5: BTree levels 
blevel :: Int -> BTree -> String
blevel x (Branch c tl tr)
      |x<1    = ""
      |x==1   = [c]
      |(tl==Empty && tr==Empty) = []
      |tl==Empty   = blevel (x-1) tr
      |tr==Empty   = blevel (x-1) tl 
      |otherwise   = blevel (x-1) tl ++ blevel (x-1) tr


------------------------------------------------------------------------
-- Problem 6: MTree levels 
mlevel :: Int -> MTree -> String
mlevel x (Node c ts)
      |x<1    = ""
      |x==1   = [c]
      |ts==[] = []
      |otherwise = concat [mlevel (x-1) g | g<-ts]

------------------------------------------------------------------------
-- Problem 7: Postorder
postorder Empty = ""
postorder (Branch c tl tr) = (postorder tl ++ postorder tr) ++ [c]
------------------------------------------------------------------------
-- Problem 8: reconstructing a BTree with the bin. search-tree prop.
--   from its postorder traversal

--reconstruct "" = =Empty
--reconstruct (c:cs) = 

-- Some testing tools for Problem 8

-- For c :: Char and t :: BTree with the bin. search-tree prop
-- (bstAdd c t) = a version of t with c added as a leaf satisfying
--                the bin. search-tree prop
bstAdd :: Char -> BTree -> BTree
bstAdd c Empty = Branch c Empty Empty
bstAdd c t@(Branch c' tl tr)
    | c==c'     = t
    | c<c'      = Branch c' (bstAdd c tl) tr
    | otherwise = Branch c' tl (bstAdd c tr)

-- (genTree n) = a BTree with the bin. search-tree prop built
--               from 'a'..'z', different n's build different trees
genTree :: Int -> BTree
genTree n = btree ['a'..'z'] n
    where
      btree "" _ = Empty
      btree cs k = Branch c (btree pre k') (btree post k'')
          where
            (pre,c:post) = splitAt ((k*101) `mod` (length cs)) cs
            k'           = (k*103) `mod` 233
            k''          = (k*107) `mod` 233

------------------------------------------------------------------------
-- Problem 9: making BTrees
makeTrees 0 = [Empty]
makeTrees n = fix

------------------------------------------------------------------------
-- Drawing
------------------------------------------------------------------------
-- The following assumes Mac OS X with graphviz installed.
-- (See the download section of: http://www.graphviz.org.)
-- There are probably easy patches to make this work on 
-- other OS's since dump is the only system dependent function.

-- dump str 
--   writes the temp file /tmp/graph.gv with contents str
--   then opens it

dump str = do { withFile "/tmp/graph.gv" WriteMode action
              ; system "open /tmp/graph.gv"
              }
    where action handle = hPutStrLn handle str 

-- (drawBTree t) creates a gv description of t and displays it
drawBTree t = dump $ start  ++ nodes ++ edges ++ end
    where
      start = "digraph g {\n    "
      (nodes,edges) = draw t 1
      end   = "}\n"

-- draw tree root_address = (node_decls, edge_decls)
draw :: BTree -> Integer -> (String,String)
draw Empty m = (inode m,"")
    where inode m = (show m) ++ " [style=invis];\n    "
draw (Branch c tl tr) m 
    = ((node c m)++nl++nr,(edge m ml tl)++(edge m mr tr)++el++er)
    where ml      = 2*m
          mr      = 2*m+1
          (nl,el) = draw tl ml
          (nr,er) = draw tr mr
          node c m      = (show m) ++ " [label=" ++ (show [c]) ++ "];\n    "
          edge m n Empty = (show m) ++ "->" ++ (show n) 
                          ++" [style=invis];\n    "
          edge m n _    = (show m) ++ "->" ++ (show n) ++ ";\n    "


-- (drawMTree t) creates a gv description of t and displays it
drawMTree t = dump $ start  ++ nodes ++ edges ++ end
    where
      start = "digraph g {\n    "
      (nodes,edges) = mdrawAux (t,"X")
      end   = "}\n"


-- draw tree root_address = (node_decls, edge_decls)
mdraw :: MTree -> (String,String)
mdraw mt = mdrawAux (mt,"X")

mdrawAux :: (MTree,String) -> (String,String)
mdrawAux (Node c ts,tag) = (nodes,edges)
    where 
      taggedts = zip ts (map (\k->tag++'x':show k) [0..])
      subs = map mdrawAux taggedts
      edge (Node _ _,tag') = tag ++ " -> " ++ tag' ++            ";\n    "
      nodes = (tag ++ " [label=" ++ (show [c]) ++ "];\n    ")
              ++ concatMap fst subs
      edges = (concatMap edge taggedts) ++ concatMap snd subs
------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------

t10 = Branch 'm' 
     (Branch 'h' 
      (Branch 'c' 
       (Branch 'a' Empty Empty) 
       (Branch 'e' Empty (Branch 'f' Empty Empty)))
      Empty)
     (Branch 'u' 
      (Branch 's' (Branch 'p' Empty Empty) Empty)
      (Branch 'z' Empty Empty))
     
-- QuickCheck BTree generator
instance Arbitrary BTree where
    arbitrary = sized tree
        where
          tree 0 = return Empty
          tree n = do c <- elements (['a'..'z']++['A'..'Z'])
                      m1 <- elements [0..(2*n `div` 3)]
                      m2 <- elements [0..(2*n `div` 3)]
                      liftM2 (Branch c) (variant 1 (tree m1))
                                        (variant 2 (tree m2))

-- QuickCheck MTree generator
instance Arbitrary MTree where
    arbitrary = sized tree
        where
          tree n = do c <- elements (['a'..'z']++['A'..'Z'])
                      m <- elements [0..n]
                      liftM (Node c) (liftM (take m)
                                           (listOf (tree (2*(n-m)`div` 3))))



------------------------------------------------------------------------
-- Testing for bleaves

bleaves_prop t = and $ zipWith (==) (bleaves (bleafRel t)) ['a'..maxBound]

bleafRel :: BTree -> BTree
bleafRel t = fst $ relab (t,['a'..maxBound])
    where 
      relab (Empty,cs) = (Empty,cs)
      relab (Branch _ Empty Empty,c:cs)=(Branch c Empty Empty,cs)
      relab (Branch c tl tr,cs) = (Branch c tl' tr',cs'')
          where (tl',cs') = relab (tl,cs)
                (tr',cs'') = relab (tr,cs')


------------------------------------------------------------------------
-- Testing for mleaves

mleaves_prop t = and $ zipWith (==) (mleaves (mleafRel t)) ['a'..maxBound]

mleafRel :: MTree -> MTree
mleafRel t = fst $ relab (t,['a'..maxBound])
    where 
      relab (Node _ [],c:cs)=(Node c [],cs)
      relab (Node c ts,cs)  = foo (ts,[],cs)
          where foo ([],ts',cs) = (Node c (reverse ts'),cs)
                foo (t:ts,ts',cs) = let (t',cs') = relab (t,cs)
                                    in foo (ts,t':ts',cs')



------------------------------------------------------------------------
-- Testing for postorder

-- Run: quickCheck postorder_prop
postorder_prop t = (str == (take (length str) ['a'..maxBound]))
    where t' = postLabel t
          str = reverse (postorder t')
          
postLabel t = fst (relab t ['a'..maxBound])
    where
      relab Empty cs = (Empty,cs)
      relab (Branch _ tl tr) (c:cs) = (Branch c tl' tr',cs'')
          where (tr',cs')  = relab tr cs
                (tl',cs'') = relab tl cs'

------------------------------------------------------------------------
-- Testing for reconstruct

recon_prop n = (t == (reconstruct (postorder t)))     where t = genTree n

------------------------------------------------------------------------
-- Testing for makeTrees

makeTreesTest = all test [0..8]
    where
      test n = let trees = makeTrees n
               in (all (\t->bcount t == n) trees)
                  && ( length(nub trees) == cats!!n)

-- the first 20 Catalan numbers 
-- see: http://en.wikipedia.org/wiki/Catalan_number
cats = [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 
        742900, 2674440, 9694845, 35357670, 129644790, 477638700, 1767263190]

