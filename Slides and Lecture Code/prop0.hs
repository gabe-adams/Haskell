(------------------------------------------------------------------------
------------------------------------------------------------------------
-- Propositional logic, based on an example of W. Heijltjes & P. Wadler

import Data.List( nub, sort )
-- stuff for QuickCheck
import Control.Monad( liftM, liftM2 )
import Test.QuickCheck( quickCheck, sample, Arbitrary( arbitrary ),
                        Gen, oneof, elements, sized  )

-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :->: Prop
          | Prop :<->: Prop
          deriving (Eq, Ord)
          --deriving (Eq, Ord, Show)
                   
-- some samples
vp, vq, vr, p1, p2, p3 :: Prop
vp = Var "P"
vq = Var "Q"
vr = Var "R"
p1 = ((vp :|: vq) :&: (vp :&: vq))
p2 = ((vp :|: vq) :&: ((Not vp) :&: (Not vq)))
p3 = ((vp :&: (vq :|: vr))
      :&:
      (((Not vp) :|: (Not vq)) :&: ((Not vp) :|: (Not vr))))


type Names = [Name]
type Env = [(Name, Bool)]

lookUp :: Name -> Env -> Bool
lookUp x env = case (lookup x env) of
                 (Just b) -> b
                 Nothing  -> False

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)     =  x
showProp (F)         =  "F"
showProp (T)         =  "T"
showProp (Not p)     =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)   =  "("  ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)   =  "("  ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :->: q)  =  "("  ++ showProp p ++ "->" ++ showProp q ++ ")"
showProp (p :<->: q) =  "("  ++ showProp p ++ "<->" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)     =  lookUp x e
eval e (F)         =  False
eval e (T)         =  True
eval e (Not p)     =  not (eval e p)
eval e (p :|: q)   =  eval e p || eval e q
eval e (p :&: q)   =  eval e p && eval e q
eval e (p :->: q)  =  (not (eval e p)) || eval e q
eval e (p :<->: q) =  eval e p == eval e q

------------------------------------------------------------------------

-- names p = a list of all the names in p (with no reps)
names :: Prop -> Names
names (Var x)       =  [x]
names (F)           =  []
names (T)           =  []
names (Not p)       =  names p
names (p :|: q)     =  nub (names p ++ names q)
names (p :&: q)     =  nub (names p ++ names q)
names (p :->: q)    =  nub (names p ++ names q)
names (p :<->: q)   =  nub (names p ++ names q)

------------------------------------------------------------------------

-- hasNot p = tests if p contains a negation
hasNot :: Prop -> Bool
hasNot (Var x)       =  False
hasNot (F)           =  False
hasNot (T)           =  False
hasNot (Not p)       =  True
hasNot (p :|: q)     =  hasNot p || hasNot q
hasNot (p :&: q)     =  hasNot p || hasNot q
hasNot (p :->: q)    =  hasNot p || hasNot q
hasNot (p :<->: q)   =  hasNot p || hasNot q

------------------------------------------------------------------------

-- replace :: Name -> Prop -> Prop -> Prop
-- replace x r p = the result of replacing each occurrence
--    of (Var x) in p with proposition r.
replace :: Name -> Prop -> Prop -> Prop
replace x r (Var y)       
    | x==y      = r
    | otherwise = Var y
replace x r (F)           =  F
replace x r (T)           =  T
replace x r (Not p)       =  Not (replace x r p)
replace x r (p :|: q)     =  (replace x r p) :|:   (replace x r q)
replace x r (p :&: q)     =  (replace x r p) :&:   (replace x r q)
replace x r (p :->: q)    =  (replace x r p) :->:  (replace x r q)
replace x r (p :<->: q)   =  (replace x r p) :<->: (replace x r q)


------------------------------------------------------------------------

-- envs [n1,...,nk] = a list of all possible environments for these names.
-- E.g.: 
envs :: Names -> [Env]
envs []     = [[]] 
envs [x]    = [[(x,True)],[(x,False)]]
envs (x:xs) = [(x,True):t | t <- ts] ++ [(x,False):t | t <- ts]
    where
      ts = envs xs

satisfiable :: Prop -> Bool
satisfiable p = or [eval e p| e <- envs(names p)]

tautology :: Prop -> Bool
tautology p =  and [eval e p| e <- envs(names p)]

equivalent, equivalent' :: Prop -> Prop -> Bool
equivalent p q = and [eval e p == eval e q | e <- envs theNames]
    where theNames = nub (names p ++ names q)

equivalent' p q = tautology (p :<->: q)

equiv1_prop :: Prop -> Prop -> Bool
equiv1_prop p q = (equivalent p q == equivalent' p q)

equiv2_prop :: Prop -> Bool
equiv2_prop p = (equivalent p p) && (equivalent' p p)

subformulas :: Prop -> [Prop]
subformulas (Var x)     =  [Var x]
subformulas (F)         =  [F]
subformulas (T)         =  [T]
subformulas (Not p)     =  (Not p):subformulas p
subformulas (p :|: q)   =  (p :|: q):nub (subformulas p ++ subformulas q)
subformulas (p :&: q)   =  (p :&: q):nub (subformulas p ++ subformulas q)
subformulas (p :->: q)  =  (p :->: q):nub (subformulas p ++ subformulas q)
subformulas (p :<->: q) =  (p :<->: q):nub (subformulas p ++ subformulas q)


-- For QuickCheck --------------------------------------------------------

instance Show Prop where show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                       , liftM2 (:->:) subform subform
                                       , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)

{- To see what kind of propositions are generated, try:
   sample (arbitrary :: Gen Prop)
-}

-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
    where
      n = length s
      h = (w - n) `div` 2
              
-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True
