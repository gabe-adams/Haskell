import AS1
import Parser1
import Debug.Trace

fixMe = error "Please fix me"
errorMsg = error "Divide-by-zero"
-- eval e | trace ("entering eval with arg: "++ show e) False = undefined 
--eval :: Aexp -> Aexp
eval (Num n)          = n
eval (Add a1 a2)      = (eval a1) + (eval a2)
eval (Sub a1 a2)      = (eval a1) - (eval a2)
eval (Mult a1 a2)     = (eval a1) * (eval a2)
eval (Div a1 a2)      = if eval a2/= 0 then (div (eval a1) (eval a2)) else errorMsg
eval (Cond a1 a2 a3)  = if eval a1/=0 then eval a2 else eval a3

------------------------------------------------------------------------
-- run e 
--   parses e, evaluates e, prints the answer
--   Try: (run "2+3*5")
run :: String -> IO ()
run etxt = do { let e = aparse etxt
              ; putStrLn $ "Evaluating: " ++ show e
              ; let val = eval e
              ; putStrLn $ "    Result: " ++ show val
              }

-- read-eval-print
rep :: IO ()
rep = do { etxt <- getLine;
         ; let e = aparse etxt
         ; let val = eval e
         ; putStrLn $ "Evaluates to:\t"++ show val
         }
