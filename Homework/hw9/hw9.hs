--HW09 I/O Gabe Adams CIS 352 4/5/2016
import Data.List (unlines,replicate)

printFlipped :: Int -> IO()
printFlipped 0 = return ()
printFlipped n = do {a<-getLine;
                     printFlipped (n-1);
                     putStrLn $ reverse $ a}

ask :: String -> IO Char
ask str = do {putStrLn str;
			  a <- getLine;
			  if a == "" then return 'n'
			  else return (a !! 0)}

	
game :: IO()
game = do { putStrLn "Think of a number between 1 and 100"
          ; value <- binSearch 1 100
          ;  putStrLn ("Your number must be "++(show value))
         }

binSearch :: Int -> Int -> IO Int 
binSearch low high
    = if low==high
      then return high
      else do { let mid = (low+high) `div` 2
              ; ans <- ask ("Is your number <= " ++(show mid)++ " ?")
              ; if ans=='y' then binSearch low (mid-1) else binSearch (mid+1) high
              }