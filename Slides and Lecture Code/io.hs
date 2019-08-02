-- March 29th version (running this inside of Emacs may be a bit strange)
import Data.List (unlines,replicate)


echo :: IO ()
echo = getChar >>= putChar

echoTwice :: IO ()
echoTwice = getChar >>= (\c -> putChar c >>= (\() -> putChar c))

echoTwice' = getChar >>= (\c -> putChar c >> putChar c)

getTwoChars = getChar >>= (\c1 ->
              getChar >>= (\c2 ->
              return (c1,c2)))                 

getTwoChars'' = do c1 <- getChar
                   c2 <- getChar
                   return (c1,c2)

put4times str = do putStrLn str
                   putStrLn str
                   putStrLn str
                   putStrLn str

putNtimes, putNtimes' :: Int -> String -> IO ()
putNtimes n str = if n <= 1
                  then putStrLn str
                  else do putStrLn str
                          putNtimes (n-1) str

-- alternative
putNtimes' n str = putStr $ unlines $ replicate n str

-- New on March 29

getLine' :: IO String
getLine' = do c <- getChar
              if c=='\n'
                 then return ""
                 else do cs <- getLine' -- the rest of the line
                         return (c:cs)


copy = do str <- getLine
          putStrLn str

getInt = do item <- getLine
            return (read item :: Int)

sumInts = do n <- getInt
             if (n<=0)
             then return 0
             else do m <- sumInts
                     return (n+m)

chattySum
    = do putStrLn "Enter integers one per line"
         putStrLn "This will be summed until a 0 is entered."
         sum <- sumInts
         putStr "The sum is "
         print sum

forever :: IO () -> IO ()
forever a = do {a ; forever a }                            

repeatN :: Int -> IO () -> IO ()
repeatN 0 a = return ()
repeatN n a = do { a ; repeatN (n-1) a }

for, for' :: [a] -> (a -> IO b) -> IO ()
for [] fa     = return ()
for (x:xs) fa = do { fa x; for xs fa }

for' xs fa = sequence_ [fa x | x <- xs]                   


get3lines = do a <- getLine
               b <- getLine
               c <- getLine
               return [a,b,c]

get3lines' = do rs <- sequence [getLine,getLine,getLine]
                return rs
