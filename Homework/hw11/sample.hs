import Parsing 
import Data.Char

{- 
   Suppose we have a simple text-file database of students and their
   test scores.  Each student gets a line (a row) in the textfile.  
   (See row1 and row2 below for examples.) The format of these lines is:

   familyName, givenName; (ddd)ddd-dddd # num # num # num # num # num 

   where
     - familyName and givenName are strings of letters
     - each d is a digit (i.e.,, one of '0'...'9')
     - each num is a natural number.
     - the num's are the test scores 
     - each student has at least one test score

   We want to write a little program, testAvg, that takes a row and
     - gets the student's name
     - reads the student's test scores
     - returns a string with the student's name and average test score

   E.g.,

      *Main> parse testAvg row1
      [("Ezekiel Rawlins has an avg of 83","")]
     
-}

------------------------------------------------------------------------               
-- sample rows
row1 = "Rawlins, Ezekiel; (213)822-0779   # 97 # 78 # 84 # 67 # 92 "
row2 = "Alexander, Raymond; (213)735-9812 # 83 # 80 # 94 # 74 # 98 " 

-- the parser for this problem

name       = many1 (sat isLetter)   -- try: parse name "Doe"
skipSpaces = many  (sat isSpace)    -- try: parse skipSpaces "   xx"
digits     = many1 (sat isDigit)    -- try: parse digits "123"

phoneNum   = do { char '('          -- try: parse phoneNum "(213)822-0779"
                ; area <- digits
                ; char ')'
                ; exch <- digits
                ; char '-'
                ; num  <- digits
                ; return (area,exch,num)
                }

person     = do { family <- name   -- try: parse person "Doe, John "  
                ; skipSpaces
                ; char ','
                ; skipSpaces
                ; personal <- name
                ; skipSpaces
                ; return (personal,family)
                }

score      = do { skipSpaces             -- try: parse score " # 99 "    
                ; char '#'
                ; skipSpaces
                ; ds <- digits
                ; skipSpaces
                ; return (read ds :: Int)
                }
-- Also try: parse (many1 score)  "  # 12 # 34 #  56 # 78  "


testAvg    = do { (personal,family) <- person
                ; char ';'
                ; skipSpaces
                ; phoneNum -- we don't save the phone num
                ; scores <- many1 score
                ; let avg = (sum scores) `div` (length scores)
                ; return (personal++" "++family++" has an avg of "++show avg)
                }

-- Try: parse testAvg row1
-- Try: parse testAvg row2

prnAvg row = putStrLn ( fst(head(parse testAvg row)))

-- Try: prnAvg row1
-- Try: prnAvg row2
