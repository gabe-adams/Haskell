module Zeit (Zeit(..),stretch) where

data Zeit = Time Integer Integer

-- Convert Zeits to minutes (not exported)
toMins :: Zeit -> Integer
toMins (Time h m) = 60*h+m 

-- Stretch t f = the Zeit t stretched by amount f
-- E.g.: stretch (Time 1 0) 1.5  = Time 1 30                        
stretch :: Zeit -> Float -> Zeit
stretch t s = fromInteger(round(s * fromIntegral(toMins t)))

instance Eq Zeit where
    t1 == t2  =  toMins t1 == toMins t2
instance Ord Zeit where
    t1 <= t2  =  toMins t1 <= toMins t2
               
instance Num Zeit where
    t1 + t2       =  fromInteger (toMins t1 + toMins t2)
    t1 - t2       =  fromInteger (toMins t1 - toMins t2)
    abs t         =  fromInteger(abs(toMins t))
    t1 * t2       =  error "(*) not defined for Zeit"
    signum t      =  error "signum not defined for Zeit"
    fromInteger n =  Time h m 
        where (h,m) = divMod n 60

instance Show Zeit where
    show (Time h m)
        =  show h ++ " hours and " ++ show m ++ " minutes"
