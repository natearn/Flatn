module Flatn where

data Nest a = Value a | Nest [Nest a] deriving (Eq,Show)

-- turn arbitrarily-nested data into a flat list
flatn :: [Nest a] -> [a]
flatn []        = []
flatn [Value i] = [i]
flatn [Nest ns] = flatn ns
flatn (n:ns)    = flatn [n] ++ flatn ns
