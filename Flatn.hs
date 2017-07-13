module Flatn where

-- Arbitrary nesting isn't allowed in the standard Hakell List,
-- so we need to provide a data type definition for it.
data Nested a = Value a | Nest [Nested a] deriving (Eq,Show)

-- turn arbitrarily-nested data into a flat list
flatn :: [Nested a] -> [a]
flatn []        = []
flatn [Value i] = [i]
flatn [Nest ns] = flatn ns
flatn (n:ns)    = flatn [n] ++ flatn ns
