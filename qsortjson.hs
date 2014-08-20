{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics

main = Data.ByteString.Lazy.putStr $ encode $ quicksort ([3, 9, 2, 5, 1, 4, 8, 0, 6, 7] :: [Int]) 0

type ClockTick = Int
 
data History a = Branch [a] (History a) a (History a) ClockTick
               | Leaf [a] ClockTick 
	       deriving Show

instance ToJSON a => ToJSON (History a) where
   toJSON (Branch l h1 p h2 t)  =
     object ["name" .= l,
             "tick" .= toJSON t,
             "children" .= [toJSON h1, object ["name" .= toJSON p, "tick" .= toJSON (t+1)], toJSON h2]]
   toJSON (Leaf l t) = object ["name" .= toJSON ("Empty"::String), "tick" .= toJSON t]

--- instance ToJSON ClockTick where
---     toJSON t = object t

quicksort :: Ord a => [a] -> ClockTick -> (History a)
quicksort [] tck          = Leaf [] tck
quicksort as@(pivot:tail) tck  = Branch as (quicksort less (tck+1)) pivot (quicksort greater (tck+1)) tck
    	      where less = [y | y <- tail, y < pivot]
                    greater = [y | y <- tail, y >= pivot]

extractList :: Ord a => (History a) -> ClockTick -> [a]
extractList (Branch l h1 p h2 t) tck = if t == tck then l else (extractList h1 tck) ++ [p] ++ (extractList h2 tck) 
extractList (Leaf l t) tck = [] 

highesttick :: (History a) -> ClockTick
highesttick (Branch l h1 p h2 t) = max (highesttick h1) (highesttick h2) 
highesttick (Leaf l t)  = t

allLists :: Ord a => (History a) -> [[a]]
allLists h = Prelude.map (extractList h) [0..n]
    	             where n = highesttick h
