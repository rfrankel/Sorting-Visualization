{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics

main = Data.ByteString.Lazy.putStr $ encode $ quicksort ([3, 9, 2, 5, 1, 4, 8, 0, 6, 7] :: [Int])

data History a = Branch [a] (History a) a [a] (History a)
               | Leaf [a]  deriving Show

instance ToJSON a => ToJSON (History a) where
   toJSON (Branch l1 h1 p l2 h2)  = toJSON [object ["name" .=  toJSON l1, "children" .= toJSON h1], object ["name" .= toJSON p], object ["name" .=  toJSON l1, "children" .= toJSON h1]]
   toJSON (Leaf l) = object ["name" .= toJSON l]

quicksort :: Ord a => [a] -> (History a)
quicksort []           = Leaf []
quicksort (pivot:tail) = Branch less (quicksort less) pivot greater (quicksort greater) 
    	      where less = [y | y <- tail, y < pivot]
                    greater = [y | y <- tail, y >= pivot]


