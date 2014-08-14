{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
import Data.Aeson
import Data.ByteString.Lazy
import GHC.Generics

main = Data.ByteString.Lazy.putStr $ encode $ hanoi 3

data Rod = A | B | C deriving Show

type Move = String 
type Level = Int 

data History = Branch Level History Move History | Leaf  deriving Show

instance ToJSON History where
   toJSON (Branch l h1 m h2)  = object ["name" .= m , "children" .= [toJSON h1, toJSON h2]]
   toJSON Leaf = object ["name" .= ("leaf"::String)]

hanoi :: Int -> History
hanoi = h A B C 
        where
        h :: Rod -> Rod -> Rod -> Int -> History
        h a b c 0  = Leaf 
        h a b c n  =
          Branch n
            (h a c b (n-1)) 
            ("move size " ++ show n ++ " disc from " ++ show a ++ " to " ++ show b ++ "\n")
            (h c b a (n-1))  
