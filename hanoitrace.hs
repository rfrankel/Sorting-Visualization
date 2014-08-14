module Main where
import Debug.Trace

main = hanoi 3

data Rod = A | B | C deriving Show

hanoi :: Int -> IO ()
hanoi = h A B C "toplevel"
        where
        h :: Rod -> Rod -> Rod -> String -> Int -> IO ()
        h a b c m 0  = return ()
        h a b c m n  =
          do
            trace ("name " ++ msg1 ++ "parent " ++ m) $ h a c b msg1 (n-1) 
            putStr ("move disc from " ++ show a ++ " to " ++ show b ++ "\n")
            trace ("name " ++ msg2 ++ "parent " ++ m) $ h c b a msg2 (n-1)  
	  where msg1 = show n ++ "th level first branch"
                msg2 = show n ++ "th level second branch"	 
