{- paren-match.hs : the Paren Match module
 -
 - checks if a given string of the type Paren is matched
 - written using references from the 15-150 SML paren match code
 -}

module ParenMatch
( Paren(..)
, isMatched,
) where

import Data.List

data Paren = O | C deriving (Eq,Ord)

isMatched :: String -> Bool
isMatched s = (pm s') == (0,0)
  where s' = map (\x -> if x=='(' then O else C) s
        pm s =
          case s of [] -> (0,0)
                    [O] -> (0,1)
                    [C] -> (1,0)
                    _ -> 
                      let (left,right) = splitAt (n `div` 2) s
                          ((i,j),(k,l)) = (pm left, pm right)
                      in  if j > k then (i, l+j-k) else (i+k-j,l)
                      where n = length s



