{- list.hs : the list data strucuture
 -
 - custom data structure for lists
 -}

--Declaring special infix functions for the structure
--5 puts them at 1 fixity below addition
infixr 5 :-:
infixr 5 .++

data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

--List append
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)


