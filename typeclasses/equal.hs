{- equal.hs : the Equality typeclass
 -
 - basically the builtin one
 -}

 class Equal a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)


{- Example uses -}

-- Taken from ../custom_modules/traffic.hs
data TrafficLight = Red | Yellow | Green

instance Equal Traffic where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

