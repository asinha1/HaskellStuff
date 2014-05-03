{- yes-no.hs : the YesNo typeclass
 -
 - "If some base case, false, else true"
 - Based off of python/Javascript's ability to throw basically anything
 - into an if statement
 -}

class YesNo a where
  yesno :: a -> Bool
  yesnoIf :: (YesNo y) => y -> a -> a -> a
  yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult
                                        else noResult

{- Instances -}

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno _ = True

