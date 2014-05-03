{- lockers.hs : the school locker control system module
 -
 - the module lets you manage a locker system, giving you the combination
 - for a locker only if the locker is not taken yet
 -}

-- Dont include the data types, bad security
module Locker
(
) where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

{- Lookup locker combination, only returns if locker isn't already taken -}
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                          then Right code
                         else Left $ "Locker " ++ show lockerNumber 
                            ++ " is already taken!"

{- For testing purposes -}

lockers :: LockerMap
lockers = Map.fromList [(100,(Taken,"ZD83"))
                       ,(101,(Free,"G359"))
                       ,(102,(Free,"G9FJ"))
                       ,(103,(Taken,"KQ1P"))
                       ]


