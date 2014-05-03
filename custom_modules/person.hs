{- person.hs : the person module
 -
 - first use of the record datatype constructor
 -
 - Supports the following fields:
 -  first name
 -  last name
 -  age
 -  height
 -  phone number
 -  favorite flavor of ice cream
 -}

module Person
( Person(..)
) where

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Show, Read, Eq)
