{- data-map.hs : Data.Map module file
 - rewrite/use every function in some way
 - used to keep track of progress
 -
 - case study at end
 -}

import qualified Data.Map as Map

{- BASIC FUNCTIONS
 -
 - fromList : takes in a list of (key,value) pairs, and returns a map
 - with those keys and values. If there are duplicate values for some
 - key, the last one in the list is kept
 - Map.fromList :: (Ord k) => [(k,v)] -> Map.map k v
 -
 - empty : returns an empty map
 -
 - insert : takes in a key, value, and map, and returns a new map with
 - the added pair
 -
 - null : checks if map is empty
 -
 - size : returns size of map
 -
 - singleton : takes in a key-value pair, returns a single mapping
 -
 - lookup : map lookup, uses Just _ or Nothing
 -
 - member : takes key and map, returns if the key is in the map or not
 -
 - map/filter : same as list, but for maps
 -
 - toList : returns the map as a list of (k,v)
 -
 - keys : returns list of keys
 -
 - elems : returns list of values
 -
 - fromListWith : fromList, but takes in a function to deal with duplicate
 - values for same key
 -
 - insertWith : insert, but takes in a function to deal with duplicate values
 - for the same key
 -}

-- possible implementation of fromList
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

{- CASE STUDY : implement map for phone book contacts
 - key : name (all lowercase)
 - value : numbers (all a list of strings)
 -
 - This is just to get some basic understanding of the map module. A fully
 - working phonebook module will be made soon
 -}

-- Initialize a phonebook for the user
createPhoneBook :: [(String,String)] -> Map.Map String [String]
createPhoneBook = Map.fromListWith (++) . map (\(k,v) -> (k,[v]))

-- Add contact to phonebook
addContact :: String -> String -> Map.Map String [String] -> 
              Map.Map String [String]
addContact k v book = Map.insertWith (++) k [v] book

-- Add multiple contacts to phonebook
addContacts :: [(String,String)] -> Map.Map String [String] ->
                Map.Map String [String]
addContacts list book = 
  foldr (\(k,v) acc -> Map.insertWith (++) k [v] acc) book list
