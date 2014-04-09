{- data-map.hs : Data.Map module file
 - rewrite/use every function in some way
 - used to keep track of progress
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
fromList' :: (Ord k ) => [(k,v)] -> Map.map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

