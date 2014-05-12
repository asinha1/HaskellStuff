{- tasker.hs : the haskell task organizer
 -
 - defined as a table of tables:
 -  - outer table has a key of type Day, and a value of a table
 -  - inner table has a key of int, and a value of __________
 -    which holds all tasks that need to be done in that hour
 -
 - supports adding tasks, updating tasks, removing tasks, and 
 - various "display tasks in range" queries
 -
 - LOOK INTO CLASSES FOR THE RETURN OF THE INNER TABLE
 -}

module Taskerhs
( Day(..)
, Hour(..)
) where

