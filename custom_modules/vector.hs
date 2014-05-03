{- vector.hs : the vector module
 -
 - suppors multiple vector functions
 -}

module Vector
( Vector(..)
, vplus
, vectMult
, scalarMult
) where

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i1 j1 k1) `vplus` (Vector i2 j2 k2) = Vector (i1+i2) (j1+j2) (k1+k2)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i1 j1 k1) `vectMult` (Vector i2 j2 k2) = Vector (i1*i2) (j1*j2) (k1*k2)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i1 j1 k1) `scalarMult` (Vector i2 j2 k2) = i1*i2 + j1*j2 + k1*k2

