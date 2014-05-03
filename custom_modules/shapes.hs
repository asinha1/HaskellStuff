{- shapes.hs : shapes module
 -
 - allows for creation and transformations of various shapes
 -
 - Currently Supported Shapes:
 -  Circles
 -  Rectangles
 -
 - Currently Supported Transformations
 -  Translation
 -}

module Shapes
( Point(..)
, Shape(..)
, surface
, translate
, originCircle
, originRect
) where

data Point  = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = 
  (abs $ x2 - x1) * (abs $ y2 - y1)

translate :: Shape -> Float -> Float -> Shape
translate (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
translate (Rectangle (Point x1 y1) (Point x2 y2)) a b = 
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

originCircle :: Float -> Shape
originCircle r = Circle (Point 0 0) r

originRect :: Float -> Float -> Shape
originRect width height = Rectangle (Point 0 0) (Point width height)
