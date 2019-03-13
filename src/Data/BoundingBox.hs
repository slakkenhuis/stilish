{-|
Module      : Data.BoundingBox
Description : Datastructure for a bounding box.

This module provides a datastructure for a bounding box — a simple type we will
use to organize the layout of windows.
-}
module Data.BoundingBox where

data BoundingBox t = BoundingBox
   { x1 :: t
   , y1 :: t
   , x2 :: t
   , y2 :: t
   }

minX, minY, maxX, maxY :: (Ord a) => BoundingBox a -> a 
minX bb = x1 bb `min` x2 bb
maxX bb = x1 bb `max` x2 bb
minY bb = y1 bb `min` y2 bb
maxY bb = y1 bb `max` y2 bb

width, height :: (Num a) => BoundingBox a -> a
width bb = abs (x1 bb - x2 bb)
height bb = abs (y1 bb - y2 bb)

instance (Ord a, Num a, Show a) => Show (BoundingBox a) where
   show bb = concat
      [ show $ width bb, "x", show $ height bb
      , "+", show $ minX bb, "+", show $ minY bb 
      ]
