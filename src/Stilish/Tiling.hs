{-|
Module      : Stilish.Tiling
Description : Manipulate the geometry of the tiling tree.
-}
module Stilish.Tiling where

import qualified Data.Tree.Zipper as T
import qualified Data.BoundingBox as BB

type XClientWindow = Int
type TilingTree = Maybe (T.Node Frame Window)
type BoundingBox = BB.BoundingBox Integer

data Compass
   = North
   | East
   | South
   | West

data Orientation 
    = Vertical 
    | Horizontal
    | Stacked

data Workspace = Workspace
   { tiling :: TilingTree
   , floating :: [Window]
   , tag :: String
   }

data Geometry = Geometry
   { ratio :: Int
   , box :: BoundingBox
   }

data Frame = Frame 
   { frameGeometry :: Geometry
   , orientation :: Orientation
   }

data Window = Window 
   { windowGeometry :: Geometry
   , xClientWindow :: XClientWindow
   }

-------------------------------------------------------------------------------

class HasGeometry a where
   getGeometry :: a -> Geometry
   setGeometry :: Geometry -> a -> a
   withGeometry :: a -> (Geometry -> Geometry) -> a
   withGeometry x f = flip setGeometry x . f . getGeometry $ x

instance HasGeometry Window where
   setGeometry geom win = win { windowGeometry = geom }
   getGeometry = windowGeometry

instance HasGeometry Frame where
   setGeometry geom frame = frame { frameGeometry = geom }
   getGeometry = frameGeometry

