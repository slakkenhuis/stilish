{-|
Module      : Stilish.Tiling
Description : Manipulate the geometry of the tiling tree.
-}
module Stilish.Tiling where

import qualified Data.Tree.Zipper as T
import qualified Data.BoundingBox as BB

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

data Frame = Frame
   { orientation :: Orientation
   , frameRatio :: Int
   , frameBox :: BoundingBox 
   }

data Window = Window
   { xClientWin :: Int 
   , windowRatio :: Int
   , windowBox :: BoundingBox 
   } 

-------------------------------------------------------------------------------

class HasGeometry a where
   setRatio :: Int -> a -> a
   getRatio :: a -> Int
   getBox :: a -> BoundingBox
   setBox :: BoundingBox -> a -> a

   withBox :: a -> (BoundingBox -> BoundingBox) -> a
   withBox x f = flip setBox x . f . getBox $ x

instance HasGeometry Frame where
   setRatio ratio frame = frame { frameRatio = ratio }
   getRatio = frameRatio
   setBox box frame = frame { frameBox = box }
   getBox = frameBox

instance HasGeometry Window where
   setRatio ratio window = window { windowRatio = ratio }
   getRatio = windowRatio
   setBox box window = window { windowBox = box }
   getBox = windowBox

