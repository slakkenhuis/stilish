{-|
Module      : Stilish.Tiling
Description : Manipulate the geometry of the tiling tree.
-}
module Stilish.Tiling where

import qualified Data.Tree.Zipper as T
import qualified Data.BoundingBox as BB

type TilingTree = Maybe (T.Node Frame Window)
type BoundingBox = BB.BoundingBox Integer

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
   { frameProportion :: Integer
   , orientation :: Orientation 
   , frameBox :: BoundingBox 
   }

data Window = Window
   { windowRatio :: Integer
   , xClientWin :: Integer 
   , windowBox :: BoundingBox 
   } 
