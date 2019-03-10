{-|
Module      : Data.Tree.Zipper
Description : Datastructure for a zipper tree.

This module provides a datastructure for a Zipper tree in which internal and
external nodes may be labelled by different datatypes.

Simply put, a Zipper tree is a purely functional datatype that allows for quick
manipulation due to passing around the constituent part that is currently in
'focus', together with contextual inforation. See Huet 1997.
-}
module Data.Tree.Zipper where

import Data.Maybe ( fromJust, isNothing )
import Data.Either ( lefts, rights )
import Control.Applicative ( (<|>) )
import Control.Monad ( (>=>) )

-------------------------------------------------------------------------------
-- * Auxiliaries

-- | Repeatedly apply an action that can fail, until it does.
loop :: (a -> Maybe a) -> a -> a
loop f = last . loopCollect f 


-- | Repeatedly apply an action that can fail, until it does. Aggregate the
-- successive results in a list that includes the initial value.
--
-- I could also be more generic and take any MonadPlus instead of just Maybe.
-- Also, I saw a library implementation of this same function at:
--    https://hackage.haskell.org/package/monad-loops/
loopCollect :: (a -> Maybe a) -> a -> [a]
loopCollect f x = case f x of
   Nothing -> [x]
   Just y -> x : loopCollect f y


--------------------------------------------------------------------------------
-- * Construction

-- | A 'Tree' is represented simply by its root node.
newtype Tree internal external = Tree (Node internal external)


-- | Each 'Node' in a 'Tree' is labelled with one of two seperate data types:
-- one for internal and one for external nodes. 
data Node internal external = Node
   { plain :: PlainNode internal external
   , context :: Maybe (Context internal external)
   }


-- | Whereas a 'Node' is decorated with contextual information, 'PlainNode'
-- is the actual base node data type. Its content is a value of type @i@ or
-- @e@. Moreover, internal nodes contain a reference to the child that was most
-- recently in focus — there are no other guarantees as to what position this
-- child occupies with respect to its siblings.
data PlainNode i e 
   = Internal i (Node i e)
   | External e


-- | Every node except the root node has a context. This context allows us to
-- move through the tree of which it is part. It consists of references to its
-- parent node and to its direct siblings on either side. 
data Context i e = Context
   { parent :: Node i e
   , leftSibling :: Maybe (Node i e)
   , rightSibling :: Maybe (Node i e)
   }


-- | Make a tree consisting of a single external node.
leaf :: e -> Tree i e
leaf label = Tree Node 
   { plain = External label
   , context = Nothing }


-- | Make a tree consisting of an internal node with the given subtrees. The
-- list of subtrees must not be empty, since an internal node must have at
-- least one child.
branch :: i -> [Tree i e] -> Tree i e
branch label (Tree firstChild : children) = Tree parentNode

   where
   parentNode = Node 
      { plain = Internal label $ foldl addRight firstChild' children
      , context = Nothing }

   firstChild' = firstChild
      { context = Just Context 
         { parent = parentNode
         , leftSibling = Nothing
         , rightSibling = Nothing }
      }


-- | Insert a subtree to the right of the given node. The node must not be the
-- root node. Return the new node.
addRight :: Node i e -> Tree i e -> Node i e 
addRight node = return node `addNodeAux` right node


-- | Insert a subtree to the left of the given node. The node must not be the
-- root node. Return the new node.
addLeft :: Node i e -> Tree i e -> Node i e 
addLeft node = left node `addNodeAux` return node


-- | Delete the current node and the subtree under it. Will fail if the node is
-- the root node or if it is the last remaining child of its parent.
delete :: Node i e -> Maybe (Node i e)
delete node = undefined -- TODO


-- | Auxiliary: Add a subtree between two nodes that are supposedly adjacent
-- siblings. Will be nonsensical if they are not, and will throw an error if
-- one of them is a root node.
addNodeAux :: Maybe (Node i e) -> Maybe (Node i e) -> Tree i e -> Node i e
addNodeAux nodeLeft nodeRight (Tree node) = nodeCenter where

   nodeUp = fromJust $ (nodeLeft <|> nodeRight) >>= up

   nodeCenter = node
      { context = Just Context
         { parent = changeFocus nodeCenter nodeUp
         , leftSibling = changeRightSibling (Just nodeCenter) <$> nodeLeft
         , rightSibling = changeLeftSibling (Just nodeCenter) <$> nodeRight
         }
      }


-- | Auxiliary: Change the context of a non-root node.
changeContext :: (Context i e -> Context i e) -> Node i e -> Node i e
changeContext f n = n { context = f <$> context n }


-- | Auxiliary: Change the representative child node of an internal node.
changeFocus :: Node i e -> Node i e -> Node i e
changeFocus newChild node = case node of
   Node (Internal lbl _) ctx -> Node (Internal lbl newChild) ctx
   _ -> error "only internal nodes can have children"


-- | Auxiliary: Change the parent of a non-root node.
changeParent :: Node i e -> Node i e -> Node i e 
changeParent new = changeContext (\c -> c { parent = new })


-- | Auxiliary: Change the left sibling of a non-root node.
changeLeftSibling :: Maybe (Node i e) -> Node i e -> Node i e 
changeLeftSibling new = changeContext (\c -> c { leftSibling = new })


-- | Auxiliary: Change the left sibling of a non-root node.
changeRightSibling :: Maybe (Node i e) -> Node i e -> Node i e 
changeRightSibling new = changeContext (\c -> c { rightSibling = new })


-------------------------------------------------------------------------------
-- * Traversing

-- | Move to the sibling left of the given node.
left :: Node i e -> Maybe (Node i e)
left node = do
   ctx <- context node 
   left' <- leftSibling ctx
   let new = changeParent (changeFocus new $ parent ctx) left'
   return new


-- | Move to the sibling right of the given node.
right :: Node i e -> Maybe (Node i e)
right node = do
   ctx <- context node 
   right' <- rightSibling ctx
   let new = changeParent (changeFocus new $ parent ctx) right'
   return new


-- | Ascend to the parent of the given node.
up :: Node i e -> Maybe (Node i e)
up = fmap parent . context


-- | Descend to the child of the given node that is currently in focus.
down :: Node i e -> Maybe (Node i e)
down node = changeParent node <$> focus node


-- | Return the first child of the given node.
firstChild :: Node i e -> Maybe (Node i e)
firstChild = down >=> return . loop left


-- | Return the last child of the given node.
lastChild :: Node i e -> Maybe (Node i e)
lastChild = down >=> return . loop right 


-- | Find the first leaf node that satisfies the predicate.
find :: (e -> Bool) -> Tree i e -> Node i e
find = undefined --TODO


-- | Find the first internal node that satisfies the predicate.
findInternal :: (i -> Bool) -> Tree i e -> Node i e
findInternal = undefined --TODO


-------------------------------------------------------------------------------
-- * Querying

-- | Auxiliary: Return the child that is currently in focus.
focus :: Node i e -> Maybe (Node i e)
focus node = case plain node of
   Internal _ m -> Just m
   External _ -> Nothing


-- | Return all children of the given node.
children :: Node i e -> [Node i e]
children = maybe [] siblings . down


-- | Return all siblings of the given node.
siblings :: Node i e -> [Node i e]
siblings n = reverse (loopCollect left n) ++ tail (loopCollect right n)


-- | Return a list containing the current node and all its descendants.
descendants :: Node i e -> [Node i e]
descendants n = undefined -- TODO


-- | Return the contents of all internal nodes. 
internals :: Node i e -> [i]
internals = lefts . map content . descendants


-- | Return the contents of all external nodes.
leaves :: Node i e -> [e]
leaves = rights . map content . descendants


-- | Return the label of the given node.
content :: Node i e -> Either i e
content n = case plain n of
   Internal i _ -> Left i
   External e -> Right e


-- | Return the tree of which the node is part.
tree :: Node i e -> Tree i e
tree = Tree . loop up


-- | Return the subtree associated with a node, that is, the tree if it was cut
-- at the current node.
subtree :: Node i e -> Tree i e
subtree = Tree . (\n -> n { context = Nothing })


-- | Return the root node representing the tree.
root :: Tree i e -> Node i e
root (Tree t) = t


-- | Check if a node is the root node.
isRoot :: Node i e -> Bool
isRoot = isNothing . context


-- | Check if a node has no siblings.
isOnlyChild :: Node i e -> Bool
isOnlyChild node = isNothing $ left node <|> right node


-------------------------------------------------------------------------------
-- * Instances

instance (Show i, Show e) => Show (Tree i e) where
   show = show . root

instance (Show i, Show e) => Show (Node i e) where
   show = show' "" . (:[])

      where
      show' _        []          = ""
      show' leveller (node:rest) = 
         let (prefix, level)
                | null rest = (" └╴","   ")
                | otherwise = (" ├╴"," │ ")
         in  "\n" ++ leveller ++ prefix ++ either show show (content node) ++
             show' (leveller ++ level) (children node) ++ 
             show' leveller            rest

