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
   { nodeContent :: NodeContent internal external
   , nodeContext :: Maybe (NodeContext internal external)
   }


-- | Whereas a 'Node' is decorated with contextual information, 'NodeContent'
-- is the actual base node data type. Its content is a value of type @i@ or
-- @e@. Moreover, internal nodes contain a reference to the child that was most
-- recently in focus — there are no other guarantees as to what position this
-- child occupies with respect to its siblings.
data NodeContent i e 
   = InternalNode i (Node i e)
   | ExternalNode e


-- | Every node except the root node has a context. This context allows us to
-- move through the tree of which it is part. It consists of references to its
-- parent node and to its direct siblings on either side. 
data NodeContext i e = NodeContext
   { parent :: Node i e
   , leftSibling :: Maybe (Node i e)
   , rightSibling :: Maybe (Node i e)
   }


-- | Make a tree consisting of a single external node.
leaf :: e -> Tree i e
leaf label = Tree Node 
   { nodeContent = ExternalNode label
   , nodeContext = Nothing }


-- | Make a tree consisting of an internal node with the given subtrees. The
-- list of subtrees must not be empty, since an internal node must have at
-- least one child.
branch :: i -> [Tree i e] -> Tree i e
branch label (Tree child : children) = Tree parentNode

   where
   parentNode = Node 
      { nodeContent = InternalNode label $ foldl addRight child' children
      , nodeContext = Nothing }

   child' = child 
      { nodeContext = Just NodeContext 
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

   nodeCenter = Node
      { nodeContent = nodeContent node
      , nodeContext = Just NodeContext
         { parent = nodeUp
         , leftSibling = changeParent nodeUp . changeRightSibling (Just nodeCenter) <$> nodeLeft
         , rightSibling = changeParent nodeUp . changeLeftSibling (Just nodeCenter) <$> nodeRight
         }
      }


-- | Auxiliary: Change the context of a non-root node.
changeContext :: (NodeContext i e -> NodeContext i e) -> Node i e -> Node i e
changeContext f n = n { nodeContext = f <$> nodeContext n }


-- | Auxiliary: Change the representative child node of an internal node.
changeChild :: Node i e -> Node i e -> Node i e
changeChild newChild node = case node of
   Node (InternalNode lbl _) ctx -> Node (InternalNode lbl newChild) ctx
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
   ctx <- nodeContext node 
   left' <- leftSibling ctx
   let new = changeParent (changeChild new $ parent ctx) left'
   return new


-- | Move to the sibling right of the given node.
right :: Node i e -> Maybe (Node i e)
right node = do
   ctx <- nodeContext node 
   right' <- rightSibling ctx
   let new = changeParent (changeChild new $ parent ctx) right'
   return new


-- | Ascend to the parent of the given node.
up :: Node i e -> Maybe (Node i e)
up = fmap parent . nodeContext


-- | Descend to the child of the given node that is currently in focus.
down :: Node i e -> Maybe (Node i e)
down node = changeParent node <$> focus node


-- | Auxiliary: Return the child that is currently in focus.
focus :: Node i e -> Maybe (Node i e)
focus node = case nodeContent node of
   InternalNode _ m -> Just m
   ExternalNode _ -> Nothing


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
content n = case nodeContent n of
   InternalNode i _ -> Left i
   ExternalNode e -> Right e


-- | Return the tree of which the node is part.
tree :: Node i e -> Tree i e
tree = Tree . loop up


-- | Return the subtree associated with a node, that is, the tree if it was cut
-- at the current node.
subtree :: Node i e -> Tree i e
subtree = Tree . (\n -> n { nodeContext = Nothing })


-- | Return the root node representing the tree.
root :: Tree i e -> Node i e
root (Tree t) = t


-- | Check if a node is the root node.
isRoot :: Node i e -> Bool
isRoot = isNothing . nodeContext


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

