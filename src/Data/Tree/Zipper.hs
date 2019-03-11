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


-- | Transform a function on nodes to a function on trees.
--
-- For example;
--
--    let t = branch () [leaf 2, branch () [leaf 3]]
--    withTree t $ addFront (leaf 1) >=> lastChild >=> addBack (leaf 4)
withTree :: Monad m 
         => Tree i e 
         -> (Node i e -> m (Node i e)) 
         -> m (Tree i e)
withTree t f = fmap tree . f . root $ t


--------------------------------------------------------------------------------
-- * Construction

-- | A 'Tree' is represented simply by its root node.
newtype Tree internal external = Tree (Node internal external)


-- | Each 'Node' in a 'Tree' is labelled with one of two seperate data types:
-- one for internal and one for external nodes. Every node also has a context.
-- This context allows us to move through the tree of which it is part.
data Node internal external = Node
   { plain :: PlainNode internal external
   , parent :: Maybe (Node internal external)
   , leftSibling :: Maybe (Node internal external)
   , rightSibling :: Maybe (Node internal external)
   }


-- | Whereas a 'Node' is decorated with contextual information, 'PlainNode'
-- is the actual base node data type. Its content is a value of type @i@ or
-- @e@. Moreover, internal nodes contain a reference to the child that was most
-- recently in focus — there are no other guarantees as to what position this
-- child occupies with respect to its siblings.
data PlainNode i e 
   = Internal i (Node i e)
   | External e


-- | Auxiliary: Extend a plain node with context.
--
-- To keep this all running smoothly, we have to observe that all references
-- are appropriately updated at all times. That is: the parent node, if any,
-- must always be focused on the current node. We don't have to change the
-- focus on the right or left siblings: that will be taken care of once they
-- become the current node.
decorate :: PlainNode i e -> Node i e
decorate node = Node 
   { plain = node
   , parent = Nothing
   , leftSibling = Nothing
   , rightSibling = Nothing
   } 


-- | Make a tree consisting of a single external node.
leaf :: e -> Tree i e
leaf = Tree . decorate . External


-- | Make a tree consisting of an internal node with the given subtrees. The
-- list of subtrees must not be empty, since an internal node must have at
-- least one child.
branch :: i -> [Tree i e] -> Tree i e
branch label (Tree firstChild : children) = Tree new

   where
   new = decorate . Internal label 
       $ foldl insert firstChild children
   -- we don't have to do (setParent new firstChild) since that will be taken
   -- care of once we actually descend to the child


-- | Insert a subtree to the right of the given node. The node must not be the
-- root node. Return the new node.
insertRight :: Node i e -> Tree i e -> Node i e 
insertRight node = return node `insertNodeAux` right node


-- | Insert a subtree to the left of the given node. The node must not be the
-- root node. Return the new node.
insertLeft :: Node i e -> Tree i e -> Node i e 
insertLeft node = left node `insertNodeAux` return node


-- | Alias for 'insertRight'.
insert = insertRight


-- | Add a subtree to the front of the children of a node.
addFront :: Tree i e -> Node i e -> Maybe (Node i e)
addFront t = (>>= up) . fmap (flip insertLeft t) . firstChild


-- | Add a subtree to the back of the children of a node.
addBack :: Tree i e -> Node i e -> Maybe (Node i e)
addBack t = (>>= up) . fmap (flip insertRight t) . lastChild


-- | Alias for 'addBack'.
add = addBack


-- | Delete the current node and the subtree under it. Will fail if the node is
-- the root node or if it is the last remaining child of its parent. Returns
-- the node to the left of the deleted node; if there is none, returns the node
-- the right of it.
delete :: Node i e -> Maybe (Node i e)
delete n
   =   setLeft (setRight (rightSibling n) <$> leftSibling n) <$> right n
   <|> setRight (setLeft (leftSibling n) <$> rightSibling n) <$> left n
      

-- | Auxiliary: Add a subtree between two nodes that are supposedly adjacent
-- siblings. Will be nonsensical if they are not, and will throw an error if
-- one of them is a root node.
insertNodeAux :: Maybe (Node i e) -> Maybe (Node i e) -> Tree i e -> Node i e
insertNodeAux nodeLeft nodeRight (Tree node) = nodeCenter 

   where
   nodeUp = (nodeLeft <|> nodeRight) >>= parent
   nodeCenter = node
      { parent = setChild nodeCenter <$> nodeUp
      , leftSibling = setRight (Just nodeCenter) <$> nodeLeft
      , rightSibling = setLeft (Just nodeCenter) <$> nodeRight
      }


-- | Auxiliary: Change the focused child node of an internal node.
setChild :: Node i e -> Node i e -> Node i e
setChild newChild node = case node of
   Node (Internal lbl _) p l r -> Node (Internal lbl newChild) p l r
   _ -> error "Only internal nodes can have children"


-- | Auxiliary: Change the parent of a non-root node.
setParent :: Node i e -> Node i e -> Node i e 
setParent new node = node { parent = pure new }


-- | Auxiliary: Change the left sibling of a non-root node.
setLeft :: Maybe (Node i e) -> Node i e -> Node i e 
setLeft new node = node { leftSibling = new }


-- | Auxiliary: Change the left sibling of a non-root node.
setRight :: Maybe (Node i e) -> Node i e -> Node i e 
setRight new node = node { rightSibling = new }


-------------------------------------------------------------------------------
-- * Traversing

-- | Move to the sibling left of the given node.
left :: Node i e -> Maybe (Node i e)
left node = do
   parent' <- parent node
   left' <- leftSibling node
   let new = setRight (Just node) . setParent (setChild new parent') $ left'
   return new


-- | Move to the sibling right of the given node.
right :: Node i e -> Maybe (Node i e)
right node = do
   parent' <- parent node
   right' <- rightSibling node
   let new = setLeft (Just node) . setParent (setChild new parent') $ right'
   return new


-- | Ascend to the parent of the given node.
up :: Node i e -> Maybe (Node i e)
up node = setChild node <$> parent node


-- | Descend to the child of the given node that is currently in focus.
down :: Node i e -> Maybe (Node i e)
down node = setParent node <$> focus node


-- | Return the first child of the given node.
firstChild :: Node i e -> Maybe (Node i e)
firstChild = fmap (loop left) . down


-- | Return the last child of the given node.
lastChild :: Node i e -> Maybe (Node i e)
lastChild = fmap (loop right) . down 


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
descendants n = n : concatMap descendants (children n)


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


-- | Return the label of the given node for trees where all labels are of the
-- same type.
content' :: Node a a -> a
content' = either id id . content


-- | Return the tree of which the node is part.
tree :: Node i e -> Tree i e
tree = Tree . loop up


-- | Return the subtree associated with a node, that is, the tree if it was cut
-- at the current node.
subtree :: Node i e -> Tree i e
subtree = Tree . decorate . plain


-- | Return the root node representing the tree.
root :: Tree i e -> Node i e
root (Tree t) = t


-- | Check if a node is the root node.
isRoot :: Node i e -> Bool
isRoot = isNothing . parent


-- | Check if a node has no siblings.
isOnlyChild :: Node i e -> Bool
isOnlyChild node = isNothing $ leftSibling node <|> rightSibling node


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


instance Foldable (Tree i) where
   foldr f z = foldr f z . root

instance Foldable (Node i) where
   foldr f z = foldr' f z . loop rightSibling

      where
      foldr' f z node = 
         let z' = case plain node of
                     External x -> f x z
                     Internal _ m -> foldr f z m
         in maybe z' (\k -> foldr' f z' k) (leftSibling node)

