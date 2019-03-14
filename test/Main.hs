import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe ( fromJust )
import Control.Monad ( (>=>) )

import Data.Tree.Zipper (Tree, leaf, branch, root, down, up)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
   [ testGroup "Unit tests" [ treeUnitTests ] ]

basicTree :: Tree () Char
basicTree = branch () 
   [ leaf 'A'
   , branch () 
      [ leaf 'B'
      , leaf 'C' 
      ]
   ]


infix 1 @/=

(@/=) :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
x @/= y = x /= y @? "wrong equality detected:  " ++ show x ++ " and " ++ show y

treeUnitTests :: TestTree
treeUnitTests = testGroup "Trees"
   [ testCase "Equality" 
   $ basicTree @?= basicTree
   , testCase "Inequality"
   $ branch () [leaf 'A'] @/= branch () [leaf 'A',leaf 'B']
   , testCase "Equality of nodes" 
   $ (root basicTree) @?= ( fromJust . (down >=> up) $ root basicTree)  --(fromJust $ (down >=> up) . root basicTree)
   ]
