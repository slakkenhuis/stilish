import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Tree.Zipper as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
   [ unitTests ]

basicTree :: T.Tree () Int
basicTree = T.branch () [T.leaf 1, T.branch () [T.leaf 2, T.leaf 3]]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Equality" $
      1 + 1 @?= (2 :: Int)
  ]
