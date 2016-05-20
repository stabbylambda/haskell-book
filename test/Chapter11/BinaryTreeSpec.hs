module Chapter11.BinaryTreeSpec where

import Chapter11.BinaryTree

import Test.Hspec

spec :: Spec
spec =
  describe "Binary Tree" $
      let testTree =
            Node (Node Leaf 1 Leaf)
                2
                (Node Leaf 3 Leaf)
          testTree' =
            Node (Node Leaf 3 Leaf)
                1
                (Node Leaf 4 Leaf)
          mapExpected =
            Node (Node Leaf 4 Leaf)
                2
                (Node Leaf 5 Leaf)
  in do it "mapTree" $ mapTree (+ 1) testTree' `shouldBe` mapExpected
        it "foldTree" $ foldTree (+) 0 testTree `shouldBe` 6
        -- it "map from foldTree" $
        --   mapTree' (+ 1) testTree' `shouldBe` mapExpected
        it "preorder" $ preorder testTree `shouldBe` [2,1,3]
        it "inorder" $ inorder testTree `shouldBe` [1,2,3]
        it "postorder" $ postorder testTree `shouldBe` [1,3,2]
