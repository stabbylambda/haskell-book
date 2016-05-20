module Chapter11.BinaryTree where

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq,Ord,Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)
insert' b _ = Node Leaf b Leaf

preorder :: BinaryTree a -> [a]
preorder (Node Leaf a Leaf) = [a]
preorder (Node l a r) =
  let left = preorder l
      right = preorder r
  in concat [[a], left, right]
preorder Leaf = []

inorder :: BinaryTree a -> [a]
inorder (Node Leaf a Leaf) = [a]
inorder (Node l a r) =
  let left = inorder l
      right = inorder r
  in concat [left, [a], right]
inorder Leaf = []

postorder :: BinaryTree a -> [a]
postorder (Node Leaf a Leaf) = [a]
postorder (Node l a r) =
  let left = postorder l
      right = postorder r
  in concat [left, right, [a]]
postorder Leaf = []

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  let mappedLeft = mapTree f left
      mappedRight = mapTree f right
      mappedCurrent = f a
  in Node mappedLeft mappedCurrent mappedRight

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc (Node l a r) =
  let left = foldTree f acc l
      right = foldTree f left r
      current = f a right
  in current
foldTree _ acc Leaf = acc

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' = undefined
