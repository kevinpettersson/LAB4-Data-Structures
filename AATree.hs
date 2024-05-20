--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where
import Debug.Trace (traceEvent)
import Data.Maybe
--import Control.Applicative (Alternative(empty))
--mport Data.Sequence (Seq(Empty), empty)

--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node {  level :: Int,
                                value :: a,
                                left  :: AATree a,
                                right :: AATree a
                              } deriving (Eq, Show, Read)
-- Returns a empty tree.
-- Complexity: O(1)
emptyTree :: AATree a
emptyTree = Empty

-- Takes a value and checks if the value exists in the tree, if it less than parent value we check in the left sub-tree
-- otherwise we check in the right sub-tree and if value dosent exist func returns nothing.
-- Complexity: O(log n)
get :: Ord a => a -> AATree a -> Maybe a
get _ Empty   = Nothing
get x (Node _ v l r)
  | x < v     = get x l
  | x > v     = get x r
  | otherwise = Just x

-- Takes a value and creates a node with two empty children.
-- Complexity: O(1)
singleton :: a -> AATree a
singleton x = Node 1 x Empty Empty

-- If the tree is empty, call singleton function.
-- If the insert value is less than root value insert element into left sub-tree, 
-- and call the skew and split function after inserting to maintain the invariant.
-- By doing so we recurisvly call insert and check the parent value with the new value, minimizing to a new sub-tree.
-- Complexity: O(log n)
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty            = singleton x
insert x (Node lvl v l r)
  | x < v                 = split (skew (Node lvl v (insert x l) r))
  | x > v                 = split (skew (Node lvl v l (insert x r)))
  | otherwise             = Node lvl v l r

-- If parent level is the same as left child level, does a skew.
-- Complexity: O(1)
skew :: AATree a -> AATree a
skew (Node lvl v (Node llvl lv ll lr) r)
  | llvl == lvl = Node llvl lv ll (Node lvl v lr r)
skew tree       = tree

-- If parent level and right child and right grandchild are on the same level split the 3-node.
-- Complexity: O(1)
split :: AATree a -> AATree a
split (Node lvl v l(Node rlvl rv rl (Node rrlvl rrv rrl rrr)))
  | lvl == rlvl && rlvl == rrlvl = Node (rlvl+1) rv (Node lvl v l rl) (Node rrlvl rrv rrl rrr)
split tree                       = tree

-- Recursivly calls the function on the left and right sub-tree and appending the value in between, creating a list.
-- Complexity: O(n)
inorder :: AATree a -> [a]
inorder Empty          = []
inorder (Node _ v l r) = inorder l ++ [v] ++ inorder r

-- Calculates the total amount of nodes in the tree.
-- Complexity: O(n)
size :: AATree a -> Int
size Empty          = 0
size (Node _ _ l r) = 1 + size l + size r

-- Returns 0 if the tree is empty and otherwise adds 1 and choosing the maximum height of the left and right sub-tree.
-- By recursively calling the height function.
-- Complexity: O(n)
height :: AATree a -> Int
height Empty          = 0
height (Node _ _ l r) = 1 + max (height l) (height r)

--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants
-- Complexity: O(n)
checkTree :: Ord a => AATree a -> Bool
checkTree tree =
  isSorted (inorder tree) 
  && all checkLevels (nodes tree)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x : nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
-- Recursivly checks that left node is less or equal to the right node. And that the level of the left node 
-- is greater or equal to the right node.
-- Complexity: O(n)
isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)


-- Checks so the parent lvl is greater than left-child and right child and right grandchild.
-- Complexity: O(n)
checkLevels :: AATree a -> Bool
checkLevels Empty             = True
checkLevels (Node lvl _ l r)
  | lvl > height l            = True
  | lvl >= height r           = True
  | lvl > height (rightSub r) = True
  | otherwise                 = False

-- Complexity: O(1)
isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Returns left subtree
-- Complexity: O(1)
leftSub :: AATree a -> AATree a
leftSub Empty          = Empty
leftSub (Node _ _ l _) = l

--return right subtree.
-- Complexity: O(1)
rightSub :: AATree a -> AATree a
rightSub Empty          = Empty
rightSub (Node _ _ _ r) = r

extractMin :: Ord a => AATree (a, b) -> Maybe ((a, b), AATree (a, b))
extractMin Empty = Nothing
extractMin (Node lvl v Empty r) = Just (v, r)
extractMin (Node lvl v l r) = let (Just (minVal, newLeft)) = extractMin l in Just (minVal, Node lvl v newLeft r)

null :: AATree a -> Bool
null Empty = True
null _ = False


--------------------------------------------------------------------------------



