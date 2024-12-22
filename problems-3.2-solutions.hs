data IntTree = Empty | Node Int IntTree IntTree

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))

------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        = 0
rootValue (Node i _ _) = i

height :: IntTree -> Int
height Empty        = 0
height (Node _ s t) = 1 + max (height s) (height t)

find :: Int -> IntTree -> Bool
find y Empty        = False
find y (Node x l r) = find y l || y == x || find y r


-------------------------


instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]


------------------------- Exercise 2 

member :: Int -> IntTree -> Bool
member _ Empty = False
member i (Node j s t)
    | i == j    = True
    | i <  j    = member i s
    | otherwise = member i t    

member' :: Int -> IntTree -> Bool
member' _ Empty = False
member' i (Node j s t) = (i < j && member' i s) || i == j || (i > j && member' i t)

largest :: IntTree -> Int
largest Empty            = error "largest: empty tree"
largest (Node x l Empty) = x
largest (Node x l r)     = largest r

deleteLargest :: IntTree -> IntTree
deleteLargest Empty            = error "deleteLargest: empty tree"
deleteLargest (Node x l Empty) = l
deleteLargest (Node x l r)     = Node x l (deleteLargest r)

delete :: Int -> IntTree -> IntTree
delete _ Empty = Empty
delete y (Node x l r)
    | y < x     = Node x (delete y l) r
    | y > x     = Node x l (delete y r)
    | isEmpty l = r
    | otherwise = Node (largest l) (deleteLargest l) r
