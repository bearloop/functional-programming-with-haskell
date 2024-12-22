data IntTree = Empty | Node Int IntTree IntTree
  deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = undefined
isEmpty _     = undefined

rootValue :: IntTree -> Int
rootValue Empty        = undefined
rootValue (Node i _ _) = undefined

height :: IntTree -> Int
height = undefined

find :: Int -> IntTree -> Bool
find = undefined


-------------------------

{-
instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]
-}

------------------------- Exercise 2

member :: Int -> IntTree -> Bool
member = undefined   

largest :: IntTree -> Int
largest Empty            = undefined
largest (Node x l Empty) = undefined
largest (Node x l r)     = undefined

deleteLargest :: IntTree -> IntTree
deleteLargest = undefined

delete :: Int -> IntTree -> IntTree
delete _ Empty = undefined
delete y (Node x l r)
    | y < x     = undefined
    | y > x     = undefined
    | isEmpty l = undefined
    | otherwise = undefined


