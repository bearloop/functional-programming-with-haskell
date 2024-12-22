-- a. Define the data type NatTerTree (use the Word data type to represent Natural numbers)
data NatTerTree = Empty
                | Node Word NatTerTree NatTerTree NatTerTree
                deriving Show

-- b. Implement the “findNatural” function that returns a Boolean – The function returns True if a value exists in the left or middle node of every subtree, and False otherwise

findNatural :: Word -> NatTerTree -> Bool
findNatural = findNatural' False
                where
                    findNatural' :: Bool -> Word -> NatTerTree -> Bool
                    findNatural' _ w Empty = False
                    findNatural' isRightMost w (Node v l m r) = findNatural' False w l || 
                                                                findNatural' False w m ||
                                                                findNatural' True w r ||
                                                                ((isRightMost==False) && (w==v))

findNaturalSimple :: Word -> NatTerTree -> Bool
findNaturalSimple _ Empty = False
findNaturalSimple w (Node v l m _)
                | w == v = True
                | otherwise = findNaturalSimple w l || findNaturalSimple w m


testCase :: NatTerTree
testCase = Node 1
            (Node 2
                (Node 5 Empty Empty Empty)
                (Node 6 Empty Empty Empty)
                (Node 7 Empty Empty Empty)
            )
            (Node 3 Empty Empty Empty)
            (Node 4
                (Node 8 Empty Empty Empty)
                (Node 9 Empty Empty Empty)
                (Node 10 Empty Empty Empty)
            )
