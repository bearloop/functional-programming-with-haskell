allTrue1,allTrue2,allTrue3 :: [Bool] -> Bool
allTrue1 [] = True
allTrue1 (x:xs) = x && allTrue1 xs
 
allTrue2 = foldr (&&) True
 
allTrue3 = foldl (&&) True
 
----
 
longestLength1,longestLength2,longestLength3 :: [[a]] -> Int
 
longestLength1 [x] = length x
longestLength1 (x:xs) = max (length x) (longestLength1 xs)
 
longestLength2 = foldr1 max . map length
 
longestLength3 = foldl1 max . map length
 
----
 
sumOddSquares1,sumOddSquares2,sumOddSquares3,sumOddSquares4 :: [Int] -> Int
sumOddSquares1 xs = sum [ x^2 | x <- xs , odd x ]
 
sumOddSquares2 [] = 0
sumOddSquares2 (x:xs)
    | odd x     = x^2 + sumOddSquares2 xs
    | otherwise = sumOddSquares2 xs
 
sumOddSquares3 = foldr (+) 0 . map (^2) . filter odd
 
sumOddSquares4 = foldl (+) 0 . map (^2) . filter odd
 
----
 
shortFWords1,shortFWords2,shortFWords3,shortFWords4,shortFWords5 :: [String] -> Bool
 
shortFWords1 [] = False
shortFWords1 (x:xs)
    | length x == 4, head x == 'F' = True
    | otherwise = shortFWords1 xs
 
shortFWords2 xs = or [ head x == 'F' | x <- xs , length x == 4 ]
 
shortFWords3 = foldr (||) False . map ((=='F') . head) . filter ((== 4) . length)
 
shortFWords4 = foldl (||) False . map ((=='F') . head) . filter ((== 4) . length)
 
shortFWords5 = foldr ((||).(=='F').head) False . filter ((== 4) . length)