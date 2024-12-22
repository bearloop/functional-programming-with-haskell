import Data.Char

badFib :: Int -> Int
badFib 1 = 1
badFib 2 = 1
badFib n = badFib (n-1) + badFib (n-2)

{-badFib 6 = badFib 5 + badFib 4
           = (badFib 4 + badFib 3) + (badFib 3 + badFib 2)
           = ((badFib 3 + badFib 2) + (badFib 2 + badFib 1)) + ((badFib 2 + badFib 1) + 1)
           = (((badFib 2 + badFib 1) + 1) + (1 + 1)) + ((1 + 1) + 1)
           = (((1 + 1) + 1) + (1 + 1)) + ((1 + 1) + 1)
           = 8

The number of calls of badFib is exponentional in n.
-}

fibAcc :: Int -> Integer -> Integer -> Integer
fibAcc 1 cur _ = cur
fibAcc n cur prev = fibAcc (n-1) (cur+prev) cur

goodFib :: Int -> Integer
goodFib n = fibAcc n 1 0

{-goodFib 6 = fibAcc 6 1 0
            = fibAcc 5 1 1
            = fibAcc 4 2 1
            = fibAcc 3 3 2
            = fibAcc 2 5 3
            = fibAcc 1 8 5
            = 8

The number of calls of fibAcc is just n
-}

fibList :: [Integer]
fibList = 1 : 1 : zipWith (+) fibList (tail fibList)

{-fibList = 1 : 1 :             zipWith (+) fibList (tail fibList)
          = 1 : 1 :             zipWith (+) (1 : (drop 1 fibList)) (1 : (drop 2 fiblist))
          = 1 : 1 : 2 :         zipWith (+) (drop 1 fibList)       (drop 2 fibList)
          = 1 : 1 : 2 :         zipWith (+) (1 : drop 2 fibList)   (2 : (drop 3 fiblist))
          = 1 : 1 : 2 : 3 :     zipWith (+) (drop 2 fibList)       (drop 3 fibList)
          = 1 : 1 : 2 : 3 :     zipWith (+) (2 : drop 3 fibList)   (3 : (drop 4 fiblist))
          = 1 : 1 : 2 : 3 : 5 : zipWith (+) (drop 3 fibList)       (drop 4 fibList)
          = 1 : 1 : 2 : 3 : 5 : zipWith (+) (3 : drop 4 fibList)   (5 : (drop 5 fiblist))
          = 1 : 1 : 2 : 3 : 5 : 8 ...

The number of calls of  fibList is roughly 4n, but in practice this is of similiar speed to goodFib.
-}
------------------------- e

wordScore1 :: String -> Int
wordScore1 [] = 0
wordScore1 (x:xs)
    | 1 <= i, i <= 26 = i + wordScore1 xs
    | otherwise = wordScore1 xs
  where
    i = subtract 64 (ord (toUpper x))

wordScore2 xs = sum [ f x | x <- xs , 1 <= f x , f x <= 26 ]
  where
    f x = subtract 64 (ord (toUpper x))

wordScore3 = foldl (+) 0 . filter (1 <=) . filter (<= 26) .  map (subtract 64 . ord .  toUpper)
------------------------- f

concatCheapWords1 :: [String] -> String
concatCheapWords1 [] = []
concatCheapWords1 (w:ws)
   | wordScore1 w <= 42 = ' ' : w ++ concatCheapWords1 ws
   | otherwise = concatCheapWords1 ws

concatCheapWords2 ws = concat [ ' ':w | w <- ws , wordScore1 w <= 42 ]

concatCheapWords3 = foldl (++) [] . map (' ':) . filter ((<=42) . wordScore1)
