-----Tail Recursion

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
fibAcc 1 cur _ = undefined
fibAcc n cur prev = fibAcc undefined undefined undefined

goodFib :: Int -> Integer
goodFib n = undefined

{-goodFib 6 = fibAcc ? ? ?
            = fibAcc ? ? ?
            = ...
-}

fibList :: [Integer]
fibList = 1 : 1 : zipWith undefined undefined undefined

{-fibList = 1 : 1 :     zipWith ? ? ?
          = 1 : 1 :     zipWith ? ? ?
          = 1 : 1 : 2 : zipWith ? ? ?
          = 1 : 1 : 2 : zipWith ? ? ?
          = ...
-}

-----Hard Recursion Exercises

wordScore :: String -> Int
wordScore = undefined

concatCheapWords :: [String] -> String
concatCheapWords = undefined

