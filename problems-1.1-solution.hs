
--Function Types
 
isEven :: Int -> Bool
isEven n = (n `mod` 2 == 0)
 
square :: Int -> Int
square n = n * n
 
squareEven1 :: Int -> Int
squareEven1 n = if (n `mod` 2 == 0)
                  then n * n
                  else n
 
squareEven2 :: Int -> Int
squareEven2 n = if (isEven n)
                  then square n
                  else n
                       
 
--Guards
 
myOdd :: Int -> Bool
myOdd n | (n `mod` 2 == 0) = False
      | otherwise        = True
 
grade :: Int -> String
grade n | n <  0    = "Marks cannot be negative"
        | n <  40   = "Fail"
        | n <  50   = "Low Pass"
        | n <  60   = "Medium Pass"
        | n <  70   = "High Pass"
        | n <  80   = "Merit"
        | n <= 100  = "Distinction"
        | otherwise = "Invalid Mark"

       
 
--Recursion
 
factorial :: Int -> Int
factorial n
  | n <= 1 =1
  | otherwise = n * factorial (n-1)
 
triangle :: Int -> Int
triangle n
  | n <= 1 =1
  | otherwise = n + triangle (n-1)
 
total :: [Int] -> Int
total xs | (xs == []) = 0
         | otherwise = head xs + total (tail xs)
 
multiple :: [Int] -> Int
multiple xs | (xs == []) = 1
         | otherwise = head xs * multiple (tail xs)
 
triangle' :: Int -> Int
triangle' n = total [1..n]
 
factorial' :: Int -> Int
factorial' n = multiple [1..n]
 
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | x > y = euclid (x-y) y
 
--Multiple Arguements and Partial Application
 
gcd5 :: Int -> Int
gcd5 = euclid 5
 
facDiv :: Int -> Int -> Int
facDiv m n | (m > n) = (factorial m) `div` (factorial n)
           | (m < n) = (factorial n) `div` (factorial m)
           | otherwise = 1
 
facDiv7 :: Int -> Int
facDiv7 = facDiv 7
 
facTri :: Bool -> Int -> Int
facTri b n | b = factorial n
           | otherwise = triangle n
 
facEvenTriOdd :: Int -> Int
facEvenTriOdd n = facTri (isEven n) n
