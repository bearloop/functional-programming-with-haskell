
--Part 1

isEven :: Int -> Bool
isEven n = if (n `mod` 2 == 0)
           then True
           else False

-- main :: IO ()
-- main = print (isEven 3)