--Lists

halving :: Int -> [Int]
halving n | n == 0 = []
          | odd n  = n : halving (n-1)
          | even n = n : halving (div n 2)

collatz :: Int -> [Int]
collatz n | n == 1 = [1]
          | odd n = n : collatz (3*n+1)
          | even n = n : collatz (div n 2)

colLength :: Int -> Int
colLength n = length (collatz n)

-- Pattern Matching

maxList :: [Int] -> Int
maxList [] = 0
maxList [x] = x
maxList (x:xs) | maxList xs > x = maxList xs
               | otherwise = x


allDucks :: [String] -> Bool
allDucks [] = False
allDucks ["duck"] = True
allDucks (x:xs) | x == "duck" = allDucks xs
                | otherwise = False

duckDuckGoose :: [String] -> Bool
duckDuckGoose [] = False
duckDuckGoose ["goose"] = True
duckDuckGoose (x:xs) 
                | x == "duck" = duckDuckGoose xs
                | otherwise = False

--Pairs

ducks :: [(String,Int)]
ducks = [("Donald",6),("Daisy",5),("Huey",2),("Louie",2),("Dewey",2),("Rewey",4),("Dorris",2)]

noDDucks :: [(String,Int)] -> [String]
noDDucks [] = []
noDDucks ((name,age):xs) 
            | take 1 name /= "D" = name : noDDucks xs
            | otherwise = noDDucks xs

youngOrShort :: [(String,Int)] -> Bool
youngOrShort [] = False
youngOrShort ((name,age):xs)
            | age < 3 = True
            | length name <= 3 = True
            | otherwise = youngOrShort xs

describeDucks :: [(String,Int)] -> String
describeDucks [] = ""
describeDucks ((name,age):xs) = name ++ " is a duck who is " ++ show age ++ " years old. " ++ describeDucks xs


