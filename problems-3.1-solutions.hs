import Data.Char

---Maps

toLowerSt :: String -> String
toLowerSt = map toLower

toLowerCons :: Char -> Char
toLowerCons c | c `elem` vowels = c
              | otherwise       = toLower c
   where vowels = ['A','E','I','O','U']

toLowerConsSt :: String -> String
toLowerConsSt = map toLowerCons

---Filters

onlyLetters :: String -> String
onlyLetters = filter isLetter

onlyNumsOrLetters = filter (numOrLetter)
                  where numOrLetter c = isDigit c || isLetter c

onlyLettersToLower1 st = map toLower (filter isLetter st)

onlyLettersToLower2 st = filter isLetter (map toLower st)

---Zips

firstNames :: [String]
firstNames = ["Adam","Brigitte","Charlie","Dora","Engelbert"]

secondNames :: [String]
secondNames = ["Ashe","Brown","Cook","De Santis"]

wholeNames :: [(String,String)]
wholeNames = zip firstNames secondNames

countNames :: [String] -> [(Int,String)]
countNames = zip [1..]

wholeNames2 :: [String]
wholeNames2 = zipWith concatSpace firstNames secondNames
      where concatSpace xs ys = xs ++ " " ++ ys 

rollCall :: [String] -> [String]
rollCall = zipWith call xs
      where call name x = name ++ ": " ++ x ++"? 'Present!'" 
            xs          = map show [1..]

