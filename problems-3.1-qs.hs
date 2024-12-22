import Data.Char
import Data.ByteString (count)

---Maps

toLowerSt :: String -> String
toLowerSt [] = []
toLowerSt (c:cs) = toLower c : toLowerSt cs

toLowerSt2 :: String -> String
toLowerSt2 cc = map toLower cc

toLowerCons :: String -> String
toLowerCons cc = map (\x -> if (x /= 'A' && x /= 'E' && x /= 'I' && x /= 'O') then (toLower x) else x) cc

-- toLowerConsSt = undefined

---Filters

onlyLetters :: String -> String
onlyLetters [] = []
onlyLetters (c:cs) | isLetter c  = c : onlyLetters cs
                   | otherwise   =     onlyLetters cs

onlyLetters2 :: String -> String
onlyLetters2 str = filter isLetter str

checkIfNumOrLetter:: Char -> Bool
checkIfNumOrLetter x | isLetter x = True
                     | isDigit x = True
                     | otherwise = False

onlyNumsOrLetters :: String -> String
onlyNumsOrLetters str = filter checkIfNumOrLetter str


onlyLettersToLower1 :: String -> String
onlyLettersToLower1 st = map toLower (filter isLetter st)

onlyLettersToLower2 :: String -> String
onlyLettersToLower2 st = filter isLetter (map toLower st)

---Zips

firstNames :: [String]
firstNames = ["Adam","Brigitte","Charlie","Dora"]

secondNames :: [String]
secondNames = ["Ashe","Brown","Cook","De Santis"]

wholeNames :: [(String, String)]
wholeNames = zip firstNames secondNames
-- wholeNames fn sn = zip fn sn

countNames :: [String] -> [(Int,String)]
countNames n = zip [i | i<- [1..length n]] n

wholeNames2 :: [String]
wholeNames2 = zipWith (\x1 x2-> x1++" "++x2) firstNames secondNames
              


-- rollCall :: [String] -> [String]
-- rollCall ls = zipWith  call  xs++"? 'Present!'" ls 
--            where call = [ show i | i<- [1..length ls]]
--             --      xs   = "? 'Present!'"

rollCall :: [String] -> [String]
rollCall = zipWith call xs
      where call name x = name ++ ":" ++ x ++ "? 'Present!'"
            xs          = map show [1..]
