import Data.Char

----List Comprehension

toLowerSt :: String -> String
toLowerSt st = [toLower c | c <- st]

onlyLetters :: String -> String
onlyLetters st = [ c | c <- st, isLetter c]

onlyNumsOrLetters :: String -> String
onlyNumsOrLetters st = [ c | c <- st, (isLetter c || isDigit c)]

onlyLettersToLower :: String -> String
onlyLettersToLower st = [ toLower c | c <- st, isLetter c]

--onlyLettersToLower eliminates non-letters before converting them to lowercase, so it corresponds to:

--onlyLettersToLower1 :: String -> String
--onlyLettersToLower st = map toLower (filter isLetter st)
