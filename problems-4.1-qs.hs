import Data.Char

----List Comprehension

toLowerSt :: String -> String
toLowerSt [] = []
toLowerSt (c:cs) = toLower c : toLowerSt cs

toLowerSt2 :: String -> String
toLowerSt2 s = [toLower c | c <- s]

onlyLetters :: String -> String
onlyLetters [] = []
onlyLetters (c:cs) | isLetter c  = c : onlyLetters cs
                   | otherwise   =     onlyLetters cs

onlyLetters2 :: String -> String
onlyLetters2 s = [toLower c | c <- s, isLetter c]

onlyNumsOrLetters :: String -> String

onlyNumsOrLetters s = [ x | x <- s, (isDigit x||isLetter x) ]

onlyLettersToLower :: String -> String
onlyLettersToLower s = [ toLower x | x <- s, isLetter x ]
