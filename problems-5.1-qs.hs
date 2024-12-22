import Data.Char

---IO

quiz :: IO ()
quiz = do
  putStrLn "What is your first name?"
  f_name <- getLine
  putStrLn "What is your last name?"
  l_name <- getLine
  putStrLn "What is your age?"
  age <- getLine
  putStrLn "What is your job?"
  job <- getLine
  putStrLn ("Results: your name is " ++ f_name ++ " " ++ l_name ++ ", your age is " ++ age ++ " and your job is " ++ job ++ ".")

isNumber :: String -> Bool
isNumber = all isDigit

yesNo :: IO Bool
yesNo = undefined

data Person = Person String Int String
  deriving Show

survey :: IO [Person]
survey = do
    person <- quiz
    putStrLn "Enter another person? Answer Y/N"
    continue <- yesNo
    if continue
      then do undefined
      else do undefined





  
 




  
