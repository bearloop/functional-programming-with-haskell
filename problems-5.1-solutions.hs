import Data.Char
 
quiz :: IO Person
quiz = do
  putStrLn "What is your first name?"
  firstName <- getLine
  putStrLn "What is your second name?"
  secondName <- getLine
  putStrLn "What is your age?"
  age <- getLine
  if not (all isDigit age)
    then do
    putStrLn "Sorry, that's not a number."
    quiz
    else do
    putStrLn "What is your job?"
    job <- getLine
    putStrLn ("Results: your name is " ++ firstName ++ " " ++ secondName ++ ", your age is "  ++ age ++ " and your job is " ++ job ++ ".")
    putStrLn "Is this data correct? Answer Y/N"
    correct <- yesNo
    if correct
      then return (Person (firstName ++ secondName) (read age) job)
      else quiz
   
isNumber :: String -> Bool
isNumber = all isDigit
 
yesNo :: IO Bool
yesNo = do
    ans <- getLine
    if (ans == "Y")
      then do (return True)
      else if (ans == "N")
        then do (return False)
        else do
        putStrLn "Please answer Y/N"
        yesNo
 
data Person = Person String Int String
  deriving Show        
 
survey :: IO [Person]
survey = do
    person <- quiz
    putStrLn "Enter another person? Answer Y/N"
    continue <- yesNo
    if continue
      then do
      people <- survey
      return (person:people)
      else do
      putStrLn "The survey is complete."
      return [person]
