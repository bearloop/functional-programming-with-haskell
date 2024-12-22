import Data.List (sort)

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
  -- deriving Show

instance Show Term where
 show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Variable "a")) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

------------------------- Assignment 1
numeralRecursion :: Int -> Term
numeralRecursion r | r <= 0 = Variable "x"
                       | otherwise= (Apply (Variable "f") (numeralRecursion (r-1)))

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeralRecursion i))

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------- Assignment 2

-- 2a. Create the infinite list "variables" | Flatten the list of lists created by the list comprehension(s) using "concat"
alphabet :: [Var]
alphabet = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

variables :: [Var]
variables = alphabet ++ concat [ [letter ++ show num | letter <- alphabet] | num <- [1..]]

-- 2b. Create the function "filterVariables" to remove the elements of the 2nd list from the 1st
elemNotInList :: Var -> [Var] -> Bool
elemNotInList x ls = (x `elem` ls) == False

filterVariables :: [Var] -> [Var] -> [Var]
filterVariables l1 l2 = filter (\x -> elemNotInList x l2) l1

-- 2c. Create the function "fresh" to return the first available element of "variables" not in a given list
fresh :: [Var] -> Var
fresh ls = (filterVariables variables ls) !! 0

-- 2d.
-- Create "removeDuplicates" function to remove any duplicated elements
removeDuplicates :: [Var] -> [Var]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` ( removeDuplicates xs ) = removeDuplicates xs
    | otherwise                = x : removeDuplicates xs

-- Create "collect" function to record the variable names used for the Variable and Lambda terms
collect :: Term -> [Var]
collect = f 0
    where
      f i (Variable x) = [x]
      f i (Lambda x m) = [x] ++ f 0 m 
      f i (Apply  n m) = f 1 n ++ f 2 m

-- Position mapping of each Var within the infite list 
variablesIndexed :: [(Var,Int)]
variablesIndexed = [(variables !! ind, ind) | ind <- [0..]]


-- Create "getSortedIndices" function to identify the position of each element in the variables list
getSortedIndices :: [Var] -> [Int]
getSortedIndices ls = sort [ getIndexOf variablesIndexed el | el <- ls]
        where  getIndexOf ((var,ind):xs) elem 
                        | var == elem = ind 
                        | otherwise = getIndexOf xs elem 

-- uncomment to review "getIndexOf" function e.g run `gio variablesIndexed "z5000"` on the terminal\
-- caution: it takes a bit of time to run for lists longer than ~80,000 elements
-- gio :: [(Var,Int)] -> Var -> Int
-- gio ((var,ind):xs) elem 
--               | var == elem = ind 
--               | otherwise = gio xs elem 

-- Create the function "used" to collect all the variable names used in a Term, return them in an ordered list
used :: Term -> [Var]
used term = [variables !! i | i <- getSortedIndices (removeDuplicates (collect term))]




------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
-- Handle Variable term
rename x y (Variable z) | z == x = Variable y
                        | otherwise = Variable z

-- Handle Lambda term
rename x y (Lambda z n) | z == x = Lambda z n
                        | otherwise = Lambda z (rename x y n)

-- Handle Apply term
rename x y (Apply  n m) = Apply (rename x y n) (rename x y m)


substitute :: Var -> Term -> Term -> Term
-- first arg: var to substitute in the term
-- second arg: term to substitute with
-- third arg: term where substitution takes place

substitute x n (Variable m) | m == x = n
                            | otherwise = Variable m

substitute x n (Lambda y m) | y == x = Lambda y m
                             -- λz.(M[z/y][N/x])
                            | otherwise = Lambda z ( substitute x n (rename y z m))
                              -- z is fresh, not used in M or N and z != x
                              where z = fresh (merge (merge (used m) (used n)) ([x]))

substitute x n (Apply m1 m2) = Apply (substitute x n m1) (substitute x n m2) -- (M1[N/x])(M2[N/x])

------------------------- Assignment 4

-- Hint: you will need four pattern-matching cases: one to see if the term is a redex, and if not, the three usual cases 
  -- for Term to look further down in the term. In the first case, don’t forget to look for further redexes as well. 
  -- Since beta returns a list, you will have to take care with your recursive calls.

-- 4a. Beta reduction
beta :: Term -> [Term]
beta (Variable x) = []
beta (Lambda x m) =  [ Lambda x m0 | m0 <- beta m]
beta (Apply m1 m2) = rdx ++ 
                     [Apply temp_m1 m2 | temp_m1 <- beta m1] ++
                     [Apply m1 temp_m2 | temp_m2 <- beta m2]
          where 
            rdx = case m1 of
                        Lambda x m -> [substitute x m2 m]
                        _          -> []

-- 4b.
-- "normalize" returns a list of beta-step applications
normalize :: Term -> [Term]
normalize t  | length (beta t) > 0 = [t] ++ normalize (beta t !! 0)
             | otherwise = [t]

-- "normal" returns the normal form of a term
normal :: Term -> Term
normal t = last (normalize t)


------------------------- 
-- 4c. (i)  "beta", "normalize" and "normal" implement the normal-order/standard reduction of the input term (i.e. reduce the leftmost outermost redex first)

-- 4c. (ii) adjusting the functions to implement applicative-order reduction (i.e. reduce the leftmost innermost redex first)
a_beta :: Term -> [Term]
a_beta (Variable x) = []
a_beta (Lambda x m) =  [ Lambda x m0 | m0 <- a_beta m]
a_beta (Apply m1 m2) = [Apply temp_m1 m2 | temp_m1 <- a_beta m1] ++
                       [Apply m1 temp_m2 | temp_m2 <- a_beta m2] ++
                       rdx
          where 
            rdx = case m1 of
                        Lambda x m -> [substitute x m2 m]
                        _          -> []

a_normalize :: Term -> [Term]
a_normalize t | length (a_beta t) > 0 = [t] ++ a_normalize (a_beta t !! 0)
              | otherwise = [t]

a_normal :: Term -> Term
a_normal t = last (a_normalize t)

-------------------------

-- 4c. (iii) A term which reduces to normal form in more steps with normal-order than with applicative-order reduction
-- (𝜆𝑥.𝑥𝑥)((𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦))
example1 :: Term
example1 = Apply (Lambda "x" (Apply (Variable "x") (Variable "x"))) (Apply (Lambda "z" (Apply (Variable "z") (Variable "z"))) ((Lambda "y" (Variable "y"))))

-- 4d. (iv) A term which reduces to normal form in fewer steps with normal-order than with applicative-order reduction
-- (λx.λz.λy.y)(λx.x)(λr.r)
example2 :: Term
example2 = Apply (Lambda "x" (Lambda "z" (Lambda "y" (Variable "y")))) (Apply (Lambda "x" (Variable "x")) (Lambda "r" (Variable "r")))


-- other examples
-- use the following examples to test the behavior of "normal" and "normalize"

-- (λf.λx.f(fx))(λf.λx.f(fx))
-- num2Application :: Term
-- num2Application = Apply (numeral 2) (numeral 2)

-- (λf.λx.f f x)(λa.((λx.x) ((λy.y) ((λz.z)a) ) ))
-- otherExample :: Term
-- otherExample = Apply (Lambda "f" (Lambda "x" (Apply (Apply (Variable "f") (Variable "f")) (Variable "x")))) (Lambda "a" (Apply (Lambda "x" (Variable "x")) (Apply (Lambda "y" (Variable "y")) (Apply (Lambda "z" (Variable "z")) (Variable "a")))))

-- term without a normal form (𝜆𝑥.𝑥𝑥)(𝜆𝑥.𝑥𝑥)
-- infiniteTerm :: Term
-- infiniteTerm = Apply (Lambda "x" (Apply (Variable "x") (Variable "x"))) (Lambda "x" (Apply (Variable "x") (Variable "x")))

-- (λx.xx)((λy.y)z)
-- anotherExample2 :: Term
-- anotherExample2 = Apply (Lambda "x" (Apply (Variable "x") (Variable "x"))) (Apply (Lambda "y" (Variable "y")) (Variable "z"))

-- (λx.λy.y)((λx.xx)(λx.xx))(λx.xx)
-- anotherExample3 :: Term
-- anotherExample3 = Apply ( Apply (Lambda "x" (Lambda "y" (Variable "y"))) (Apply (Lambda "x" (Apply (Variable "x") (Variable "x"))) (Lambda "x" (Apply (Variable "x") (Variable "x")))) ) (Lambda "x" (Apply (Variable "x") (Variable "x")))

test1::Term
test1 = Lambda "y" (Apply (Variable "x") (Lambda "x" (Apply (Variable "x") (Variable "y"))))

test1Replace::Term
test1Replace = (Lambda "y" (Apply (Variable "y") (Variable "x")))


test2::Term
test2 = Apply (Variable "y") (Lambda "z" (Apply (Variable "x") (Variable "z")))


test2Replace::Term
test2Replace = (Lambda "y" (Lambda "z" (Apply (Variable "y") (Variable "x"))))

-- -- x(λx.x)((λy.(λx.x)y)x)
-- test3::Term
-- test3 =  Apply (Apply (Variable "x")  (Lambda "x" (Variable "x")))
--                  (Apply 
--                 (Lambda "y" (Apply (Lambda "x" (Variable "x")) (Variable "y"))) 
--                 (Variable "x") )


-- (λz.λu.zu)u(zλw.ww)
p3a :: Term
p3a = Apply (Apply (Lambda "z" (Lambda "u" (Apply (Variable "z") (Variable "u")))) (Variable "u")) 
      (Apply (Variable "z") (Lambda "w" (Apply (Variable "w") (Variable "w"))))
      
p3a2 :: Term
p3a2 = (Apply (Lambda "z" (Lambda "u" (Apply (Variable "z") (Variable "u")))) (Variable "u")) 

      
-- (λu.((λw.(zu))a))t
p3b :: Term
p3b = Apply (Lambda "u" (Apply (Lambda "w" (Apply (Variable "z") (Variable "u"))) (Variable "a"))) (Variable "t")

p3c :: Term
p3c = Apply (Lambda "v" (Apply (Lambda "w" (Apply (Variable "z") (Variable "u"))) (Variable "a"))) (Variable "t")


p4 :: Term
p4 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "y") (Variable "x"))))
      (Lambda "x" (Apply (Variable "x") (Variable "y")))


p5 :: Term
p5 = Apply (Lambda "x" (Lambda "y" (Variable "x"))) (Lambda "x" (Lambda "y" (Variable "y")))