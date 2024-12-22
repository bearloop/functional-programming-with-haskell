data Duck = Duck String Int Float
            | Duckling String Int Float
  deriving Show

donald :: Duck
donald = Duck "Donald" 6 0.12

daisy :: Duck
daisy = Duck "Daisy" 5 0.16

huey :: Duck
huey = Duckling "Huey" 2 0.68

dewey :: Duck
dewey = Duckling "Dewey" 2 0.21

duckFamily :: [Duck]
duckFamily = [donald,daisy,huey,dewey]

birthday :: Duck -> Duck
birthday (Duck s a f) = Duck s (a+1) f
birthday (Duckling s a f) = Duckling s (a+1) f


tall :: Duck -> Bool
tall (Duck s a f) = if f>0.6 then True else False
tall (Duckling s a f) = if f>0.25 then True else False



-- correct
 
birthday2 :: Duck -> Duck
birthday2 (Duck n a h) = Duck n (a+1) h
birthday2 (Duckling n a h) | a == 2  = Duck n 3 h
                          | otherwise = Duckling n (a+1) h
 
tall2 :: Duck -> Bool
tall2 (Duck n a h) = h >= 0.6
tall2 (Duckling n a h) = h >= 0.25