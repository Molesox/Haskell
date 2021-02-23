-- Daniel Sanz, 2021
-- My solutioons to the 99 exersises of the Haskell wiki
-- https://wiki.haskell.org/99_questions/1_to_10


-- 1) Find the last element of a list

myLast :: [a] -> a
myLast [] = error "list cannot be empty"
myLast (h : t)
  | null t = h
  | otherwise = myLast t

myLast' :: [a] -> a
myLast' = head . reverse -- It's the same as head(reverse (x))

-- 2) Find the last but one element of a list.

myBut :: [a] -> a
myBut [] = error "list cannot be empty"
myBut [h] = h
myBut [h, h2] = h
myBut (h : t)
  | length t == 2 = head t
  | otherwise = myBut t

myBut' :: [a] -> a
myBut' [] = error "list cannot be empty"
myBut' [h] = h
myBut' [h, h2] = h
myBut' l = reverse l !! 1

myBut'' :: [a] -> a
myBut'' l = head $ tail $ reverse l -- [1,2,3] -> [3,2,1] -> [2,1] -> 2

myBut''' :: [a] -> a
myBut''' = head . tail . reverse -- [1,2,3] -> [3,2,1] -> [2,1] -> 2

-- 3) Find the K'th element of a list. The first element in the list is number 1.

myElementAt :: [p] -> Int -> p
myElementAt l n =
  if length l < n
    then error "index out of bounds"
    else fst . last $ zip l [1 .. n]

myElementAt' [] _ = error "index out of bounds"
myElementAt' list n = if n == 1 then head list else myElementAt' (tail list) $ n - 1

-- 4) Find the number of elements of a list.

count :: [a] -> Int 
count  = foldl (\acc _ -> acc + 1) 0