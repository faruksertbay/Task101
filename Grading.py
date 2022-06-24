mport Data.List

--returns the sum of the elements in the same position   * 3
add_list (x:xs) (y:ys) = if (length xs > 0) && (length ys > 0)
then (x+y) : (add_list xs ys)
else if (length xs > 0) && (length ys == 0)
then (x+y) : (add_list xs [0])
else if (length xs == 0) && (length ys > 0)
then (x+y) : (add_list [0] ys)
else (x+y) : []


--returns a letter grade based upon a numeric grade        *8
classify_g x
 | x > 90 = "you got A"
 | x > 70 = "you got B"
 | x > 50 = "you got C"
 | otherwise = "you got D"
 
 
 --returns a letter for grade based on the degree of the grade used by if statment   *9
classify_i x = 
    if x > 90 then "You got a A"
    else if 80 < x && x < 90 then "you got a B"
    else if 70 < x && x < 80 then "You got a C"
    else if 60 < x && x < 70 then "you got a D"
    else "You got a F"
    

-- returns the pair of two different lists         *7
pair_lists (x:xs) (y:ys) | length xs >0 && length ys >0 = (y,x) : (pair_lists xs ys)
 | otherwise = (y,x) : []


--return the first 5 odd numbers 
firstodd = [ x | x <- [1..10], x `mod` 2 == 1]


-- return even numbers from a given list  * 2
getEvenNumbers :: [Integer] -> [Integer]
getEvenNumbers [] = []
getEvenNumbers (x:xs)
    | even x = x : getEvenNumbers xs
    | otherwise = getEvenNumbers xs



-- return odd numbers from a givin list * 4

getOddNumbers :: [Integer] -> [Integer]
getOddNumbers [] = []
getOddNumbers (x:xs)
    | odd x = x : getOddNumbers xs
    | otherwise = getOddNumbers xs
    
    
-- return the order of the list in ascending order    1
order [] = []
order (pivot:xs) = order [x | x<-xs, x<pivot] ++ [pivot] ++ order [x | x<-xs, x>=pivot]

--return the occurance of an element    6
howmany :: Eq a => [a] -> Int
howmany [] = 0
howmany xs = (maximum . map length . group) xs



--1 main = print $ (order [5,9,6,2,23,36,1,27,15,11,3])  -- 1 order of a list into acending order
--2 main = print $ (getEvenNumbers [1,2,3,4,5,6,7,8,9,10,11,12])  --2 find even 
--3 main = print $ (add_list [3,2][4,8])    -- 3 add list
--4 main = print $ (getOddNumbers [1,2,3,4,5,6,7,8,9,10,11,12])    --4 find odd
--6 main = print $ (howmany [1,1,1,2,3,4,5])
--7 main = print $ (pair_lists [3,2,1] [4,8,9])     -- 7 pair list
--8 main = print $ (classify_g 95)  --8 class by gaurd
--9 main = print $ (classify_i 85) -- 9 class by if
--10 main = print $ firstodd                 --10 filter odd