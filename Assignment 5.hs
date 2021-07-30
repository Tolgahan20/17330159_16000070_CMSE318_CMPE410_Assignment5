--- 1. Numbers divisible by 2---
div_by_2 :: [Int] -> [Int]
div_by_2 [] = []
div_by_2 (a:as) = if a `mod` 2 == 0 then a:div_by_2 as else div_by_2 as

--- 2.) Numbers divisible by 3---
div_by_3 :: [Int] -> [Int] 
div_by_3 (a) = [b | b <- a, b `mod` 3 == 0]

--- 3. Multiply list elements by 3---
triple :: [Int] -> [Int]
triple (a) = map (3*) a

--- 4. Counting the number of element in a list--- 
count elem [] = 0
count elem (a:as) = if a == elem then 1 + (count elem as) else (count elem as)

--- 5. Nth element of the list recursive---
nth_element :: Int -> [Int] -> Int
nth_element b (a:as) = if (1 /= b) then nth_element (b-1) as else a

--- 6. Mth element of a list using head/drop---
mth_element :: Int -> [Int] -> Int
mth_element a l = head (drop (a - 1) l)

--- 7. Sorting three numbers from smallest to greatest---
sort :: Int -> Int -> Int -> [Int]
sort a b c
    | a >= b && b >= c = [c, b, a]
    | a >= c && c >= b = [b, c, a]
    | b >= a && a >= c = [c, a, b]
    | b >= c && c >= a = [a, c, b]
    | c >= a && a >= b = [b, a, c]
    | c >= b && b >= a = [a, b, c]

--- 8. Splitting a list from a to b---
from_to :: Int -> Int -> [Int] -> [Int]
from_to a b l = drop (a-1) (take b l)
 
--- 9. Adding two lists---
add_list (a:as) (b:bs) = if (length as > 0) && (length bs > 0)
		           then (a+b) : (add_list as bs)
                         else if (length as > 0) && (length bs == 0)
		           then (a+b) : (add_list as [0])
                         else if (length as == 0) && (length bs > 0)
		           then (a+b) : (add_list [0] bs) 
                         else (a+b) : []

--- 10. Letter grading based on score---
classify :: Int -> Char
classify a | a >= 90 = 'A' | (a >= 70) && (a <=89) = 'B'
	| (a >= 50) && (a <=69) = 'C'
    | otherwise = 'D'