import Data.List
import System.IO


interleave :: [Int] -> [Int] -> [Int]
interleave list1     []     = list1
interleave []     list2     = list2
--Take the first elements from both the lists corrospondingly and call teh function recursively
interleave (a:list1) (b:list2) = a : b : interleave list1 list2


heads :: [[Int]] -> [Int]
heads [] = [] 
--Call 'head' function to get the first element of all lists and ignore null lists
heads sublist = [head a | a <- sublist, not(null a)]


tailElements  :: [[Int]] -> [[Int]]
tailElements  [] = [] 
--Call 'tail' function to get the tail element list of all lists and ignore null lists
tailElements  sublist = [tail a | a <- sublist, not(null a)]


interleaveN :: [[a]] -> [a]
interleaveN [] = []
{-Filter out the null lists first , retrieve first elements of lists using head 
function and tail to get the list excluding first elements. Do this recursively 
until we get the interleaved list.-}
interleaveN xs = interl (filter (\x -> not (null x)) xs)
   where interl xs = map head xs ++ interleaveN (map tail xs)
	

doubleEach :: [Int] -> [Int]
doubleEach [] = []
--Map returns a list constructed by applying the function (*2) on each element of input list. 
doubleEach a = map (*2) a


doubleEach' :: (Int -> Int) -> [Int] -> [Int]
{-Here \x is the lambda function and in our case it is (*2) in order to multiply each element 
of the input list by 2. Since the foldr is being used, the lambda function folds the valur from
rightmost elements of the list and performs the function.-}   
doubleEach' p xs = foldr (\x b -> p x : b) [] xs  


doubleEven :: [Int]->[Int]
{-Use “filter” function to retrieve only even numbers from the input list and double those 
elements using map.-} 
doubleEven a = map (*2) (filter even a)


takeUntil :: (Int -> Bool) -> [Int] -> [Int]
takeUntil _ [] = []
{-The function is called recursively to get the elements till the first list element which 
satisfies the predicate. 
“Not (p x)” checks that elements which do not satisfy the predicate are retrieved.-} 
takeUntil p (x:a) = if not (p x) then x : takeUntil p a else []


infisquares :: [Int]
{-This function does not take any input. The input list starts from 0 to infinite and (^2)
is applied on all elements.-}
infisquares = [x^2 | x <- [0..]]

{-Here “infisquares” function helps to get the list of all square numbers and “takeUntil” function
helps to check that only squares below 100 are taken.-}
sumofhundered = sum(takeUntil (\x -> x>99) infisquares)



	





