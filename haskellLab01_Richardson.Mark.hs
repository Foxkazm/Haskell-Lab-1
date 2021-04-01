-- 1. factorial function

factorial n = product [1..n]

-- 2. write the middle function that returns the middle of a list

middle xs= reverse (drop 1 (reverse (drop 1 xs)))

-- 3. a function to find the second to last element in a list

secondToLast xs= head (drop 1 (reverse xs)) 

--4.a function that returns half of a list

halfLength xs= div (length xs) 2
halfOf xs = take (halfLength xs) xs

--5 a function that takes a list and turns it into a palindrome

palindromify xs = xs ++ tail (reverse xs)

--6 a function that takes three lists and appends them all in reverse order

app3 xs ys zs = reverse zs ++ reverse ys ++ reverse xs

--7 a function that removes elements from a list depending on the first element

remByFirst xs = drop (head xs) xs

--8 a function that finds the lowest value and makes a list of the lowest value the highest value number of times

lowList xs = replicate (maximum xs) (minimum xs)

--9 takes an int and a list and repeats the list the int number of

xTimesL x xs = x*(length xs)

reList x xs= take (xTimesL x xs) (cycle xs)

--10 a function that counts by the first number up to the amount of times given

countBy x y= [x,x*2..x*y]

--11 a function the repeats the highest and lowest values of a list

minMaxDiff xs= maximum xs-minimum xs

minMaxList xs = reList (minMaxDiff xs) [minimum xs, maximum xs]

--12 make a function to find the median of a list

findMedians xs =if mod (length xs) 2 >0 
                            then
                            take 1 (drop (halfLength xs) xs)
                            else
                            take 2 (drop (halfLength xs-1) xs)

findMedDiff xs= (head (reverse (findMedians xs)) - head (findMedians xs))/2

calcMedian xs=head (findMedians xs) + (findMedDiff xs) 
