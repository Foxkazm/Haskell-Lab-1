
factorial n = product [1..n]

middle xs= reverse (drop 1 (reverse (drop 1 xs)))

secondToLast xs= head (drop 1 (reverse xs)) 

halfLength xs= div (length xs) 2

