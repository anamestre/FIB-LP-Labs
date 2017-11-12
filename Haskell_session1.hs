--  %%%%%%%%%%%%%%%%% Session 1 %%%%%%%%%%%%%%%%% 


-- Functions
absValue :: Int -> Int
absValue x 
      | x < 0	= -x
      | otherwise	= x
 
power :: Int -> Int -> Int       
power n 0 = 1
power n i
      | even i = x*x
      | otherwise = x*x*n
      where 
	  x = power n imitjos
	  imitjos = div i 2
	  
isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (hasDivisors (n-1))
    where
	hasDivisors 1 = False
	hasDivisors x = mod n x == 0 || hasDivisors (x-1)
  
  
quickFib :: Int -> Int
quickFib n = fst (quickFib' n)
    where
      quickFib' :: Int -> (Int, Int)
      quickFib' 0 = (0, 0)
      quickFib' 1 = (1, 0)
      quickFib' n = (fn1+fn2, fn1)
	  where (fn1, fn2) = quickFib' (n-1)


slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib (n-2)



-- -------------------------------------------------------- -- 
-- ---------------------- P25054 -------------------------- --
-- -------------------------------------------------------- -- 
myLength :: [Int] -> Int
myLength [] = 0
myLength (_:cua) = 1 + myLength cua

myMaximum :: [Int] -> Int
myMaximum [a] = a
myMaximum (x:xs) = max x (myMaximum xs)

average :: [Int] -> Float
average xs = suma xs / fromIntegral (myLength xs)

suma :: [Int] -> Float
suma [] = 0.0
suma (x:xs) = fromIntegral x + suma xs

-- No treu la soluciÃ¨Â´Â¸ correctament.
buildPalindrome :: [Int] -> [Int]
buildPalindrome a = (invertir a) ++ a

invertir :: [Int] -> [Int]
invertir [] = []
invertir (x:xs) = (invertir xs) ++ [x]

remove :: [Int] -> [Int] -> [Int]
remove [] b = []
remove (a:as) b
      | correcte a b = a:remove as b
      | otherwise = remove as b

correcte :: Int -> [Int] -> Bool
correcte x [] = True
correcte x (y:ys)
    | x == y    = False
    | otherwise = correcte x ys

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (a:as) = a ++ flatten as

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([], [])
oddsNevens a = (oddsNevensRec a [] [])

oddsNevensRec :: [Int] -> [Int] -> [Int]-> ([Int],[Int])
oddsNevensRec [] b c = (b,c)
oddsNevensRec (a:as) b c
          | even a	= (oddsNevensRec as b (c++[a]))
          | otherwise 	= (oddsNevensRec as (b++[a]) c)



primeDivisors :: Int -> [Int]
primeDivisors a = primeDivisorsRec 2
    where
        primeDivisorsRec :: Int -> [Int]
        primeDivisorsRec n
	  | n == a+1	= []
	  | mod a n == 0 && isPrime n	= n : primeDivisorsRec (n+1)
	  | otherwise	= primeDivisorsRec (n+1)
            where
             isPrime :: Int -> Bool
             isPrime 0 = False
             isPrime 1 = False
             isPrime n = not (hasDivisors (n-1))
                where
                    hasDivisors 1 = False
                    hasDivisors x = mod n x == 0 || hasDivisors (x-1)
