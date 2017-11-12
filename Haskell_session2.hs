--  %%%%%%%%%%%%%%%%% Session 2 %%%%%%%%%%%%%%%%% 


-- -------------------------------------------------------- -- 
-- ---------------------- P93632 -------------------------- --
-- -------------------------------------------------------- --

eql :: [Int] -> [Int] -> Bool 
eql a b = all (==0) (zipWith (-) a b)  && length a == length b

prod :: [Int] -> Int 
prod a = foldl (*) 1 a

prodOfEvens :: [Int] -> Int
prodOfEvens a = prod $ filter even a

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct a b = sum (zipWith (*) a b)



-- -------------------------------------------------------- -- 
-- ---------------------- P31745 -------------------------- --
-- -------------------------------------------------------- --

flatten :: [[Int]] -> [Int]
flatten a = foldl (++) [] a
  
myLength :: String -> Int
myLength a = sum $ map (const 1) a

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse a = foldl (flip (:)) [] a

countIn :: [[Int]] -> Int -> [Int]
countIn [] x = []
--countIn (a:as) x = comptar a x: countIn as x
  --where 
    --comptar :: [Int] -> Int -> Int
    --comptar [] x = 0
    --comptar a x = length $ filter (==x) a
  
firstWord :: String -> String
firstWord a = takeWhile (/=' ') $ dropWhile (== ' ') a


-- -------------------------------------------------------- -- 
-- ---------------------- P93588 -------------------------- --
-- -------------------------------------------------------- --


myMap :: (a -> b) -> [a] -> [b] 
myMap f a = [f x | x <- a]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f a = [x | x <- a, f x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f x y = [ f a b | (a,b) <- zip x y ]


thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify a b = [ (x, y) | x <- a, y <- b, mod x y == 0]

factors :: Int -> [Int]
factors a = [ x | x <- filter (divisible) (takeWhile(<= a) (iterate (+1) 1))]
  where
    divisible :: Int -> Bool
    divisible y 
      | mod a y == 0	= True
      | otherwise 	= False



-- -------------------------------------------------------- -- 
-- ---------------------- P90677 -------------------------- --
-- -------------------------------------------------------- --


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a b = myFoldl' f a (reverse b)
  where myFoldl' :: (a -> b -> a) -> a -> [b] -> a
	myFoldl' _ a [] = a
	myFoldl' f a (b:bs) = f (myFoldl' f a bs) b

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ a [] = a
myFoldr f a (b:bs) = f b (myFoldr f a bs)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil f g a 
    |f a = a
    |otherwise 	= myUntil f g (g a)

myMap :: (a -> b) -> [a] -> [b]
myMap f a = [f x | x <- a]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f a = [x | x <- a, f x]

{-
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = False
myAll f a
    |length a == length (takeWhile f a)	= True
    |otherwise	= False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f a
    |length (filter f a) > 0 = True
    |otherwise		= False -}

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs) = (a,b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = [ f a b | (a,b) <- zip x y ]


-- -------------------------------------------------------- -- 
-- ---------------------- P98957 -------------------------- --
-- -------------------------------------------------------- --



ones :: [Integer]
ones = iterate (*1) 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = iterate (segEnter) 0

segEnter :: Integer -> Integer
segEnter 0 = 1
segEnter a
      | a > 0	= - a
      | a < 0 	= - (a - 1)
      
fibs :: [Integer]
fibs = fibo 0 1
  where fibo :: Integer -> Integer -> [Integer]
	fibo a b = a : fibo b (a+b)
	
triangulars :: [Integer]
triangulars = triang 0
  where triang :: Integer -> [Integer]
	triang a = quot ((a*a) + a) 2 : triang (a+1)
	
factorials ::  [Integer]
factorials = 1 : factorial 1


fact :: Integer -> Integer
fact 1 = 1
fact a = (fact (a-1))*a

factorial :: Integer -> [Integer]
factorial a = fact a : factorial (a+1)


primes :: [Integer]
primes = nombPrimers 1

nombPrimers :: Integer -> [Integer]
nombPrimers a 
  | isPrime a 	= a : nombPrimers (a+1)
  | otherwise	= nombPrimers (a+1)
    
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (hasDivisors (n-1))
      where
      hasDivisors 1 = False
      hasDivisors x = mod n x == 0 || hasDivisors (x-1)
      
      
hammings :: [Integer]
hammings = 1 : (f3  ( map (*2) hammings) (map (*3) hammings) (map (*5) hammings))

f3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
f3 xs gs zs = (f2 (f2 xs gs) zs)

f2 :: [Integer] -> [Integer] -> [Integer]
f2 xs gs = merge xs gs


merge :: [Integer] -> [Integer] -> [Integer] 
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
      | a < b	= [a]++ (merge as ([b]++bs))
      | b < a 	= [b]++(merge ([a]++as) bs)
      |otherwise = merge ([a]++as) bs



lookNsay :: [Integer]
lookNsay = iterate comptar 1

comptar :: Integer -> Integer
comptar a = read $ next $ show a

next :: [Char] -> [Char]
next [] = []
next cs = (show n) ++ [pr] ++ next cua
  where 
    pr = head cs
    n = length $ takeWhile (==pr) cs
    cua = dropWhile (==pr) cs


--tartaglia :: [Integer]
