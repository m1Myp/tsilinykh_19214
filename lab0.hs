myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:xs) = x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast xs = myHead(myReverse xs)

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit xs = myReverse (myTail(myReverse xs))

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myAppend :: [a] -> a -> [a]
myAppend xs x = myReverse (x : myReverse xs)

myNull :: Eq a => [a] -> Bool
myNull xs = if xs == [] then True else False

myConcat :: [a] -> [a] -> [a]
myConcat xs ys = xs ++ ys-- foldr(\x xs -> (x:xs)) []xs ++ ys 

myTake :: Integer -> [a] -> [a]
myTake n [] = error "Empty list"
myTake 0 xs = []
myTake n (x:xs) = x : myTake (n-1) xs

myDrop :: Integer -> [a] -> [a]
myDrop n [] = error "Empty list"
myDrop 0 xs = xs
myDrop n (x:xs) = myDrop (n-1) xs

myGet :: [a] -> Integer -> a
myGet [] n = error "Empty list"
myGet (x:xs) 0 = x
myGet (x:xs) n = myGet xs (n-1)

mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt n [] = error "Empty list"
mySplitAt n xs = (myTake n xs, myDrop n xs)

myElem :: Eq a => [a] -> a -> Bool
myElem [] y = False
myElem (x:xs) y = if x == y then True else myElem xs y

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter test [] = []
myFilter test (x:xs) = if test x == True then x : myFilter test xs else myFilter test xs

myMap :: (Integer -> Integer) -> [Integer] -> [Integer]
myMap f xs = foldr (\x ys -> f x : ys) [] xs

myZip :: [a] -> [a] -> [(a,a)]
myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys