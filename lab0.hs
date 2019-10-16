myGet :: [a] -> Integer -> a
myGet [] n = error "Empty list"
myGet (x:xs) 0 = x
myGet (x:xs) n = myGet xs (n-1)

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:xs) = x

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (_:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit [x] = []
myInit (x:xs) = x : myInit xs 

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl(\ys x -> x:ys) []xs

--length not optimal
myLength :: [a] -> Integer
myLength xs = myLen xs 0 where
                myLen []     ans = ans
                myLen (x:xs) ans = myLen xs ans+1

myAppend :: [a] -> a -> [a]
myAppend [] y = [y]
myAppend (x:xs) y = x : myAppend xs y

myConcat :: [a] -> [a] -> [a]
myConcat [] ys = ys
myConcat (x:xs) ys = x : myConcat xs ys

myDrop :: Integer -> [a] -> [a]
myDrop n [] = error "Out of index"
myDrop 0 xs = xs
myDrop n (x:xs) = myDrop (n-1) xs

myTake :: Integer -> [a] -> [a]
myTake n [] = error "Out of index"
myTake 0 xs = []
myTake n (x:xs) = x : myTake (n-1) xs

mySplitAt :: Integer -> [a] -> ([a], [a])
mySplitAt n [] = error "Out of index"
mySplitAt n xs = (myTake n xs, myDrop n xs)

myNull :: [a] -> Bool
myNull [] = False
myNull _ = True

myElem :: Eq a => [a] -> a -> Bool
myElem [] y = False
myElem (x:xs) y = if x == y then True else myElem xs y

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter test [] = []
myFilter test (x:xs) = if test x  then x : myFilter test xs else myFilter test xs

myMap :: (a -> a) -> [a] -> [a]
myMap f xs = foldr (\x ys -> f x : ys) [] xs

myZip :: [a] -> [a] -> [(a,a)]
myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
