import Data.List

reverseList :: [Int] -> [Int]
reverseList [] = [] 
reverseList (x : xs) = reverseList xs ++ [x] -- Не очень эффективно по памяти(Простите)

mymapleft :: (Int -> Int) -> [Int] -> [Int]
mymapleft f xs = reverseList(foldl (\ys x -> f x : ys) [] xs)

mymapright :: (Int -> Int) -> [Int] -> [Int]
mymapright f xs = foldr (\x ys -> f x : ys) [] xs
