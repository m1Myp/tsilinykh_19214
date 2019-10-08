import Data.List

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x] -- Не очень эффективно по памяти

mymapleft f xs = reverseList(foldl (\ys x -> f x : ys) [] xs)

mymapright f xs = foldr (\x ys -> f x : ys) [] xs
