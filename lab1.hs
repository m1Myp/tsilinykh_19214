import Data.Char
import Data.Map

myConvertBack :: Int -> Char
myConvertBack a |  a >=  0 &&  a <=  9 = chr (a + ord '0')
                |  a >= 10 &&  a <= 35 = chr (a + ord 'a' - 10) 
                |  a >= 36 &&  a <= 61 = chr(a + ord 'A' - 36)
                | otherwise = error "Wrong Number"

myConvert :: Char -> Int
myConvert a | ord a >= ord '0' && ord a <= ord '9' = ord a - ord '0'
            | ord a >= ord 'a' && ord a <= ord 'z' = ord a - ord 'a' + 10
            | ord a >= ord 'A' && ord a <= ord 'Z' = ord a - ord 'A' + 36
            | otherwise = error "Wrong Number"
			
myToDec :: Int -> String -> Int
myToDec 1 (x:xs) = length xs
myToDec base [] =  0
--fold со всеми эрорами
myToDec base (x:xs) = if myConvert x > base then error "Wrong digit" else Prelude.foldl(\ys x -> if myConvert x > base then error "Wrong digit" else (myConvert x + ys)*base) (myConvert x) xs
			
myToDecimal :: Int -> String -> String
myToDecimal base [] = error "Where is number?"
myToDecimal base xs = if base > 62 then error "Wrong base" else show $ myToDec base xs

myFromDec :: Int -> Int -> String
myFromDec 1 xs = replicate (xs + 1) '1'
--accum
myFromDec base xs = myFromDecAcc (myConvertBack(xs `mod` base):[]) base (xs `div` base)
    where
        myFromDecAcc acc base 0 = acc
        myFromDecAcc acc base xs = myFromDecAcc (myConvertBack (xs `mod` base) : acc) base (xs `div` base)

myFromDecimal :: Int -> String -> String
myFromDecimal base [] = error "Where is number?"
myFromDecimal base xs = if base > 62 then error "Wrong base" else myFromDec base (read xs :: Int)

myConvertBtoB :: Int -> Int -> String -> String
myConvertBtoB fromBase toBase [] = error "Where is number?"
myConvertBtoB fromBase toBase xs = myFromDecimal toBase (myToDecimal fromBase xs)
