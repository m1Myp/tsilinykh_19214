import Data.Char
import Data.Map

myConvertBack :: Int -> Char
myConvertBack a = if a >= 0 && a <= 9 then chr (a + 48) else if a>=36 && a<=61 then chr (a + 29) else if a >= 10 && a <= 35 then chr (a + 87) else error "Wrong number"

myConvert :: Char -> Int
myConvert a | ord a >= ord '0' && ord a <= ord '9' = ord a - ord '0'
            | ord a >= ord 'a' && ord a <= ord 'z' = ord a - ord 'a' + 10
            | ord a >= ord 'A' && ord a <= ord 'Z' = ord a - ord 'A' + 36
            | otherwise = error "Wrong Number"
			
myToDec :: Int -> String -> Int
myToDec 1 (x:xs) = length xs
myToDec base [] =  0
			
myToDecimal :: Int -> String -> String
myToDecimal base [] = error "Where is number?"
myToDecimal base xs = if base > 62 then error "Wrong base" else show $ myToDec base xs

myFromDec :: Int -> Int -> String
myFromDec 1 xs = replicate (xs + 1) '1'
--accum
myFromDec base 0 = ""
myFromDec base 1 = "1"
myFromDec base xs = [myConvertBack (xs `mod` base)] ++ myFromDec base (xs `div` base) --Костыль, потом подумаю как убрать, если не зачтёте

myFromDecimal :: Int -> String -> String
myFromDecimal base [] = error "Where is number?"
myFromDecimal base xs = if base > 62 then error "Wrong base" else myFromDec base (read xs :: Int)

myConvertBtoB :: Int -> Int -> String -> String
myConvertBtoB fromBase toBase [] = error "Where is number?"
myConvertBtoB fromBase toBase xs = myFromDecimal toBase (myToDecimal fromBase xs)
