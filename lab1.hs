import Data.Char
import Data.Map
 
myConvert :: Char -> Int
myConvert a = if ord a > 47 && ord a < 58 then ord a - 48 else if ord a > 64 && ord a < 91 then ord a - 29 else if ord a > 96 && ord a < 123 then ord a - 87 else error "Wrong number"

myConvert1 :: Int -> Char
myConvert1 a = if a >= 0 && a <= 9 then chr (a + 48) else if a>=36 && a<=61 then chr (a + 29) else if a >= 10 && a <= 35 then chr (a + 87) else error "Wrong number"

myPow :: Int->Int->Int
myPow a 0 = 1
myPow a b = a * myPow a (b-1)
			
myToDec :: Int -> String -> Int
myToDec 1 (x:xs) = length xs
myToDec base [] =  0
myToDec base (x:xs) = if myConvert x >= base then error "Wrong digit" else myConvert x * myPow base (length (xs)) + myToDec base xs 
 
myToDecimal :: Int -> String -> String
myToDecimal base [] = error "Where is number?"
myToDecimal base xs = if base > 62 then error "Wrong base" else show $ myToDec base xs

stringToDecNum :: String -> Int
stringToDecNum [] = 0
stringToDecNum (x:xs) = myConvert x * myPow 10 (length (xs)) + stringToDecNum xs 

myFromDec :: Int -> Int -> String
myFromDec 1 0 = "1"
myFromDec 1 xs = '1' : myFromDec 1 (xs-1)
myFromDec base 0 = ""
myFromDec base 1 = "1"
myFromDec base xs = myConvert1 (xs `mod` base) : myFromDec base (xs `div` base)

myFromDecimal :: Int -> String -> String
myFromDecimal base [] = error "Where is number?"
myFromDecimal base xs = if base > 62 then error "Wrong base" else reverse (myFromDec base (stringToDecNum xs))

myConvertBtoB :: Int -> Int -> String -> String
myConvertBtoB fromBase toBase [] = error "Where is number?"
myConvertBtoB fromBase toBase xs = myFromDecimal toBase (myToDecimal fromBase xs)
