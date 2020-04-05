import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List

parser::String->Double->Double
parser str x | (str == "x") = x
			 | (take 4 str == "cos$") = cos$ parser (drop 4 str) x 
			 | (take 4 str == "sin$") = sin$ parser (drop 4 str) x 
			 | (take 4 str == "exp$") = exp$ parser (drop 4 str) x 
			 | (take 4 str == "abs$") = abs$ parser (drop 4 str) x
			 | (take 5 str == "sqrt$") = sqrt$ parser (drop 5 str) x
			 | (take 4 str == "log$") = log$ parser (drop 4 str) x
			 | (take 5 str == "acos$") = acos$ parser (drop 5 str) x
			 | (take 5 str == "asin$") = asin$ parser (drop 5 str) x
			 | otherwise = read str::Double
--			 | (take 3 str == "tg$") = cos (parser (drop 3 str) x) / sin (parser (drop 3 str) x)

arithmetic::[Double]->String->[Double]->[Double]
arithmetic  [] _ [] = []
arithmetic arrx str arry | (str == "+") = ((head arrx)+(head arry)) : (arithmetic (drop 1 arrx) str (drop 1 arry))
						 | (str == "-") = ((head arrx)-(head arry)) : (arithmetic (drop 1 arrx) str (drop 1 arry))
					   	 | (str == "/") = ((head arrx)/(head arry)) : (arithmetic (drop 1 arrx) str (drop 1 arry))
						 | (str == "*") = ((head arrx)*(head arry)) : (arithmetic (drop 1 arrx) str (drop 1 arry))
							 

firstNum::[String]->[Double]->[Double]
firstNum str range = [parser(unwords str) x |x <- range]

repeater::[Double]->[String]->[Double]->[Double]
repeater num str range | ((length str) > 0) = repeater (arithmetic num (unwords(take 1 str)) [(parser(unwords(take 1(drop 1 str))) x)|x<-range]) (drop 2 str) range
					   | ((length str) == 0) = num
				 
vertices str range num = map p2 $ (zip range (repeater num str range))

example::[String]->[Double]->Diagram B
example str range = fromVertices (vertices (drop 1 str) range (firstNum (take 1 str) range)) # strokeLine
--        # lc red # center # pad 1.1

main::IO()
main = do 
	str <- getLine
	start <- getLine
	end <- getLine
	mainWith (example (words str) [(read start::Double), ((read start::Double) + 0.01)..(read end::Double)])

