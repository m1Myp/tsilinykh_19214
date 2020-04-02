import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List

parser::String->Double->Double
parser str x | (str == "x") = x
			 | (take 4 str == "cos$") = cos$ parser (drop 4 str) x 
			 | (take 4 str == "sin$") = sin$ parser (drop 4 str) x 
			 | (take 4 str == "exp$") = exp$ parser (drop 4 str) x 
			 | (take 4 str == "abs$") = abs$ parser (drop 4 str) x
			 | (take 4 str == "sqr$") = sqrt$ parser (drop 4 str) x
			 | (take 4 str == "log$") = log$ parser (drop 4 str) x
			 | (take 5 str == "acos$") = acos$ parser (drop 5 str) x
			 | (take 5 str == "asin$") = asin$ parser (drop 5 str) x

funcParse::String->Double->Double->Double
funcParse str x y | (str == "+") = x + y 		
				  | (str == "-") = x - y 
				  | (str == "/") = x / y 
				  | (str == "*") = x * y 

vertices str start end ychange yfunc = map p2 $ [(x,funcParse yfunc ychange (parser str x))|x <- [start, (start+0.01)..end]] where

example::String->Double->Double->Double->String->Diagram B
example str start end ychange yfunc = fromVertices (vertices str start end ychange yfunc) # strokeLine
--        # lc red # center # pad 1.1

main::IO()
main = do 
	ychange <- getLine
	yfunc <- getLine
	str <- getLine
	start <- getLine
	end <- getLine
	mainWith (example str (read start::Double) (read end::Double) (read ychange::Double) yfunc)
