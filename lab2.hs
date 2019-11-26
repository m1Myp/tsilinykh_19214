data Complex a = ToComplex {
    realP :: Integer,
	imagineP :: Integer
} 
instance (Show a) => Show (Complex a) where
	(show) (ToComplex realP 0) = show realP
	(show) (ToComplex 0 1) = "i"
	(show) (ToComplex 0 (-1)) = "-i"
	(show) (ToComplex 0 imagineP) = (show imagineP) ++ "i" 
	(show) (ToComplex realP 1) = (show realP) ++ "+" ++ "i" 
	(show) (ToComplex realP (-1)) = (show realP) ++ "-i" 
	(show) (ToComplex realP imagineP) |imagineP > 0 = (show realP) ++ "+" ++ (show imagineP) ++ "i" 
								      |imagineP < 0 = (show realP) ++ (show imagineP) ++ "i" 
							
instance (Eq a) => Eq (Complex a) where
	(==) (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = realP*realP + imagineP*imagineP == realP1*realP1 + imagineP1*imagineP1
 --Взято для длин, чтобы было логично с Ord. Тут еще должен быть sqrt, но как бы ИНТЕДЖЕР != Дабл, так шо не. Будем без корней.
							
instance (Ord a) => Ord (Complex a) where
	(>) (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = realP*realP + imagineP*imagineP > realP1*realP1 + imagineP1*imagineP1	
	(<) (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = realP*realP + imagineP*imagineP <	realP1*realP1 + imagineP1*imagineP1				
	(>=) (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = realP*realP + imagineP*imagineP <= realP1*realP1 + imagineP1*imagineP1	
	(<=) (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = realP*realP + imagineP*imagineP <= realP1*realP1 + imagineP1*imagineP1	
--Как грится, ну это ж вектора. Мама мама я патернматчер

data Quantum a = ToQuantum {
    complexP :: Complex a,
	stringP :: String
}

instance (Eq a) => Eq (Quantum a) where
	(==) (ToQuantum complexP stringP) (ToQuantum complexP1 stringP1) = (complexP == complexP1) && (stringP == stringP1) 

instance (Show a) => Show (Quantum a) where
	(show) (ToQuantum complexP stringP) = (show complexP) ++ " State: " ++ stringP

instance (Ord a) => Ord (Quantum a) where
	(>) (ToQuantum complexP stringP) (ToQuantum complexP1 stringP1) = complexP > complexP1
	(<) (ToQuantum complexP stringP) (ToQuantum complexP1 stringP1) = complexP < complexP1
	(>=) (ToQuantum complexP stringP) (ToQuantum complexP1 stringP1) = complexP >= complexP1
	(<=) (ToQuantum complexP stringP) (ToQuantum complexP1 stringP1) = complexP <= complexP1
	
