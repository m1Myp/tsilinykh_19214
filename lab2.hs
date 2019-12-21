data Complex a = ToComplex {
    realP :: a,
	imagineP :: a
} 
instance (Show a, Ord a, Eq a, Num a) => Show (Complex a) where
	show (ToComplex realP 0) = show realP
	show (ToComplex 0 1) = "i"
	show (ToComplex 0 (-1)) = "-i"
	show (ToComplex 0 imagineP) = (show imagineP) ++ "i" 
	show (ToComplex realP 1) = (show realP) ++ "+i" 
	show (ToComplex realP (-1)) = (show realP) ++ "-i" 
	show (ToComplex realP imagineP) |imagineP > 0 = (show realP) ++ "+" ++ (show imagineP) ++ "i" 
								      |imagineP < 0 = (show realP) ++ (show imagineP) ++ "i" 						
instance (Show a, Ord a, Eq a, Num a) => Eq (Complex a) where
	(==) (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = realP*realP + imagineP*imagineP == realP1*realP1 + imagineP1*imagineP1
 --Взято для длин, чтобы было логично с Ord. Тут еще должен быть sqrt, но как бы я не хочу флоатинг а, так шо не. Будем без корней.
							
instance (Show a, Ord a, Eq a, Num a) => Ord (Complex a) where
	compare  (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = compare (realP*realP + imagineP*imagineP) (realP1*realP1 + imagineP1*imagineP1)	
--Как грится, ну это ж вектора. Мама мама я патернматчер

instance (Show a, Ord a, Eq a, Num a) => Num (Complex a) where
	(+) (ToComplex realP imagineP) (ToComplex realP1 imagineP1) = ToComplex (realP + realP1) (imagineP +imagineP1)
	(*)	(ToComplex realP imagineP) (ToComplex realP1 imagineP1) = ToComplex (realP*realP1 - imagineP*imagineP1) (imagineP*realP1 + realP*imagineP1)
	signum (ToComplex realP imagineP) | realP > 0 && imagineP > 0 = ToComplex 1 0
									  | otherwise = ToComplex (-1) 0
	fromInteger int = ToComplex (fromInteger int) 0
	abs (ToComplex realP imagineP) = ToComplex (abs realP) (abs imagineP)
	negate (ToComplex realP imagineP) = ToComplex (negate realP) (negate imagineP)

data Quantum a = ToQuantum {
    complexP :: a,
	stringP :: String
}

instance (Show a, Ord a, Eq a, Num a) => Eq (Quantum a) where
	(==) (ToQuantum complexP stringP) (ToQuantum complexP1 stringP1) = (complexP == complexP1) && (stringP == stringP1) 

instance (Show a, Ord a, Eq a, Num a) => Show (Quantum a) where
	(show) (ToQuantum complexP stringP) = (show complexP) ++ " State: " ++ stringP

instance (Show a, Ord a, Eq a, Num a) => Ord (Quantum a) where
	compare (ToQuantum complexP stringP) (ToQuantum complexP1 stringP1) = compare complexP complexP1

instance Functor Quantum where
	fmap f (ToQuantum complexP stringP) = ToQuantum (f complexP) stringP
	
type Qubit a = [Quantum a]

toList::Qubit (Complex a) -> [Complex a]
toList q = [complexP | (ToQuantum complexP _) <- q]

toLabelList::Qubit (Complex a) -> [String]
toLabelList q = [stringP | (ToQuantum _ stringP) <- q]

fromList::[Complex a]->[String]->Qubit (Complex a)
fromList complexPList stringPList = [ToQuantum complexP stringP| complexP <- complexPList, stringP <- stringPList]

toPairList:: Qubit (Complex a)->[(Complex a,String)]
toPairList q = [(complexP, stringP) | (ToQuantum complexP stringP) <- q]

fromPairList:: [(Complex a,String)] -> Qubit (Complex a)
fromPairList pairList = [ToQuantum complexP stringP| (complexP,stringP) <- pairList]
--фолдр
scalarProduct::(Num a, Show a, Ord a, Eq a) => Qubit (Complex a) -> Qubit (Complex a) -> a
scalarProduct q1 q2 = foldr(+) 0 [r1*r2 + i1*i2| ToQuantum (ToComplex r1 i1) _ <- q1, ToQuantum (ToComplex r2 i2) _ <- q2]
--как и раньше
entagle::(Num a, Show a, Ord a, Eq a) => Qubit (Complex a) ->Qubit (Complex a) ->Qubit (Complex a)
entagle q1 q2 = [ToQuantum (complexP * complexP1) (stringP ++ stringP1) | (ToQuantum	 complexP stringP)<-q1 , (ToQuantum complexP1 stringP1) <- q2]

l = ToQuantum (ToComplex 2 3) "1"
k = ToQuantum (ToComplex 2 3) "1"
m = ToQuantum (ToComplex 2 3) "1"
