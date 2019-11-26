data Complex a = ToComplex {
    realP :: Integer,
	imagineP :: Integer
} 
instance (Show a) => Show (Complex a) where
	(show) (ToComplex realP 0) = show realP
	(show) (ToComplex 0 1) = "i"
	(show) (ToComplex 0 (-1)) = "-i"
	(show) (ToComplex 0 imagineP) = (show imagineP) ++ "i" 
	(show) (ToComplex realP 1) = (show realP) ++ "+i" 
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
	
--instance Functor Quantum where
--    fmap func (ToQuantum complexP stringP) = ToQuantum (func complexP) stringP
	
type Qubit a = [Quantum a]

toList::Qubit a -> [Complex a]
toList q = [complexP | (ToQuantum complexP _) <- q]

toLabelList::Qubit a -> [String]
toLabelList q = [stringP | (ToQuantum _ stringP) <- q]

fromList::[Complex a]->[String]->Qubit a
fromList complexPList stringPList = [ToQuantum complexP stringP| complexP <- complexPList, stringP <- stringPList]

toPairList:: Qubit a->[(Complex a,String)]
toPairList q = [(complexP, stringP) | (ToQuantum complexP stringP) <- q]

fromPairList:: [(Complex a,String)] -> Qubit a
fromPairList pairList = [ToQuantum complexP stringP| (complexP,stringP) <- pairList]

scalarProduct:: Qubit a ->Qubit a -> Integer
scalarProduct [] [] = 0
scalarProduct _ [] = 0
scalarProduct [] _ = 0
scalarProduct ((ToQuantum (ToComplex r1 i1) _):q1) ((ToQuantum (ToComplex r2 i2) _):q2) = (r1+r2)+(i1 + i2) + (scalarProduct q1 q2)
--Ну это ж скалярное произведение? Скаляр == длина == Интегер, какое А?
entagle::Qubit a ->Qubit a ->Qubit a
entagle [] [] = []
entagle _ [] = []
entagle [] _ = []
entagle ((ToQuantum (ToComplex r1 i1) str1):q1) ((ToQuantum (ToComplex r2 i2) str2):q2) = (ToQuantum (ToComplex (r1*r2-i1*i2) (r1*i2 + r2*i1)) (concat [str1,str2])) : (entagle q1 q2)

l = ToQuantum (ToComplex 2 3) "1"
k = ToQuantum (ToComplex 2 3) "1"
m = ToQuantum (ToComplex 2 3) "1"
