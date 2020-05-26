data Logic=A|B|C
	| And Logic Logic
	| Or Logic Logic
	| Not Logic
	| Imply Logic Logic
	| Equiv Logic Logic
	deriving (Eq, Show)
	
distribute :: Logic -> Logic
distribute (And a (Or  b c)) = Or (And a b) (And a c)
distribute (Or  a (And b c)) = And (Or a b) (Or a c)
distribute x = x

demorgan :: Logic -> Logic
demorgan (Not (And a b)) = Or (Not a) (Not b)
demorgan (Not (Or a b)) = And (Not a) (Not b)
demorgan x = x






