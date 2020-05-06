import Data.Ratio
type Set a = [a]

mappend1 :: (a -> [b]) -> [a] -> [b]
mappend1 f [] = []
mappend1 f (x:xs) = f x ++ mappend1 f xs 

builds :: a -> (a -> [a]) -> Set a
builds a f = set
           where set=a: mappend1 f set

nextrationalnumber :: Rational-> [Rational]
nextrationalnumber x
            |x==0 =[1,-1]
            |x<0 =[]
            |otherwise =[(n+d)%d,n%(n+d),-((n+d)%d),-(n%(n+d))]
                where 
                    n = numerator x
                    d = denominator x

rationalset :: [Rational]
rationalset = builds 0 nextrationalnumber

a=take 20 rationalset



