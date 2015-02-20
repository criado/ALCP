module Utilities where
-- En este módulo se hacen cosas con enteros

length::[a]->Integer
length=toInteger. Prelude.length

replicate::Integer->a->[a]
replicate a= Prelude.replicate (fromInteger a)

(!!)::[a]->Integer->a
a!!b=a Prelude.!! fromInteger b

-- Cálculo de primo. Rápido en la práctica, pero O(n^1.2)
-- www.haskell.org/haskellwiki/Prime_Numbers#Tree_merging
primes::[Integer]
primes  = 2:([3,5..] `minus` foldt [[p*p,p*p+2*p..]|p<-primes_])
  where
    primes_ = 3:([5,7..] `minus` foldt [[p*p,p*p+2*p..]|p<-primes_])
    foldt ((x:xs):t) = x : union xs (foldt (pairs t))
    pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t
    minus x y|x==[] || y==[] =x
             |otherwise= case compare (head x) (head y) of
                           LT->head x :minus (tail x) y
                           EQ->        minus (tail x) (tail y)
                           GT->        minus x        (tail y)
    union x y|x==[] =y |y==[] =x
             |otherwise = case compare (head x) (head y) of
                            LT-> head x : union (tail x) y
                            EQ-> head x : union (tail x) (tail y)
                            GT-> head y : union x        (tail y)

--Toma factorización cutre. Así se compensa una buena criba de primos.
factor::Integer->[(Integer,Integer)]
factor n=let l=[(p,maxexp n p)|p<-takeWhile (abs n>=) primes]
             maxexp n p=if n`mod`p==0 then 1+maxexp(n`div`p)p else 0
         in filter ((/=0).snd) l
