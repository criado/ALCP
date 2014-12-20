module Definitions where

import Prelude hiding (mod,(+),(-),(*),(/),div)
import qualified Prelude as P
import Data.List

-- Cálculo de primos de www.haskell.org/haskellwiki/Prime_Numbers#Tree_merging
-- Ridículamente rápido en la práctica, O(n^1.2)
primes::[Integer]
primes  = 2 : ([3,5..] `minus` foldt [[p P.*p,p P.*p P.+2 P.*p..] | p<-primes_])
  where
    primes_ = 3 : ([5,7..] `minus` foldt [[p P.*p,p P.*p P.+2 P.*p..] | p<-primes_])
    foldt ((x:xs):t) = x : union xs (foldt (pairs t))
    pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t
    minus x y|x==[] || y==[] =x
             |otherwise= case compare (head x) (head y) of
                           LT->head x :minus (tail x) y
                           EQ->        minus (tail x) (tail y)
                           GT->        minus x        (tail y)
    union x y|x==[] =y
             |y==[] =x
             |otherwise = case compare (head x) (head y) of
                            LT-> head x : union (tail x) y
                            EQ-> head x : union (tail x) (tail y)
                            GT-> head y : union x        (tail y)

--data Dictionary d=ℤ|Pol (Descritptor d)|(Descriptor d) :/d|Fract (Descriptor d)

zero::Dictionary d-> d                                          ; zero=_zero
one::Dictionary d-> d                                           ; one=_one
(+)::(Dictionary d->d)->(Dictionary d->d)->Dictionary d->d      ; (a+b) des=(.+) des (a des) (b des)
(-)::(Dictionary d->d)->(Dictionary d->d)->Dictionary d->d      ; (a-b) des=(.-) des (a des) (b des)
(*)::(Dictionary d->d)->(Dictionary d->d)->Dictionary d->d      ; (a*b) des=(.*) des (a des) (b des)
(/)::(Dictionary d->d)->(Dictionary d->d)->Dictionary d->d      ; (a/b) des=(./) des (a des) (b des)
deg::(Dictionary d->d)->Dictionary d->Integer                   ; deg a des=_deg des (a des)
div::(Dictionary d->d)->(Dictionary d->d)->Dictionary d->(d,d)  ; div a b des=_division des(a des)(b des)
fact::(Dictionary d->d)->Dictionary d-> (d,[(d,Integer)])       ; fact a des=_factor des (a des)
literal::d->Dictionary d->d                                     ; literal a des=a
getOps::Dictionary d->Dictionary d                              ; getOps=id

-- Algunas estructuras algebraicas comunes:
data Dictionary t=
  Ring  {_zero::t, _one::t, (.==)::t->t->Bool, (.+)::t->t->t, 
         (.-) ::t->t->t   , (.*) ::t->t->t                                }|
  Field {_zero::t, _one::t, (.==)::t->t->Bool, (.+)::t->t->t,
         (.-) ::t->t->t   , (.*) ::t->t->t   , (./)::t->t->t              }|
  Euclid{_zero::t, _one::t, (.==)::t->t->Bool, (.+)::t->t->t, (.-)::t->t->t,
         (.*) ::t->t->t   , _deg ::t->Integer, _division::t->t->(t,t)     }|
  UFD   {_zero::t, _one::t, (.==)::t->t->Bool, (.+)::t->t->t, (.-)::t->t->t,
         (.*) ::t->t->t   , _factor::t->(t,[(t,Integer)])}
   --Division euclidea
   -- dados a,b y b/=0, se buscan q,r, cociente y resto de tal forma que
   --  a=b*q+r
   --  r=0 o deg r<deg b
   --
eea :: Dictionary t -> t->t->(t,t,t)
eea euclid a b 
  | a==zero   = (b, one, zero)
  | b==zero   = (a, zero, one)
  | otherwise = let (q,r)  = division a b
                    (d,s,t)= eea euclid b r
                in  (d,t,s-q*t)
  where Euclid zero one (==) (+) (-) (*) deg division=euclid

gcd :: Dictionary d-> d->d->d 
gcd euclid a b = d where (d,_,_)=eea euclid a b

pow :: Dictionary d->Integer->d->d
pow ring exp base=
  if exp==0 then one else a*a*(if exp `P.mod` 2==1 then base else one)
  where (*)=(.*) ring; one=_one ring
        a=pow ring (exp `P.div` 2) base

order :: Dictionary  t->t->Maybe Integer
order ring elem=fmap (\i->1 P.+ toInteger i) $ findIndex (==one) $ take 1000 $  iterate (*elem) elem
  where (*)=(.*)ring; one=_one ring; (==)=(.==)ring
