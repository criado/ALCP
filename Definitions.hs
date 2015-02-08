module Definitions where

--infix 0 ^-^ 
import Prelude hiding(gcd,mod,(+),(-),(*),(/),div)
import qualified Prelude   as P
import qualified Data.List as L

data Dictionary t=
  Field {_zero::t, _one::t, (.==)::t->t->Bool, (.+)::t->t->t,
         (.-) ::t->t->t   , (.*) ::t->t->t   , (./)::t->t->t  }|

  Euclid{_zero::t, _one::t, (.==)::t->t->Bool, (.+)::t->t->t,
         (.-) ::t->t->t,    (.*) ::t->t->t   , (./)::t->t->t,
         _deg ::t->Integer, _div::t->t->(t,t)                 }|

  UFD   {_zero::t, _one::t, (.==)::t->t->Bool, (.+)::t->t->t,
         (.-) ::t->t->t,    (.*) ::t->t->t,
         _factor::t->(t,[(t,Integer)])                        }

eea :: Dictionary t -> t->t->(t,t,t)
eea euclid a b 
  | b==zero   = (a, one, zero)
  | otherwise = let (q,r)  = div a b
                    (d,s,t)= eea euclid b r
                in  (d,t,s-(q*t))
  where Euclid zero one (==)(+)(-)(*)(/) deg div=euclid

gcd :: Dictionary d-> d->d->d 
gcd euclid a b = d where (d,_,_)=eea euclid a b

pow :: Dictionary d->d->Integer->d
pow ring base exp=
  if exp==0 then one  else a*a*(if exp `P.mod` 2 P.==1 then base else one )
  where (*)=(.*) ring; one=_one ring
        a=pow ring base (exp `P.div` 2)

mul :: Dictionary d->d->Integer->d
mul ring base fac=
  if fac==0 then zero else a+a+(if fac `P.mod` 2 P.==1 then base else zero)
  where (+)=(.+) ring; zero=_zero ring
        a=mul ring base (fac `P.div` 2)

order :: Dictionary  t->t->Maybe Integer
order ring elem=fmap(\i->1 P.+ toInteger i)$ L.findIndex(==one) $ take 1000 $iterate(*elem)elem
  where (*)=(.*)ring; one=_one ring; (==)=(.==)ring
