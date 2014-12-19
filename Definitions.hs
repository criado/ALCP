module Definitions where

import Prelude hiding (mod)
import qualified Prelude as P

-- Algunas estructuras algebraicas comunes:
data Structure t=
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
   
eea :: Structure t -> t->t->(t,t,t)
eea euclid a b 
  | a==zero   = (b, one, zero)
  | b==zero   = (a, zero, one)
  | otherwise = let (q,r)  = division a b
                    (d,s,t)= eea euclid b r
                in  (d,t,s-q*t)
  where Euclid zero one (==) (+) (-) (*) deg division=euclid

gcd :: Structure d-> d->d->d 
gcd euclid a b = d where (d,_,_)=eea euclid a b

pow :: Structure d->Integer->d->d
pow ring exp base=
  if exp==0 then one else a*a*(if exp `P.mod` 2==1 then base else one)
  where (*)=(.*) ring; one=_one ring
        a=pow ring (exp `P.div` 2) base
