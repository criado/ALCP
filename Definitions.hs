{-# LANGUAGE Rank2Types #-}

module Definitions where

import Prelude hiding (id)

-- Algunas estructuras algebraicas comunes:

data Structure d=
 Group {(.<>)::d->d->d, _id :: d                                                                                }|
 Ring  {_zero::d, _one::d, (.+)::d->d->d, (.-)::d->d->d, (.*)::d->d->d                                          }|
 Field {_zero::d, _one::d, (.+)::d->d->d, (.-)::d->d->d, (.*)::d->d->d, (./)::d->d->d                           }|
 Euclid{_zero::d, _one::d, (.+)::d->d->d, (.-)::d->d->d, (.*)::d->d->d, _deg::d->Integer, _division::d->d->(d,d)}
 UFD   {_zero::d, _one::d, (.+)::d->d->d, (.-)::d->d->d, (.*)::d->d->d, _factor::d->(d,[d,Integer])}
   --Division euclidea
   -- dados a,b y b/=0, se buscan q,r, cociente y resto de tal forma que
   --  a=b*q+r
   --  r=0 o deg r<deg b

mulGroup::Structure d->Structure d
mulGroup ring=Group{(.<>)=(.*) ring, _id=_one ring}

sumGroup::Structure d->Structure d
sumGroup ring=Group{(.<>)=(.+) ring, _id=_zero ring}

eea :: Eq d => Structure d -> d->d->(d,d,d)
eea euclid a b 
          | b==zero   = (a, zero, a)
          | otherwise = let (q,r)  = division a b
                            (d,s,t)= eea euclid b r
                        in (d,t,s-q*t)
          where (-)=(.-) euclid
                (*)=(.*) euclid
                zero=_zero euclid
                division=_division euclid
{-
gcd :: Euclidean e => e->e->e 
gcd a b = d where (d,_,_)=eea a b
-}
