{-# LANGUAGE Rank2Types #-}

module Definitions where

import Prelude hiding (id)

-- Algunas estructuras algebraicas comunes:

data Structure t=
 Group {(.<>)::t->t->t, _id :: t                                                                                }|
 Ring  {_zero::t, _one::t, (.+)::t->t->t, (.-)::t->t->t, (.*)::t->t->t                                          }|
 Field {_zero::t, _one::t, (.+)::t->t->t, (.-)::t->t->t, (.*)::t->t->t, (./)::t->t->t                           }|
 Euclid{_zero::t, _one::t, (.+)::t->t->t, (.-)::t->t->t, (.*)::t->t->t, _deg::t->Integer, _division::t->t->(t,t)}|
 UFD   {_zero::t, _one::t, (.+)::t->t->t, (.-)::t->t->t, (.*)::t->t->t, _factor::t->(t,[(t,Integer)])}
   --Division euclidea
   -- dados a,b y b/=0, se buscan q,r, cociente y resto de tal forma que
   --  a=b*q+r
   --  r=0 o deg r<deg b

mulGroup::Structure t->Structure t
mulGroup ring=Group{(.<>)=(.*) ring, _id=_one ring}

sumGroup::Structure t->Structure t
sumGroup ring=Group{(.<>)=(.+) ring, _id=_zero ring}

eea :: Eq t => Structure t -> t->t->(t,t,t)
eea euclid a b 
          | b==zero   = (a, zero, a)
          | otherwise = let (q,r)  = division a b
                            (d,s,t)= eea euclid b r
                        in (d,t,s-q*t)
          where (-)=(.-) euclid
                (*)=(.*) euclid
                zero=_zero euclid
                division=_division euclid

gcd :: Eq d=>Structure d-> d->d->d 
gcd euclid a b = d where (d,_,_)=eea euclid a b
