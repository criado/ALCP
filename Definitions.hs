module Definitions where

infixl 6 +:
infixl 6 -:
infixl 7 *: 
infixl 7 /:

-- Algunas estructuras algebraicas comunes:
class Eq g => Group g where
 (*:) ::  g->g->g
 unit :: g

class (Group r) => Ring r where
 (+:) :: r->r->r
 zero :: r
 addinverse :: r->r
 (-:) :: r->r->r
 a-:b = a+:(addinverse b)

class (Ring f) => Field f where
 inverse :: f->f	
 (/:) :: f->f->f
 a/:b = a*:(inverse b)

class (Ring e) => Euclidean e where
 deg :: e->Integer
 division :: e->e->(e,e) --Division euclidea
-- dados a,b y b/=0, se buscan q,r, cociente y resto de tal forma que
--  a=b*q+r
--  r=0 o deg r<deg b

eea :: Euclidean e => e->e->(e,e,e)
eea a b | b==zero   = (a, zero, a)
        | otherwise = let (q,r)  = division a b
                          (d,s,t)= eea b r
                      in (d,t,s-:(q*:t))

gcd :: Euclidean e => e->e->e 
gcd a b = d where (d,_,_)=eea a b
