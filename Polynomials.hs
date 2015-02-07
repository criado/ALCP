module Polynomials where

#include "Header.hs"
import Definitions

pol::Dictionary d->Dictionary [d]
pol field = Euclid _zero _one (.==) (.+) (.-) (.*) _deg _division where
  Field zero one (==) (+) (-) (*) (/) = field 

  _one=[one]; _zero=[]
  p.==q         = and $ zipWith (==) (reduction p) (reduction q)
  p.+q          = let l=fromInteger $ _deg p P.- _deg q 
                      pad a=(++) $ replicate a zero
                  in zipWith (+) (pad (0 P.- l) p) (pad l q)
  p.-q          = p .+ map (zero-) q
  p.*q          = if q.==_zero then [] else map (*head q) p .+ (p.*tail q)
  _deg          = toInteger.length.reduction
  _division p q = let q'=reduction q
                      (c,r)=divmonic p (q'.*[one/head q'])
                  in  (reduction (c.*[head q']),reduction r)

  reduction = dropWhile (zero==)
  divmonic p q
    |q.==_zero     = error "division por cero" 
    |_deg q<_deg p = (p,_zero)
    |otherwise     = (c.+(head p:pad),r) 
    where (c,r)    = divmonic (p.-(map (*head p) q++pad)) q 
          pad      = let degInt=fromInteger._deg 
                     in replicate (degInt p P.- degInt q) zero

derivate::Dictionary d->[d]->[d]
derivate ring p=let l=length p
                in [mul ring (l P.-i) (p!!(l P.-i)) 
                    |i<-[l P.-1,l P.-2.. 0] ]
{-
cyclotomic::Dictionary d->[d]
cyclotomic field =
  [foldr (\a b->fst $ _division (pol field) b a) (start n) (dividers n)| n<-[1..]]
  where Field one zero (==) (+) (-) (*) (/)= field
        start n=[one]++replicate (n P.- 1) zero++[zero-one]
        dividers n=[cyclotomic field!!k|k<-[1..n-1], k `P.mod` n P.==0]
 -}    
