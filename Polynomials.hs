module Polynomials where

#include "Header.hs"

pol::Dictionary d->Dictionary [d]
pol field = Euclid _zero _one (.==) (.+) (.-) (.*) _deg _div where
  Field zero one (==) (+) (-) (*) (/) = field 

  _one=[one]; _zero=[]
  p.==q    = let l=length p P.- length q
             in  and $ zipWith (==) (pad(0 P.-l)++p) (pad l++q)
  p.+q     = let l=length p P.- length q 
             in reduction$zipWith (+) (pad(0 P.-l)++p) (pad l++q)
  p.-q     = p .+ map (zero-) q
  p.*q     = reduction$ if q.==_zero 
                        then [] 
                        else (map (*head q) p++pad (length $tail q)) .+ (p.*tail q)
  _deg     = length.reduction
  _div p q
    |q.==_zero = error "división por cero"
    |otherwise=(reduction (c.*[one/head q']),reduction r)
    where q'=reduction q
          (c,r)=divmonic p (q'.*[one/head q'])

  pad a=replicate a zero
  reduction = dropWhile (zero==)
  divmonic p q
    |q.==_zero     = error "division por cero" 
    |_deg p<_deg q = (_zero,p)
    |otherwise     = (c.+(head p:pad),r) 
    where (c,r)    = divmonic (p.-(map (*head p) q++pad)) q 
          pad      = replicate (_deg p P.- _deg q) zero

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
