module Polynomials where

import Prelude
import qualified Prelude as P
import Definitions

pol::Structure d->Structure [d]
pol field = Euclid _zero _one (.==) (.+) (.-) (.*) _deg _division where
  _one=[one]; _zero=[]
  p.==q         = and $ zipWith (==) (reduction p) (reduction q)
  p.+q          = (reduction.reverse) $ zipWith (+) (reverse p) (reverse q)
  p.-q          = p .+ map (zero-) q
  p.*q          = if q.==_zero then [] else map (*head q) p .+ (p.*tail q)
  _deg          = toInteger.length.reduction
  _division p q = let q'=reduction q
                      (c,r)=divmonic p (q'.*[one/head q'])
                  in  (reduction (c.*[head q']),reduction r)

  Field one zero (==) (+) (-) (*) (/) = field 
  reduction = dropWhile (zero==)
  divmonic p q
    |q.==_zero     = error "division por cero" 
    |_deg q<_deg p = (p,_zero)
    |otherwise     = (c.+(head p:pad),r) 
    where (c,r)    = divmonic (p.-(map (*head p) q++pad)) q 
          pad      = let degInt=fromInteger._deg 
                     in replicate (degInt p P.- degInt q) zero
