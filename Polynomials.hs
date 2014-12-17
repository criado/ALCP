module Polynomials where

import Prelude
import qualified Prelude as P
import Definitions

pol::Structure d->Structure [d]
pol field = Euclid _one _zero (.==) (.+) (.-) (.*)  _deg _division where
  _one=[one]; _zero=[]
  (.+)= \   p q-> reduction.reverse $ zipWith (+) (reverse p) (reverse q)
  (.-) = \  p q-> p .+ map (-) q
  (.*)  = \ p q-> if (q.==_zero) then [] else map (*head q) p .+ (p.*tail q)
  (.==)  = \p q-> and zipWith (==) (reduction p) (reduction q)
  _deg    = toInteger.length.reduction
  _division= \p q->let q'=reduction q; (c,r)=divmonic p (q'.*[1/(q'!!0)])
                   in  (reduction (c.*[q'!!0]),reduction r)
  Field one zero (+) (-) (*) (==) (/) = field 
  reduction p=dropWhile(zero==)
  divmonic p q
    |q.==_zero     = error "division por cero" 
    |_deg q<_deg p = (p,_zero)
    |otherwise     = (c.+((p!!0):pad),r) 
    where (c,r)    = _division (p.-((map (*(p!!0)) q)++pad)) q 
          pad      = replicate (_deg p P.-_deg q) zero
