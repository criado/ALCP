module Fract where 

#include "Header.hs"
import Definitions
import Numbers

fract::Dictionary d->Dictionary (d,d)
fract euclid=Field{
  _zero=(zero,one),
  _one=(one,one),
  (.+) = \(a,b) (c,d)->reduce ((a*d)+(c*b),b*d),
  (.-) = \(a,b) (c,d)->reduce ((a*d)-(c*b),b*d),
  (.*) = \(a,b) (c,d)->reduce (a*c,b*d),
  (.==)= \(a,b) (c,d)->       (a*d)==(b*c),
  (./) = \(a,b) (c,d)->reduce (a*d,b*c)
 } where Euclid zero one (==) (+) (-) (*) deg div =euclid
         reduce (a,b)=(fst$ a`div`d, fst$ b`div`d)
                      where d=gcd euclid a b

𝕢=fract 𝕫; rational=𝕢
