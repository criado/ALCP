module Quotient where

#include "Header.hs"

mod::Dictionary d->d->Dictionary d
mod euclid m=Field{ 
    _zero= zero, _one=one,
    (.==)= \a b-> snd (div (a-b) m)==zero,
    (.+) = \a b-> snd $ div (a+b) m,
    (.-) = \a b-> snd $ div (a-b) m,
    (.*) = \a b-> snd $ div (a*b) m,
    (./) = \a b-> let g=gcd euclid a $ gcd euclid b m
                      a'=fst $ div a g
                      b'=fst $ div b g
                      m'=fst $ div m g
                      (d',s',t')=eea euclid b' m'  
                  in reduce $ assert (deg d' P.== 1) (a'*s'/d')
  } where Euclid zero one (==)(+)(-)(*)(/) deg div=euclid
          reduce a=snd$div a m

--Los modulos deben ser primos dos a dos (no se comprueba)
--La solución es módulo el producto de todo
chinese :: Dictionary d-> [(d,d)]->(d,d)
chinese euclid= foldr1 chinese2
  where chinese2 (x1,y1) (x2,y2)= reduce
          ((x1*y2*(./) (euclid`mod`y1) one y2)+
           (x2*y1*(./) (euclid`mod`y2) one y1), y1*y2)
        Euclid zero one (==)(+)(-)(*)(/) deg div=euclid
        reduce (a,b)=(snd$ a `div`b,b)
