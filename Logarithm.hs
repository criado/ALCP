module Logarithm  where
#include "Header.hs"
import Quotient
import Polynomials
import Numbers
import FiniteField

--La idea es sencilla: 
-- α^γ=β siendo α un generador de G, cíclico de orden n.
-- bγ=a mod n. Entonces defines f:G->G "aleatoria"
-- Se trata de buscar un ciclo, como guardabas los exponentes,
-- reahaces el resultado. No da una respuesta, si no una congruencia.
-- http://www.dtc.umn.edu/~odlyzko/doc/discrete.logs.hff.pdf
logarithm::Show d=>Dictionary d->Int->d->d->(Integer,Integer)
logarithm field seed α β=
  end $ loop $ zip (iterate f (one,0,0)) (iterate (f.f) (one,0,0)) 
   
  where f (x,a,b)
          |opt P.==0 = (α*x, 1 P.+a,       b)
          |opt P.==1 = (x*x, 2 P.*a, 2 P.* b) 
          |opt P.==2 = (β*x,      a, 1 P.+ b)
          where opt= (sum (map ord(show x)) `P.div` seed)`P.mod` 3
          
        Field zero one (==)(+)(-)(*)(/)=field
        n= fromJust $ order field α

        end ((_,a,b),(_,a',b'))= 
          assert (b'' /=zero)
           ((./) (integer`mod`n') a'' b'' ,n')
          where n' = fst $ n    `div` g
                a''= fst $ a'-a `div` g
                b''= fst $ b-b' `div` g
                g=gcd integer (a'-a) $ gcd integer (b-b') n
                Euclid zero one (==)(+)(-)(*)(/) deg div=integer
                                 
        loop= fromJust. L.find (\((x,_,_),(y,_,_))-> x==y).tail
