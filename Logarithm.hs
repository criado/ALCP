module Logarithm  where
#include "Header.hs"
import Quotient
import Polynomials
import Numbers
import FiniteField

logarithm::Show d=>Dictionary d->d->d->(Integer,Integer)
logarithm field α β=
  chineseInteger$ [0..12] >>= (\u->logarithmEq field u α β)

logarithmEq::Show d=>Dictionary d->Int->d->d->[(Integer,Integer)]
logarithmEq field seed α β=
  end $ loop $ zip (iterate f (one,0,0)) (iterate (f.f) (one,0,0)) 
   
  where f (x,a,b)
          |opt P.==0 = (α*x, 1 P.+a,       b)
          |opt P.==1 = (x*x, 2 P.*a, 2 P.* b) 
          |opt P.==2 = (β*x,      a, 1 P.+ b)
          where opt= hash seed x `P.mod` 3
          
        Field zero one (==)(+)(-)(*)(/)=field
        n= fromJust $ order field α

        end ((_,a,b),(_,a',b'))= 
          [((./) (integer`mod`n') a'' b'' ,n')| b'' /=zero]
          where n' = fst $ n    `div` g
                a''= fst $ a'-a `div` g
                b''= fst $ b-b' `div` g
                g=gcd integer (a'-a) $ gcd integer (b-b') n
                Euclid zero one (==)(+)(-)(*)(/) deg div=integer
                                 
        loop= fromJust. L.find (\((x,_,_),(y,_,_))-> x==y).tail

hash seed x=
  hashaux (show x)
  where
    hashaux l=pow (integer`mod`541) (toInteger$ seed P.+1) (tonum l) 
    tonum []=0
    tonum (c:cs)= tonum cs P.* 256 P.+ (toInteger$ord c)
