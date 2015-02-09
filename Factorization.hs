module Factorization where
#include "Header.hs"
import Quotient
import Polynomials
import Numbers
import FiniteField

type Pol=[[Integer]]

irred::Integer->Integer->Pol->Bool
irred p n f= --Test de irreducibilidad (Rabin) en 𝔽q, q=p^n, p primo
  zero == h d && 
   all (\n_i-> one == gcd (pol field) f (h n_i) ) primedivs
  where 
    d=_deg (pol field) f P.- 1 
    field@(Field _zero _one (.==)(.+)(.-)(.*)(./))=finite p n
    Field zero one (==)(+)(-)(*)(/) =pol field`mod`f
    h n_i=pow (pol field`mod`f) x (pow integer p (n_i P.* n) ) - x
    x=[_one,_zero] --polinomio x
    primedivs=[d `P.div` p_i| p_i<-(fst.unzip.factor) d]

factorPol::Integer->Integer->Pol->[Pol]
factorPol p n f= fact1 p n f >>=
                 (L.transpose.fact2 p n.monic(finite p n)) >>=
                 fact3 p n.monic(finite p n)

factorPol p n f=  (\u-> curry fact3 p n $ zip [1..] u)
                 $ L.Transpose $ map (fact1 p n) $ fact1 p n f

fact1::Integer->Integer->Pol->[Pol]
fact1 p n f=
  if deg f P.<3 then [f] else f2:fact1 p n g
  where f'= derivate (finite p n) f
        g = gcd fqx f f'
        f2= fst $ _div fqx f g
        fqx@(Euclid zero one (==)(+)(-)(*)(/)deg div)=pol(finite p n)

fact2::Integer->Integer->Pol->[Pol]
fact2 p n f=
  aux f (pow (pol field) x q)
  where Euclid zero one (==)(+)(-)(*)(/) deg div= pol field
        field= finite p n
        q= pow integer p n
        x= [_one field, _zero field]
        aux::Pol->Pol->[Pol]
        aux f xq | deg f P.==1= []
                 | g/=one     = g:aux f' xq'
                 | otherwise= aux f xq'
                 where g  = gcd (pol field) f (xq-x)
                       xq'= pow (pol field) xq q
                       f' = fst $ f `div` g

fact3::Integer->Integer->Integer->Pol->[Pol]
fact3 p n f= error "ash"
