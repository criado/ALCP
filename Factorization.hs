module Factorization where
#include "Header.hs"
import Quotient
import Polynomials
import Numbers
import FiniteField

--Test de irreducibilidad (Rabin) en 𝔽q, q=p^m, p primo
irred::Integer->Integer->[[Integer]]->Bool
irred p n f=
  zero == h d && 
   all (\n_i-> one == gcd (pol field) f (h n_i) ) primedivs
  where 
    d=_deg (pol field) f P.- 1 
    field@(Field _zero _one (.==)(.+)(.-)(.*)(./))=finite p n
    Field zero one (==)(+)(-)(*)(/) =pol field`mod`f
    h n_i=pow (pol field`mod`f) x (pow integer p (n_i P.* n) ) - x
    x=[_one,_zero] --polinomio x
    primedivs=[d `P.div` p_i| p_i<-(fst.unzip.factor) d]

factorPol::Integer->Integer->[[Integer]]->[[[Integer]]]
factorPol p n f= fact1 p n f >>=
                 fact2 p n.monic(finite p n) >>=
                 fact3 p n.monic(finite p n)

fact1::Integer->Integer->[[Integer]]->[[[Integer]]]
fact1 p n f=
  if deg f P.<3 then [f] else f2:fact1 p n g
  where f'= derivate (finite p n) f
        g = gcd fqx f f'
        f2= fst $ _div fqx f g
        fqx@(Euclid zero one (==)(+)(-)(*)(/)deg div)=pol(finite p n)

fact2::Integer->Integer->[[Integer]]->[[[Integer]]]
fact2 p n f=
  (replicate g i:fact2 p n (fst `div` g))
  where Euclid zero one (==)(+)(-)(*)(/) deg div= pol $ finite p n
-- where h n_i=pow (pol field`mod`f) x (pow integer p (n_i P.* n) ) - x

fact3::Integer->Integer->[[Integer]]->[[[Integer]]]
fact3 p n f= error "ash"
