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
   all (isUnit (pol field).gcd (pol field) f.h) primedivs
  where 
    d=_deg (pol field) f P.- 1 
    field@(Field _zero _one (.==)(.+)(.-)(.*)(./))=finite p n
    Field zero one (==)(+)(-)(*)(/) =pol field`mod`f
    h n_i=pow (pol field`mod`f) x (pow integer p (n_i P.* n) ) - x
    x=[_one,_zero] --polinomio x
    primedivs=[d `P.div` p_i| p_i<-(fst.unzip.factor) d]
{-
factorPol::Integer->Integer->Pol->[Pol]
factorPol p n f= fact1 p n f >>=
                 (L.transpose.fact2 p n.monic(finite p n)) >>=
                 fact3 p n.monic(finite p n)

factorPol p n f=  (\u-> curry fact3 p n $ zip [1..] u)
                 $ L.Transpose $ map (fact1 p n) $ fact1 p n f
-}

fact1::Integer->Integer->Pol->[Pol]
fact1 p n f
  | f' == zero  = map (stride p) $ fact1 p n (unstride p f)
  | deg g P.==1 = [f]
  | otherwise   = f2:fact1 p n g
  where f'= derivate (finite p n) f
        g = gcd (pol(finite p n)) f f'
        f2= f/g
        Euclid zero one (==)(+)(-)(*)(/)deg div= pol(finite p n)
        stride p= 
          concatMap(\a->replicate(p P.-1)(_zero$finite p n)++[a])
        unstride p f=reverse[reverse f!!i| i<-[0,p..deg f]]

fact2::Integer->Integer->Pol->[(Pol,Integer)]
fact2 p n f=
  aux 1 f (pow (pol field`mod`f) x q)
  where Euclid zero one (==)(+)(-)(*)(/) deg div= pol field
        field= finite p n
        q= pow integer p n
        x= [_one field, _zero field]
        aux::Integer->Pol->Pol->[(Pol,Integer)]
        aux d f xq | deg f P.==1= [(f,1)]
                   | deg g P./=1= (g, d):aux (d P.+1) f' xq'
                   | otherwise  =        aux (d P.+1) f xq'
                   where g  = gcd (pol field) f (xq-x)
                         xq'= pow (pol field `mod` f) xq q
                         f' = f/g

fact3::Integer->Integer->Integer->Pol->[Pol]
fact3 p n d f= error "ash"
