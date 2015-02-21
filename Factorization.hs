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
  | deg f P.<=1 = [f]
  | f' == zero  = concat $ replicate p $ fact1 p n (unstride f)
  | deg g P.==1 = [f]
  | otherwise   = f2:fact1 p n g
  where f'= derivate (finite p n) f
        g = monic(finite p n) $ gcd (pol(finite p n)) f f'
        f2= (./) (pol$ finite p n) f g
        unstride f=reverse[reverse f!!i| i<-[0,p..deg f]]
        Euclid zero one (==)(+)(-)(*)(/)deg div= pol(finite p n)

fact2::Integer->Integer->Pol->[(Pol,Integer)]
fact2 p n f=
  aux 1 f (pow (pol field`mod`f) x q)
  where Euclid zero one (==)(+)(-)(*)(/) deg div= pol field
        field= finite p n
        q= pow integer p n
        x= [_one field, _zero field]
        aux::Integer->Pol->Pol->[(Pol,Integer)]
        aux d f xq | deg f P.==1= []
                   | deg g P./=1= (g, d):aux d f' xq
                   | otherwise  =        aux (d P.+1) f xq'
                   where g  = monic field$ gcd (pol field) f (xq-x)
                         xq'= pow (pol field `mod` f) xq q
                         f' = f/g

fact3::Integer->Integer->(Pol,Integer)->[Pol]
fact3 p n (f,d)= assert(p P./= 2)$ trace (show (take 3 pols)) $
  fact [f] pols
  where fact factors pols
          | length factors P.==r = factors
          | otherwise = fact (concatMap split factors) pols'
          where (h:pols')= pols
                g=pow (euclid`mod`f) h exp-one
                  where Field zero one (==)(+)(-)(*)(/)=euclid`mod`f
                exp=(p P.^(n P.*d) P.-1) `P.div` 2
                split f| deg f P.==d = [f]
                       | m==one   = [f]
                       | m==f     = [f]
                       | otherwise= [f/m,m]
                       where m=monic(finite p n)$ gcd euclid f g
        euclid@(Euclid zero one(==)(+)(-)(*)(/)deg div)=pol$finite p n

        l=deg f P.- 1
        r=l `P.div` d

        pols::[Pol]
        pols= filter (/=zero)$ 
          map (take l) $ iterate (drop l ) 
            $ map(take n)$ iterate (drop n)$ map (getRandom p) [1..]
        getRandom::Integer->Integer->Integer
        getRandom n i =unsafePerformIO $ do
          g<-getStdGen; return $ randomRs (0,n P.-1) g !!i

