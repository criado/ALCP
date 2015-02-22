module Hensel  where
#include "Header.hs"

import Quotient
import Polynomials
import Numbers
import FiniteField
import Factorization
import Utilities

--Paso 1
primeMod::[Integer]->Integer
primeMod f= const 7 $
  fromJust$ L.find (test f) primes
  where test f p=
          (head f`P.mod`p P./=0) && (f'/=[]) && (m==[1])
          where f'=derivate (integer`mod`p) f
                m=monic (integer`mod`p)$
                    gcd (pol(integer`mod`p)) f f'
                (==)=(.==) (pol(integer`mod`p))

--Paso 3: devuelve el módulo que debe sobrepasar Hensel
mignotteBound::Integer->[Integer]->Integer
mignotteBound p f=
  ceiling bound
  where norm= sqrt$ sum$ map(\a->fromInteger (a*a)) f
        bound= 2.0^length f*norm

--Paso 4: devuelve, en el mismo formato, el hensel Lift, de n a n^2
henselLift n (f,g,h,s,t)= trace ("\nHensel: "++show n++" "++show f++" "++show g++" "++show h++" "++show s++" "++show t++"\n"++"\nHensel: "++show n++" "++show f++" "++show g'++" "++show h'++" "++show s'++" "++show t'++"\n")  $
  assert ((.==) (pol(integer`mod`n)) f (g'*h')) $ 
  assert ((.==) (pol(integer`mod`n)) one ((s'*g')+(t'*h'))) $
  assert (f==(g'*h')) $ 
  assert (one==((s'*g')+(t'*h'))) $
  (f,g',h',s',t')
  where (g',h')= ((g*(one+q))+(t*δ), h+r)  
                 where δ=f-(g*h)
                       (q,r)=(s*δ) `div` h
        (s',t')= (s-r, ((one-ε)*t)-(g'*q))
                 where ε=(s*g')+(t*h')-one
                       (q,r)=(s*ε) `div` h'

        Euclid zero one(==)(+)(-)(*)(/) deg div=pol (integer`mod`(n P.*n))

--Paso 5: recombina los factores como buenamente puede
{-
recombine::Integer->[Integer]->[[Integer]]->[[Integer]]
recombine n f dividers
  | null dividers = [f]
  | otherwise = 
  where l=head f
-}
  
--Devuelve los coeficientes al representante mas cercano a 0
trueRepr n= map (\a-> if a<=(n`div`2) then a else a-n)

--Da la factorización elevada factorizacion hasta p^N
factorPolInt::[Integer]->[[Integer]]
factorPolInt f= trace ("factors: "++show factors++"\nElevados: "++show (liftaux p(zipWith prep factors(products factors)))) $
  map (trueRepr (49 P.*49)) $ tail $ dividers $ liftaux p (zipWith prep factors (products factors) )

  where --factors=map(map to_zp) $factorPol p 1(monic fp (map (:[]) f))
        --        where to_zp []= 0
        --              to_zp (a:_)=a
        factors=[[1,0,1,1],[1,3,0 P.-2],[1,0 P.-3,0 P.-2]]

        fp=finite p 1

        products [_]= [one]
        products (a:as)= let bs=products as in (head as*head bs:bs)

        prep::[Integer]->[Integer]->([Integer],[Integer],[Integer],[Integer],[Integer])
        prep g h=(trueRepr p (g*h), g, h, s/d, t/d) where (d,s,t)= eea euclid g h
 
        euclid@(Euclid zero one(==)(+)(-)(*)(/)deg div)=pol(integer`mod`p)
        liftaux n x| n>mign  = map (\(_,g,_,_,_)->g) x
                   | otherwise = liftaux (n P.*n) $ map (henselLift n) x
        mign=mignotteBound p f
        p=primeMod f

        dividers []=[one]
        dividers (x:xs)= [(.*) (pol(integer`mod`(49 P.*49)))x ys| x<-[[1],x], ys<-dividers xs]
                          
