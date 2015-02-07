module FiniteField where

#include "Header.hs"
import Quotient
import Polynomial
import Numbers
import Data.List.Ordered

finite p n=assert (member p primes) $
            (𝕫 `mod` p) `mod`  

enumPol p (-1)=[]
enumPol p n=[c:q|c<-[0..p-1],q<-enumPol p (n-1)]

enumPolMonic p n=[1:q|q<-enumPol p n]

isIrreductible::Field f->[f]->Bool
isIrreductible _ []=True
isIrreductible _ [a]=True
isIrreductible _ [a,b]=True





