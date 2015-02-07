module FiniteField where

import Definitions
import Quotient
import Polynomial
import Numbers

finite p n=pol (𝕫 `mod` p) `mod` (irreductible!!fromJust(findIndex p primes)!!n)

enumPol p (-1)=[]
enumPol p n=[c:q|c<-[0..p-1],q<-enumPol p (n-1)]

enumPolMonic p n=[1:q|q<-enumPol p n]

isIrreductible::Field f->[f]->Bool
isIrreductible _ []=True
isIrreductible _ [a]=True
isIrreductible _ [a,b]=True


