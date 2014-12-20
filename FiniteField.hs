module FiniteField where

import Definitions
import Quotient
import Polynomial
import Numbers

finite p n=(𝕫 `mod` p) `mod` (irreductible!!p!!n)

enumPol p (-1)=[]
enumPol p n=[c:q|c<-[0..p-1],q<-enumPol p (n-1)]

enumPolMonic p n=[1:q|q<-enumPol p n]

isIrreductible::


