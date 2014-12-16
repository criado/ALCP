import definitions
module Fp where

--Mirar GADT y dependent types
newtype Fp = Fp Integer Integer

instance Group Fp where
 (Fp mod n1) *: (Fp mod n2) = (j
 unit = Fp 1 
 addinverse (Fp mod n) = Fp mod -n

instance Ring(Fp n) where
 (Fp n1) +: (Fp n2) = mod (n1+:n2) value
 zero = Fp 0

instance Field(Fp n) where
 inverse (Fp n1)=
