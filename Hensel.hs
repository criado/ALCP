module Hensel  where
#include "Header.hs"
import Quotient
import Polynomials
import Numbers
import FiniteField

hensel::Integer->[Integer]->Integer->Integer->Integer->Integer
hensel p f r k m= --construye, dado r raiz de f en Z[x] mod (p^k)
                   -- una raíz de f en Z[x] mod (p^(k+m))

