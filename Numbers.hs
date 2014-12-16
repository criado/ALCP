module Numbers where

import Definitions

integer_euclid=Euclid {
 _zero=0::Integer,
 _one =1::Integer,
 (.+)=(+),
 (.-)=(-),
 (.*)=(*),
 _deg= \a->a,
 _division= \a b->(div a b, mod a b)
} 
