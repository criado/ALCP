module Numbers where

import Definitions

𝕫=Euclid {
 _zero=0::Integer,
 _one =1::Integer,
 (.+)=(+),
 (.-)=(-),
 (.*)=(*),
 _deg= \a->a,
 _division= \a b->(div a b, mod a b)
} 
