module Numbers where

import Definitions

pepito=2

𝕫=Euclid {
 _zero=0::Integer,
 _one =1::Integer,
 (.==)=(==),
 (.+)=(+),
 (.-)=(-),
 (.*)=(*),
 _deg= id,
 _division= \a b->(div a b, mod a b)
}

integer=𝕫
