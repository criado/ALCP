module Numbers where

#include "Header.hs"

𝕫=Euclid {
 _zero=0::Integer,
 _one =1::Integer,
 (.==)=(==),
 (.+)=(+),
 (.-)=(-),
 (.*)=(*),
 (./)= \n m->assert(m ==1||m == -1) n *m,
 _deg= P.id,
 _div= \a b->(P.div a b, P.mod a b)
}

integer=𝕫
