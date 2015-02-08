module Numbers where

#include "Header.hs"

𝕫=Euclid {
 _zero=0::Integer,
 _one =1::Integer,
 (.==)=(P.==),
 (.+)=(P.+),
 (.-)=(P.-),
 (.*)=(P.*),
 (./)= \n m->assert(m P.==1||m P.== -1) n P.*m,
 _deg= P.id,
 _div= \a b->(P.div a b, P.mod a b)
}

integer=𝕫
