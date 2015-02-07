module Numbers where

import Definitions
import Prelude hiding((+),(-),(*),(/),div,mod)
import qualified Prelude as P

𝕫=Euclid {
 _zero=0::Integer,
 _one =1::Integer,
 (.==)=(P.==),
 (.+)=(P.+),
 (.-)=(P.-),
 (.*)=(P.*),
 _deg= P.id,
 _division= \a b->(P.div a b, P.mod a b)
}

integer=𝕫
