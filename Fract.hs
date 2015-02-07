module Fract where

import Definitions
import Numbers

fract::Dictionary d->Dictionary (d,d)
fract field=Field{
  _zero=(zero,one),
  _one=(one,one),
  (.+)= \(a,b) (c,d)->(a*d+c*b,b*d),
  (.-)= \(a,b) (c,d)->(a*d-c*b,b*d),
  (.*)= \(a,b) (c,d)->(a*b,c*d),
  (.==)= \(a,b) (c,d)->(a*d)==(b*c),
  (./)= \(a,b) (c,d)->(a*d,b*c)
 } where Field zero one (==) (+) (-) (*) (/)=field

𝕢=fract 𝕫
rational=𝕢
