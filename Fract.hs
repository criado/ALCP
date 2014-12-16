module Fract where

import Definitions

fract'field::Structure d->Structure (d,d)
fract'field field=Field{
  _zero=(zero,one),
  _one=(one,one),
  (.+)= \(a,b) (c,d)->(a*d+c*b,b*d),
  (.-)= \(a,b) (c,d)->(a*d-c*b,b*d),
  (.*)= \(a,b) (c,d)->(a*b,c*d),
  (./)= \(a,b) (c,d)->(a*d,b*c)
 } where one=_one field; zero=_zero field; (+)=(.+) field; (-)=(.-)field; (*)=(.*)field
