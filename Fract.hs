
newtype Fract r = F a b

instance Ring r => Eq (Fract r) where
 F a b == F c d = (a*:d) == (b*:c)

instance Ring r => Group (Fract r) where
 F a b + F c d = F (a*:d +: c*:b) (d*:b)
 unit = F unit unit
 addinverse (F a b) = F (addinverse a) b

instance Ring r => Ring (Fract r) where
 F a b * F c d = F (a*:c) (b*:d)
 zero= F zero unit

instance Ring r => Field (Fract r) where
 inverse (F a b) = F b a

 


