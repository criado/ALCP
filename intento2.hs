{-# LANGUAGE Rank2Types #-}

data Ring a= Ring {
 suma :: a->a->a,
 uno :: a }

cociente::(Ring a)->a->(Ring a)
cociente (Ring suma uno) q =Ring (flip suma) (suma uno uno)
