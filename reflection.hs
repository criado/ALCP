{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, KindSignatures #-}
import Unsafe.Coerce

data Proxy k = Proxy

class Reifies s a | s -> a where
  reflect :: proxy s -> a

newtype Magic a r = Magic (forall (s :: *). Reifies s a => Proxy s -> r)

reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
reify a k = unsafeCoerce (Magic k :: Magic a r) (const a) Proxy
{-# INLINE reify #-}
