# church
Exercise in Church encodings.
Defines a `Church` data family + type class for data types that can be Church encoded.

e.g.
```haskell
instance Church Word where
  newtype C Word = CNat (forall r. (r -> r) -> r -> r)
  fromChurch (CNat cn) = cn (+1) 0
  toChurch 0 = CNat $ \f z -> z
  toChurch n = CNat $ \f z -> let CNat n' = toChurch (n-1)
                               in f (n' f z)

instance Num (C Word) where
  CNat cn1 + CNat cn2 = CNat $ \f -> cn1 f . cn2 f
  CNat cn1 * CNat cn2 = CNat $ cn1 . cn2
  abs = id
  signum _ = 1
  fromInteger = toChurch . fromInteger
```
