# church
Exercise in Church encodings.
Defines a `Church` type class for data types that can be Church encoded.

```haskell
instance Church Int where
  newtype C Int = CNat (forall r. (r -> r) -> r -> r)
  fromChurch (CNat cn) = cn (+1) 0
  toChurch 0 = CNat $ \f z -> z
  toChurch n = CNat $ \f z -> let CNat x = toChurch (n-1)
                               in f (x f z)

instance Num (C Int) where
  CNat cn1 + CNat cn2 = CNat $ \f -> cn1 f . cn2 f
  CNat cn1 * CNat cn2 = CNat $ cn1 . cn2
  (-) = undefined
  abs = id
  signum _ = 1
  fromInteger = toChurch . fromInteger
```
