{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

class Church t where
  data C t
  toChurch :: t -> C t
  fromChurch :: C t -> t

instance (Church a, Show a) => Show (C a) where
  show = show . fromChurch

cTrue'  a b = a
cFalse' a b = b

instance Church Bool where
  newtype C Bool = CBool (forall r. r -> r -> r)
  toChurch True = CBool cTrue'
  toChurch False = CBool cFalse'
  fromChurch (CBool cb) = cb True False

cNot (CBool cb) = CBool (flip cb)
cAnd (CBool cb1) (CBool cb2) = CBool (cb1 cb2 cFalse')
cOr  (CBool cb1) (CBool cb2) = CBool (cb1 cTrue' cb2)

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

instance Church (a,b) where
  newtype C (a,b) = CTuple (forall r. (a -> b -> r) -> r)
  fromChurch (CTuple ct) = ct (,)
  toChurch (a,b) = CTuple $ \f -> f a b

instance Church (Either a b) where
  newtype C (Either a b) = CEither (forall r. (a -> r) -> (b -> r) -> r)
  fromChurch (CEither ce) = ce Left Right
  toChurch (Left l)  = CEither $ \fl _ -> fl l
  toChurch (Right r) = CEither $ \_ fr -> fr r

instance Church [a] where
  newtype C [a] = CList (forall r. (a -> r -> r) -> r -> r)
  fromChurch (CList cl) = cl (:) []
  toChurch []     = CList $ \_ z -> z
  toChurch (x:xs) = CList $ \f z -> let CList l' = toChurch xs in f x (l' f z)

main :: IO ()
main = print $ (3 + 1 :: C Int)
