module TypeFun.Data.Peano where

import Data.Typeable
import GHC.Generics (Generic)
import GHC.TypeLits

data N = Z | S N
         deriving ( Eq, Ord, Read, Show
                  , Generic, Typeable )

type family ToNat (a :: N) :: Nat where
  ToNat Z = 0
  ToNat (S a) = 1 + (ToNat a)

type family FromNat (a :: Nat) :: N where
  FromNat 0 = Z
  FromNat a = S (FromNat (a - 1))

type family (:+:) (a :: N) (b :: N) :: N where
  Z :+: b = b
  (S a) :+: b = a :+: (S b)

type family (:-:) (a :: N) (b :: N) :: N where
  a :-: Z = a
  (S a) :-: (S b) = a :-: b

type family (:*:) (a :: N) (b :: N) :: N where
  Z :*: b = Z
  a :*: Z = Z
  (S a) :*: b = b :+: (a :*: b)
