module TypeFun.Constraint where

import GHC.Exts

type family AllSatisfy (c :: k -> Constraint) (s :: [k]) :: Constraint where
  AllSatisfy c '[]       = ()
  AllSatisfy c (a ': as) = (c a, AllSatisfy c as)
