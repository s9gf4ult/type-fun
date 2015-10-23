-- | Collection of type families on type lists. This module differs
-- from __Data.Singletons.Prelude.List__ because it polykinded and
-- works with __[*]__ as well

module TypeFun.Data.List where

import Data.Type.Bool
import GHC.Exts
import GHC.TypeLits
import TypeFun.Data.Eq
import TypeFun.Data.List.Errors
import TypeFun.Data.Maybe

type family Length (a :: [k]) :: Nat where
  Length '[] = 0
  Length (a ': as) = 1 + (Length as)

-- | Generates unresolvable constraint if fists element is not
-- contained inside of second
type Elem a s = NothingToConstr (IndexOf a s)
                                (ElementNotFoundInList a s)

-- | Reverse of 'Elem'
type NotElem a s = JustToConstr (IndexOf a s)
                                (ElementIsInList a s)

-- | Constanints that first argument is a sublist of second. Reduces
-- to __(Elem a1 b, Elem a2 b, Elem a3 b, ...)__
type family SubList (a :: [k]) (b :: [k]) :: Constraint where
  SubList '[]       bs = ()
  SubList (a ': as) bs = (Elem a bs, SubList as bs)

type family NotSubList (a :: [k]) (b :: [k]) :: Constraint where
  NotSubList '[]       bs = ()
  NotSubList (a ': as) bs = (NotElem a bs, NotSubList as bs)

type IndexOf a s = IndexOf' 0 a s

type family IndexOf' (acc :: Nat) (a :: k) (s :: [k]) :: Maybe Nat where
  IndexOf' acc a '[]       = 'Nothing
  IndexOf' acc a (a ': as) = 'Just acc
  IndexOf' acc a (b ': as) = IndexOf' (acc + 1) a as

type IsPrefixOf a b = ( If (IsPrefixOfBool a b)
                           (() :: Constraint)
                           (ListIsNotPrefixOf a b)
                      , SubList a b )

type IsNotPrefixOf a b = If (IsPrefixOfBool a b)
                            (ListIsPrefixOf a b)
                            (() :: Constraint)

-- | First argument is prefix of second
type family IsPrefixOfBool (a :: [k]) (b :: [k]) :: Bool where
  IsPrefixOfBool '[]       b         = 'True
  IsPrefixOfBool (a ': as) (a ': bs) = IsPrefixOfBool as bs
  IsPrefixOfBool as        bs        = 'False

type ElementIsUniq a s = If (Equal 1 (Count a s))
                            (() :: Constraint)
                            (ElementIsNotUniqInList a s)

type family Count (a :: k) (s :: [k]) :: Nat where
  Count a '[]       = 0
  Count a (a ': as) = 1 + (Count a as)
  Count a (b ': as) = Count a as

type UniqElements a = UniqElements' a a

type family UniqElements' (a :: [k]) (self :: [k]) :: Constraint where
  UniqElements' '[]       self = ()
  UniqElements' (a ': as) self = (ElementIsUniq a self, UniqElements' as self)
