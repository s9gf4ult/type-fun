-- | Collection of type families on type lists. This module differs
-- from __Data.Singletons.Prelude.List__ because works with __[*]__
-- and relies on GHC's type equality (~)

module TypeFun.Data.List where

import Data.Type.Bool
import GHC.Exts
import GHC.TypeLits
import TypeFun.Data.Eq
import TypeFun.Data.List.Errors
import TypeFun.Data.Maybe
import TypeFun.Data.Peano

type family Length (a :: [k]) :: N where
  Length '[] = 'Z
  Length (a ': as) = 'S (Length as)

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

type IndexOf a s = IndexOf' Z a s

type family IndexOf' (acc :: N) (a :: k) (s :: [k]) :: Maybe N where
  IndexOf' acc a '[]       = 'Nothing
  IndexOf' acc a (a ': as) = 'Just acc
  IndexOf' acc a (b ': as) = IndexOf' (S acc) a as

type family Index (idx :: N) (s :: [k]) :: Maybe k where
  Index idx     '[]       = 'Nothing
  Index Z       (a ': as) = 'Just a
  Index (S idx) (a ': as) = Index idx as

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

-- | Checks that element 'a' occurs in a list just once
type ElementIsUniq a s = If (Equal (S Z) (Count a s))
                            (() :: Constraint)
                            (ElementIsNotUniqInList a s)

-- | Count elements in a list
type family Count (a :: k) (s :: [k]) :: N where
  Count a '[]       = 'Z
  Count a (a ': as) = 'S (Count a as)
  Count a (b ': as) = Count a as

-- | Checks that all elements in list are unique
type UniqElements a = UniqElements' a a

type family UniqElements' (a :: [k]) (self :: [k]) :: Constraint where
  UniqElements' '[]       self = ()
  UniqElements' (a ': as) self = (ElementIsUniq a self, UniqElements' as self)

type family Drop (c :: N) (s :: [k]) :: [k] where
  Drop 'Z     s         = s
  Drop ('S c) '[]       = '[]
  Drop ('S c) (a ': as) = Drop c as

type family Take (c :: N) (s :: [k]) :: [k] where
  Take 'Z     s         = '[]
  Take ('S c) '[]       = '[]
  Take ('S c) (a ': as) = a ': (Take c as)
