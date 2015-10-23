-- | Collection of type families on type lists. This module differs
-- from __Data.Singletons.Prelude.List__ because it polykinded and
-- works with __[*]__ as well

module TypeFun.Data.List where

import Data.Type.Bool
import GHC.Exts
import GHC.TypeLits
import TypeFun.Data.List.Internal
import TypeFun.Data.Maybe

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

-- type family SubListBool (a :: [k]) (b :: [k]) :: Bool where
--   SubListBool '[]       bs = 'True
--   SubListBool (a ': as) bs = (ElemBool a bs) && (SubListBool as bs)


-- type family UniqElemsBool (a :: [k]) :: Bool where
--   UniqElemsBool

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
