module Data.Namespace where

import Control.Applicative
import Control.Lens hiding ((<|))
import Data.List.NonEmpty as NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Prelude hiding (lookup)

{-
# Namespace
-}

{-
## Data
-}

data Namespace n a = Namespace
  { _scope :: Scope n,
    _store :: Store n a,
    _counter :: Int
  }
  deriving (Show)

-- nonempty list of mappings from names to UIDs
type Scope n = NonEmpty (Map.Map n (UID n))

-- mapping from UIDs to values
type Store n a = Map.Map (UID n) a

-- unique identifier
type UID n = (n, Int)

makeLenses ''Namespace

{-
## Instances
-}

instance Ord n => Semigroup (Namespace n a) where
  -- overlap is overrided by right-side argument
  nsp1 <> nsp2 =
    Namespace
      { _scope = nsp1 ^. scope <> nsp2 ^. scope,
        _store = nsp1 ^. store <> nsp2 ^. store,
        _counter = max (nsp1 ^. counter) (nsp2 ^. counter)
      }

instance Ord n => Monoid (Namespace n a) where
  mempty =
    Namespace
      { _scope = mempty :| mempty,
        _store = mempty,
        _counter = 0
      }

type instance Index (Namespace n a) = n

type instance IxValue (Namespace n a) = a

instance Ord n => Ixed (Namespace n a) where
  ix n f nsp =
    case lookup n nsp of
      Just a -> f a <&> \a' -> insert n a' nsp
      Nothing -> pure nsp
  {-# INLINE ix #-}

instance Ord n => At (Namespace n a) where
  at n f nsp =
    f (lookup n nsp) <&> \r -> case r of
      Nothing -> maybe nsp (const (delete n nsp)) (lookup n nsp)
      Just a' -> insert n a' nsp

{-
## Interface
-}

newUID :: n -> Namespace n a -> (Namespace n a, UID n)
newUID n nsp = (nsp & counter %~ succ, (n, nsp ^. counter))

initialize :: Ord n => n -> a -> Namespace n a -> Namespace n a
initialize n a nsp =
  let (nsp', uid) = newUID n nsp
   in nsp'
        & (scope %~ insertIntoScope n uid)
          . (store %~ insertIntoStore uid a)

-- lookup

lookup :: Ord n => n -> Namespace n a -> Maybe a
lookup n nsp = do
  uid <- lookupFromScope n (nsp ^. scope)
  Map.lookup uid (nsp ^. store)

lookupFromScope :: Ord n => n -> Scope n -> Maybe (UID n)
lookupFromScope n scp = foldl (<|>) Nothing . fmap (Map.lookup n) $ scp

lookupFromStore :: Ord n => UID n -> Store n a -> Maybe a
lookupFromStore uid str = Map.lookup uid str

-- insert

insert :: Ord n => n -> a -> Namespace n a -> Namespace n a
insert n a nsp =
  case lookupFromScope n (nsp ^. scope) of
    Just uid ->
      nsp
        & (scope %~ insertIntoScope n uid)
          . (store %~ insertIntoStore uid a)
    Nothing ->
      -- let (nsp', uid) = newUID n nsp
      --  in nsp'
      --       & (scope %~ insertIntoScope n uid)
      --         . (store %~ insertIntoStore uid a)
      nsp

insertIntoScope :: Ord n => n -> UID n -> Scope n -> Scope n
insertIntoScope n uid (m :| scp) = Map.insert n uid m :| scp

insertIntoStore :: Ord n => UID n -> a -> Store n a -> Store n a
insertIntoStore uid a str = Map.insert uid a str

-- delete

delete :: Ord n => n -> Namespace n a -> Namespace n a
delete n nsp = case lookupFromScope n (nsp ^. scope) of
  Just uid ->
    nsp
      & (foldl (.) id)
        [ scope %~ deleteFromScope n,
          store %~ Map.delete uid
        ]
  Nothing -> nsp

deleteFromScope :: Ord n => n -> Scope n -> Scope n
deleteFromScope n scp = go scp
  where
    go (m :| []) = Map.delete n m :| []
    go (m :| (m' : scp')) =
      if n `elem` Map.keys m
        then Map.delete n m :| (m' : scp')
        else m <| go (m' :| scp')

-- use only when all references have gone out of scope
deleteUID :: Ord n => UID n -> Namespace n a -> Namespace n a
deleteUID uid nsp = nsp & store %~ Map.delete uid

{-
## Utilities
-}

enterScope :: Ord n => Namespace n a -> Namespace n a
enterScope nsp = nsp & scope %~ (mempty <|)

leaveScope :: Ord n => Namespace n a -> Namespace n a
leaveScope nsp = case nsp ^. scope of
  _ :| [] -> nsp & scope .~ mempty :| mempty -- cannot exit top scope
  _ :| (m : scp) -> nsp & scope .~ m :| scp

isVisible :: Ord n => n -> Namespace n a -> Bool
isVisible n nsp = isJust $ lookup n nsp

isReferencedFromScope :: Ord n => UID n -> Scope n -> Bool
isReferencedFromScope uid scp = foldl f False scp
  where
    f b m = if b then True else uid `elem` Map.elems m

visibleUIDs :: Ord n => Namespace n a -> [UID n]
visibleUIDs nsp = foldl f [] (nsp ^. scope)
  where
    f uids m = Map.elems m ++ uids

invisibleUIDs :: Ord n => Namespace n a -> [UID n]
invisibleUIDs nsp = foldl f [] (Map.keys (nsp ^. store))
  where
    f uids uid =
      ( if isReferencedFromScope uid (nsp ^. scope)
          then []
          else [uid]
      )
        ++ uids

garbagecollect :: Ord n => Namespace n a -> Namespace n a
garbagecollect nsp = foldl (flip deleteUID) nsp (invisibleUIDs nsp)
