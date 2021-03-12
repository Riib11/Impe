module Data.Namespace where

import Control.Applicative
import Control.Lens hiding ((<|))
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Text.Printf
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
data UID n = UID n Int
  deriving (Eq)

instance Show n => Show (UID n) where
  show (UID n i) = printf "%s#%s" (show n) (show i)

instance Ord n => Ord (UID n) where
  UID n1 i1 <= UID n2 i2 = n1 <= n2 && (n1 /= n2 || i1 <= i2)

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
  -- f :: a -> f a
  ix n f nsp =
    case lookupWithUID n nsp of
      Just (a, uid) -> f a <&> \a' -> nsp & store %~ replaceStore uid a'
      Nothing -> nsp & pure
  {-# INLINE ix #-}

instance Ord n => At (Namespace n a) where
  at n f nsp = case lookupScope n (nsp ^. scope) of
    Just uid ->
      f (lookup n nsp) <&> \case
        Just a' -> nsp & store %~ replaceStore uid a'
        Nothing -> nsp & delete n
    Nothing ->
      f (lookup n nsp) <&> \case
        Just a' -> nsp & initialize n a'
        Nothing -> nsp

{-
## Interface

analytic:
- initialize
- delete
- lookup
- alter -- (Maybe a -> Maybe a)

synthetic:
- update -- (a -> Maybe a)
- adapt -- (Maybe a -> a)
- adjust -- (a -> a)
- replace -- a

-}

-- initialize

initialize :: Ord n => n -> a -> Namespace n a -> Namespace n a
initialize n a nsp =
  let (nsp', uid) = newUID n nsp
   in nsp'
        & (scope %~ initializeScope n uid)
          . (store %~ initializeStore uid a)

initializeScope :: Ord n => n -> UID n -> Scope n -> Scope n
initializeScope n uid (m :| scp) = Map.insert n uid m :| scp

initializeStore :: Ord n => UID n -> a -> Store n a -> Store n a
initializeStore uid a str = Map.insert uid a str

newUID :: n -> Namespace n a -> (Namespace n a, UID n)
newUID n nsp = (nsp & counter %~ succ, UID n (nsp ^. counter))

-- delete

delete :: Ord n => n -> Namespace n a -> Namespace n a
delete n nsp = case lookupScope n (nsp ^. scope) of
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

deleteUIDs :: Ord n => [UID n] -> Namespace n a -> Namespace n a
deleteUIDs uids nsp = foldl (flip deleteUID) nsp uids

-- lookup

lookup :: Ord n => n -> Namespace n a -> Maybe a
lookup n nsp = do
  uid <- lookupScope n (nsp ^. scope)
  Map.lookup uid (nsp ^. store)

lookupUID :: Ord n => n -> Namespace n a -> Maybe (UID n)
lookupUID n nsp = lookupScope n (nsp ^. scope)

lookupWithUID :: Ord n => n -> Namespace n a -> Maybe (a, UID n)
lookupWithUID n nsp = do
  a <- lookup n nsp
  uid <- lookupUID n nsp
  return (a, uid)

lookupScope :: Ord n => n -> Scope n -> Maybe (UID n)
lookupScope n scp = foldl (<|>) Nothing . fmap (Map.lookup n) $ scp

lookupStore :: Ord n => UID n -> Store n a -> Maybe a
lookupStore uid str = Map.lookup uid str

-- alter

alter :: Ord n => n -> (Maybe a -> Maybe a) -> Namespace n a -> Namespace n a
alter n f nsp = case lookupScope n (nsp ^. scope) of
  Just uid -> case f (lookup n nsp) of
    Just a' -> nsp & store %~ replaceStore uid a'
    Nothing -> nsp & delete n
  Nothing ->
    case f (lookup n nsp) of
      Just a' -> nsp & initialize n a'
      Nothing -> nsp

-- update, adapt, adjust, replace

update :: Ord n => n -> (a -> Maybe a) -> Namespace n a -> Namespace n a
update n f = alter n (>>= f)

adapt :: Ord n => n -> (Maybe a -> a) -> Namespace n a -> Namespace n a
adapt n f = alter n (Just . f)

adjust :: Ord n => n -> (a -> a) -> Namespace n a -> Namespace n a
adjust n f = alter n (f <$>)

replace :: Ord n => n -> a -> Namespace n a -> Namespace n a
replace n a = alter n (\_ -> Just a)

adaptStore :: Ord n => UID n -> (a -> Maybe a) -> Store n a -> Store n a
adaptStore uid f str = case lookupStore uid str of
  Just a -> case f a of
    Just a' -> str & Map.adjust (\_ -> a') uid
    Nothing -> str & Map.delete uid
  Nothing -> str

replaceStore :: Ord n => UID n -> a -> Store n a -> Store n a
replaceStore uid a = adaptStore uid (\_ -> Just a)

{-
## Utilities
-}

-- enter a new (empty) local scope
enterLocalScope :: Ord n => Namespace n a -> Namespace n a
enterLocalScope nsp = nsp & scope %~ (mempty <|)

-- leave the local scope and garbagecollect its references
leaveLocalScope :: Ord n => Namespace n a -> Namespace n a
leaveLocalScope nsp = case nsp ^. scope of
  _ :| [] -> nsp & scope .~ mempty :| mempty
  m :| (m' : scp) ->
    nsp
      & deleteUIDs (Map.elems m) -- garbagecollect
        . (scope .~ m' :| scp)

-- adds the given scope on top of current scope
recallScope :: Ord n => Scope n -> Namespace n a -> Namespace n a
recallScope scp nsp = nsp & scope %~ (scp <>)

-- removes `n` scope levels from local scope, without garbagecollection,
-- where `n` is the depth of the given scope
-- TODO: danger- assumes `scp` is at top of scope
-- TODO: danger- assumes not called down to bottom scope
forgetScope :: Ord n => Scope n -> Namespace n a -> Namespace n a
forgetScope scp nsp =
  nsp & scope %~ NonEmpty.fromList . NonEmpty.drop (NonEmpty.length scp)

isVisible :: Ord n => n -> Namespace n a -> Bool
isVisible n nsp = isJust $ lookup n nsp

isReferencedFromScope :: Ord n => UID n -> Scope n -> Bool
isReferencedFromScope uid scp = foldl f False scp
  where
    f b m = b || uid `elem` Map.elems m

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

-- TODO: doesn't work...
-- TODO: or perhaps it just doesnt work due to how leaveLocalScope is used?
garbagecollect :: Ord n => Namespace n a -> Namespace n a
garbagecollect nsp = foldl (flip deleteUID) nsp (invisibleUIDs nsp)
