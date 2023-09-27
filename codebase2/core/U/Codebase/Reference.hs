{-# LANGUAGE DataKinds #-}

module U.Codebase.Reference
  ( Reference,
    RReference,
    TermReference,
    TermRReference,
    TermReferenceId,
    TypeReference,
    TypeRReference,
    TypeReferenceId,
    Reference' (..),
    pattern Derived,
    Id,
    RId,
    Id' (..),
    CycleSize,
    Pos,
    _ReferenceDerived,
    _RReferenceReference,
    t_,
    h_,
    idH,
    idToHash,
    idToRId,
    idToShortHash,
    idToText,
    isBuiltin,
    isPrefixOf,
    ridToId,
    rreferenceToReference,
    toId,
    toRReference,
    toShortHash,
    toText,
    unsafeId,
    fromId,
    fromText,
    componentFor,
    componentFromLength,
  )
where

import Control.Lens (Lens, Prism, Prism', Traversal, lens, preview, prism)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Char (isDigit)
import Data.Generics.Sum (_Ctor)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH

-- | This is the canonical representation of Reference
type Reference = Reference' Text Hash

-- | A possibly-self (R = "recursive") reference.
type RReference = Reference' Text (Maybe Hash)

-- | A term reference.
type TermReference = Reference

-- | A possibly-self term reference.
type TermRReference = RReference

-- | A type declaration reference.
type TypeReference = Reference

-- | A possibly-self type declaration reference.
type TypeRReference = RReference

-- | A reference id.
type Id = Id' Hash

-- | A possibl-self reference id.
type RId = Id' (Maybe Hash)

-- | A term reference id.
type TermReferenceId = Id

-- | A type declaration reference id.
type TypeReferenceId = Id

-- | Either a builtin or a user defined (hashed) top-level declaration. Used for both terms and types.
data Reference' t h
  = ReferenceBuiltin t
  | ReferenceDerived (Id' h)
  deriving stock (Eq, Generic, Ord, Show)

instance Bifunctor Reference' where
  bimap f _ (ReferenceBuiltin t) = ReferenceBuiltin (f t)
  bimap _ g (ReferenceDerived id) = ReferenceDerived (g <$> id)

instance Bifoldable Reference' where
  bifoldMap f _ (ReferenceBuiltin t) = f t
  bifoldMap _ g (ReferenceDerived id) = foldMap g id

instance Bitraversable Reference' where
  bitraverse f _ (ReferenceBuiltin t) = ReferenceBuiltin <$> f t
  bitraverse _ g (ReferenceDerived id) = ReferenceDerived <$> traverse g id

_RReferenceReference :: Prism' (Reference' t (Maybe h)) (Reference' t h)
_RReferenceReference = prism embed project
  where
    embed = \case
      ReferenceBuiltin x -> ReferenceBuiltin x
      ReferenceDerived (Id h p) -> ReferenceDerived (Id (Just h) p)

    project = \case
      ReferenceBuiltin x -> Right (ReferenceBuiltin x)
      ReferenceDerived (Id mh p) -> case mh of
        Nothing -> Left (ReferenceDerived (Id mh p))
        Just h -> Right (ReferenceDerived (Id h p))

_ReferenceDerived :: Prism (Reference' t h) (Reference' t h') (Id' h) (Id' h')
_ReferenceDerived =
  _Ctor @"ReferenceDerived"

pattern Derived :: h -> Pos -> Reference' t h
pattern Derived h i = ReferenceDerived (Id h i)

{-# COMPLETE ReferenceBuiltin, Derived #-}

type CycleSize = Word64

type Pos = Word64

-- | @Pos@ is a position into a cycle, as cycles are hashed together.
data Id' h = Id h Pos
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

t_ :: Traversal (Reference' t h) (Reference' t' h) t t'
t_ f = \case
  ReferenceBuiltin t -> ReferenceBuiltin <$> f t
  ReferenceDerived id -> pure (ReferenceDerived id)

h_ :: Traversal (Reference' t h) (Reference' t h') h h'
h_ f = \case
  ReferenceBuiltin t -> pure (ReferenceBuiltin t)
  Derived h i -> Derived <$> f h <*> pure i

idH :: Lens (Id' h) (Id' h') h h'
idH = lens (\(Id h _w) -> h) (\(Id _h w) h -> Id h w)

idToHash :: Id -> Hash
idToHash (Id h _) = h

idToRId :: Hash -> Id -> RId
idToRId h (Id h' i) = Id oh i
  where
    oh = if h == h' then Nothing else Just h'

idToShortHash :: Id -> ShortHash
idToShortHash = toShortHash . ReferenceDerived

idToText :: Id -> Text
idToText = toText . ReferenceDerived

isBuiltin :: Reference -> Bool
isBuiltin = \case
  ReferenceBuiltin _ -> True
  ReferenceDerived _ -> False

isPrefixOf :: ShortHash -> Reference -> Bool
isPrefixOf sh r = SH.isPrefixOf sh (toShortHash r)

ridToId :: Hash -> RId -> Id
ridToId h (Id oh i) =
  Id (fromMaybe h oh) i

rreferenceToReference :: Hash -> RReference -> Reference
rreferenceToReference h = \case
  ReferenceBuiltin t -> ReferenceBuiltin t
  ReferenceDerived i -> ReferenceDerived (ridToId h i)

toId :: Reference -> Maybe Id
toId =
  preview _ReferenceDerived

toRReference :: Hash -> Reference -> RReference
toRReference h = \case
  ReferenceBuiltin t -> ReferenceBuiltin t
  ReferenceDerived i -> ReferenceDerived (idToRId h i)

toShortHash :: Reference -> ShortHash
toShortHash = \case
  ReferenceBuiltin b -> SH.Builtin b
  ReferenceDerived (Id h 0) -> SH.ShortHash (Hash.toBase32HexText h) Nothing Nothing
  ReferenceDerived (Id h i) -> SH.ShortHash (Hash.toBase32HexText h) (Just i) Nothing

toText :: Reference -> Text
toText = SH.toText . toShortHash

unsafeId :: Reference -> Id
unsafeId = \case
  ReferenceBuiltin b -> error $ "Tried to get the hash of builtin " <> Text.unpack b <> "."
  ReferenceDerived x -> x

fromId :: Id -> Reference
fromId = ReferenceDerived

-- |
-- todo: take a (Reference -> CycleSize) so that `readSuffix` doesn't have to parse the size from the text.
-- examples:
--
-- builtins don’t have cycles
-- >>> fromText "##Text.take"
-- Right ##Text.take
--
-- derived, no cycle
-- >>> fromText "#dqp2oi4iderlrgp2h11sgkff6drk92omo4c84dncfhg9o0jn21cli4lhga72vlchmrb2jk0b3bdc2gie1l06sqdli8ego4q0akm3au8"
-- Right #dqp2o
--
-- derived, part of cycle
-- >>> fromText "#dqp2oi4iderlrgp2h11sgkff6drk92omo4c84dncfhg9o0jn21cli4lhga72vlchmrb2jk0b3bdc2gie1l06sqdli8ego4q0akm3au8.12345"
-- Right #dqp2o.12345
--
-- Errors with 'Left' on invalid hashes
-- >>> fromText "#invalid_hash.12345"
-- Left "Invalid hash: \"invalid_hash\""
fromText :: Text -> Either String Reference
fromText t = case Text.split (== '#') t of
  [_, "", b] -> Right (ReferenceBuiltin b)
  [_, h] -> case Text.split (== '.') h of
    [hash] ->
      case derivedBase32Hex hash 0 of
        Nothing -> Left $ "Invalid hash: " <> show hash
        Just r -> Right r
    [hash, suffix] -> do
      pos <- readSuffix suffix
      maybe (Left $ "Invalid hash: " <> show hash) Right (derivedBase32Hex hash pos)
    _ -> bail
  _ -> bail
  where
    bail = Left $ "couldn't parse a Reference from " <> Text.unpack t

    derivedBase32Hex :: Text -> Pos -> Maybe Reference
    derivedBase32Hex b32Hex i = mayH <&> \h -> Derived h i
      where
        mayH = Hash.fromBase32HexText b32Hex

    readSuffix :: Text -> Either String Pos
    readSuffix = \case
      pos
        | Text.all isDigit pos,
          Just pos' <- readMaybe (Text.unpack pos) ->
            Right pos'
      t -> Left $ "Invalid reference suffix: " <> show t

-- | enumerate the `a`s and associates them with corresponding `Reference.Id`s
componentFor :: Hash -> [a] -> [(Id, a)]
componentFor h as = [(Id h i, a) | (i, a) <- zip [0 ..] as]

componentFromLength :: Hash -> CycleSize -> Set Id
componentFromLength h size = Set.fromList [Id h i | i <- [0 .. size - 1]]
