{-# LANGUAGE DataKinds #-}

module Unison.Reference
  ( Reference,
    pattern Builtin,
    pattern Derived,
    pattern DerivedId,
    _DerivedId,
    Id (..),
    Pos,
    CycleSize,
    Size,
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
    derivedBase32Hex,
    component,
    components,
    groupByComponent,
    componentFor,
    componentFromLength,
    unsafeFromText,
    idFromText,
    isPrefixOf,
    fromShortHash,
    fromText,
    readSuffix,
    showShort,
    showSuffix,
    toHash,
    toId,
    fromId,
    toText,
    idToText,
    unsafeId,
    toShortHash,
    idToHash,
    idToShortHash,
    isBuiltin,
  )
where

import Control.Lens (Prism')
import Data.Char (isDigit)
import Data.Generics.Sum (_Ctor)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Safe.Partial (Partial)
import Unison.Hash qualified as H
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH

-- | Either a builtin or a user defined (hashed) top-level declaration.
--
-- Used for both terms and types. Doesn't distinguish between them.
--
-- Other used defined things like local variables don't get @Reference@s.
data Reference
  = -- A builtin, e.g. (Builtin "Nat")
    Builtin Text.Text
  | -- `Derived` can be part of a strongly connected component.
    -- The `Pos` refers to a particular element of the component
    -- and the `Size` is the number of elements in the component.
    -- Using an ugly name so no one tempted to use this
    DerivedId Id
  deriving (Eq, Ord, Generic)

pattern Derived :: H.Hash -> Pos -> Reference
pattern Derived h i = DerivedId (Id h i)

{-# COMPLETE Builtin, Derived #-}

_DerivedId :: Prism' Reference Id
_DerivedId = _Ctor @"DerivedId"

isBuiltin :: Reference -> Bool
isBuiltin (Builtin _) = True
isBuiltin _ = False

-- | @Pos@ is a position into a cycle, as cycles are hashed together.
data Id = Id H.Hash Pos deriving (Eq, Ord)

-- | A term reference.
type TermReference = Reference

type TermReferenceId = Id

-- | A type declaration reference.
type TypeReference = Reference

type TypeReferenceId = Id

unsafeId :: Partial => Reference -> Id
unsafeId (Builtin b) =
  error $ "Tried to get the hash of builtin " <> Text.unpack b <> "."
unsafeId (DerivedId x) = x

idToHash :: Id -> H.Hash
idToHash (Id h _) = h

idToShortHash :: Id -> ShortHash
idToShortHash = toShortHash . DerivedId

-- but Show Reference currently depends on SH
toShortHash :: Reference -> ShortHash
toShortHash (Builtin b) = SH.Builtin b
toShortHash (Derived h 0) = SH.ShortHash (H.toBase32HexText h) Nothing Nothing
toShortHash (Derived h i) = SH.ShortHash (H.toBase32HexText h) (Just $ showSuffix i) Nothing

-- toShortHash . fromJust . fromShortHash == id and
-- fromJust . fromShortHash . toShortHash == id
-- but for arbitrary ShortHashes which may be broken at the wrong boundary, it
-- may not be possible to base32Hex decode them.  These will return Nothing.
-- Also, ShortHashes that include constructor ids will return Nothing;
-- try Referent.fromShortHash
fromShortHash :: ShortHash -> Maybe Reference
fromShortHash (SH.Builtin b) = Just (Builtin b)
fromShortHash (SH.ShortHash prefix cycle Nothing) = do
  h <- H.fromBase32HexText prefix
  case cycle of
    Nothing -> Just (Derived h 0)
    Just i -> Derived h <$> readMay (Text.unpack i)
fromShortHash (SH.ShortHash _prefix _cycle (Just _cid)) = Nothing

showSuffix :: Pos -> Text
showSuffix = Text.pack . show

readSuffix :: Text -> Either String Pos
readSuffix = \case
  pos
    | Text.all isDigit pos,
      Just pos' <- readMaybe (Text.unpack pos) ->
        Right pos'
  t -> Left $ "Invalid reference suffix: " <> show t

isPrefixOf :: ShortHash -> Reference -> Bool
isPrefixOf sh r = SH.isPrefixOf sh (toShortHash r)

toText :: Reference -> Text
toText = SH.toText . toShortHash

idToText :: Id -> Text
idToText = toText . DerivedId

showShort :: Int -> Reference -> Text
showShort numHashChars = SH.toText . SH.take numHashChars . toShortHash

type Pos = Word64

type Size = CycleSize

type CycleSize = Word64

-- enumerate the `a`s and associates them with corresponding `Reference.Id`s
componentFor :: H.Hash -> [a] -> [(Id, a)]
componentFor h as = [(Id h i, a) | (i, a) <- zip [0 ..] as]

componentFromLength :: H.Hash -> CycleSize -> Set Id
componentFromLength h size = Set.fromList [Id h i | i <- [0 .. size - 1]]

derivedBase32Hex :: Text -> Pos -> Maybe Reference
derivedBase32Hex b32Hex i = mayH <&> \h -> DerivedId (Id h i)
  where
    mayH = H.fromBase32HexText b32Hex

unsafeFromText :: Text -> Reference
unsafeFromText = either error id . fromText

idFromText :: Text -> Maybe Id
idFromText s = case fromText s of
  Left _ -> Nothing
  Right (Builtin _) -> Nothing
  Right (DerivedId id) -> pure id

toId :: Reference -> Maybe Id
toId (DerivedId id) = Just id
toId Builtin {} = Nothing

fromId :: Id -> Reference
fromId = DerivedId

toHash :: Reference -> Maybe H.Hash
toHash r = idToHash <$> toId r

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
  [_, "", b] -> Right (Builtin b)
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

component :: H.Hash -> [k] -> [(k, Id)]
component h ks =
  let
   in [(k, (Id h i)) | (k, i) <- ks `zip` [0 ..]]

components :: [(H.Hash, [k])] -> [(k, Id)]
components sccs = uncurry component =<< sccs

groupByComponent :: [(k, Reference)] -> [[(k, Reference)]]
groupByComponent refs = done $ foldl' insert Map.empty refs
  where
    insert m (k, r@(Derived h _)) =
      Map.unionWith (<>) m (Map.fromList [(Right h, [(k, r)])])
    insert m (k, r) =
      Map.unionWith (<>) m (Map.fromList [(Left r, [(k, r)])])
    done m = sortOn snd <$> toList m

instance Show Id where show = SH.toString . SH.take 5 . toShortHash . DerivedId

instance Show Reference where show = SH.toString . SH.take 5 . toShortHash
