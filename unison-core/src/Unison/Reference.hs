module Unison.Reference
  ( Reference,
    Reference'
      ( ReferenceBuiltin,
        ReferenceDerived,
        Derived
      ),
    Id,
    Id' (..),
    Pos,
    CycleSize,
    Size,
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
    componentFor,
    componentFromLength,
    unsafeFromText,
    isPrefixOf,
    fromText,
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

import U.Codebase.Reference
  ( CycleSize,
    Id,
    Id' (..),
    Reference,
    Reference' (..),
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
    componentFor,
    componentFromLength,
    fromId,
    fromText,
    idToHash,
    idToShortHash,
    idToText,
    isBuiltin,
    isPrefixOf,
    toId,
    toShortHash,
    toText,
    unsafeId,
    pattern Derived,
  )
import Unison.Hash qualified as H
import Unison.Prelude

type Pos = Word64

type Size = CycleSize

unsafeFromText :: Text -> Reference
unsafeFromText = either error id . fromText

toHash :: Reference -> Maybe H.Hash
toHash = fmap idToHash . toId
