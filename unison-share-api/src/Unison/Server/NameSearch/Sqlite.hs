module Unison.Server.NameSearch.Sqlite
  ( resolveShortHash,
    typeReferencesByShortHash,
    termReferentsByShortHash,
    NameSearch (..),
    scopedNameSearch,
  )
where

import Control.Lens
import Data.Set qualified as Set
import U.Codebase.HashTags (BranchHash)
import U.Codebase.Sqlite.NameLookups (PathSegments (PathSegments), ReversedName (..))
import U.Codebase.Sqlite.NamedRef qualified as NamedRef
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Builtin qualified as Builtin
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Cv
import Unison.HashQualified' qualified as HQ'
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.NameSearch (NameSearch (..), Search (..))
import Unison.Server.SearchResult qualified as SR
import Unison.ShortHash qualified as SH
import Unison.Sqlite qualified as Sqlite
import Unison.Util.Set qualified as Set

data SearchStrategy
  = ExactMatch
  | SuffixMatch
  deriving (Show, Eq)

scopedNameSearch :: Codebase m v a -> BranchHash -> Path -> Sqlite.Transaction (NameSearch Sqlite.Transaction)
scopedNameSearch codebase rootHash path = do
  -- We compute the names perspective once in advance so we don't need to calculate it on
  -- every query.
  namesPerspective <- Ops.nameLookupForPerspective rootHash pathSegments
  pure $ NameSearch {typeSearch = mkTypeSearch namesPerspective, termSearch = mkTermSearch namesPerspective}
  where
    mkTypeSearch namesPerspective =
      Search
        { lookupNames = lookupNamesForTypes namesPerspective,
          lookupRelativeHQRefs' = lookupRelativeHQRefsForTypes namesPerspective,
          makeResult = \hqname r names -> pure $ SR.typeResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReference
        }
    mkTermSearch namesPerspective =
      Search
        { lookupNames = lookupNamesForTerms namesPerspective,
          lookupRelativeHQRefs' = lookupRelativeHQRefsForTerms namesPerspective,
          makeResult = \hqname r names -> pure $ SR.termResult hqname r names,
          matchesNamedRef = HQ'.matchesNamedReferent
        }

    pathSegments :: PathSegments
    pathSegments = coerce $ Path.toList path
    lookupNamesForTypes :: Ops.NamesPerspective -> Reference -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTypes namesPerspective ref = do
      names <- Ops.typeNamesForRefWithinNamespace namesPerspective (Cv.reference1to2 ref) Nothing
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Reference.toShortHash ref))
        & Set.fromList
        & pure
    lookupNamesForTerms :: Ops.NamesPerspective -> Referent -> Sqlite.Transaction (Set (HQ'.HashQualified Name))
    lookupNamesForTerms namesPerspective ref = do
      names <- Ops.termNamesForRefWithinNamespace namesPerspective (Cv.referent1to2 ref) Nothing
      names
        & fmap (\segments -> HQ'.HashQualified (reversedSegmentsToName segments) (Referent.toShortHash ref))
        & Set.fromList
        & pure
    -- This is a bit messy, but the existing 'lookupRelativeHQRefs' semantics
    -- will return ONLY exact matches if any exist, otherwise it falls back on
    -- suffix search, so we maintain that behaviour here. It would probably be better
    -- to have separate functions in the Search type for each of these, and be more explicit
    -- about desired behaviour at the call-site.
    lookupRelativeHQRefsForTerms :: Ops.NamesPerspective -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
    lookupRelativeHQRefsForTerms namesPerspective hqName = do
      exact <- hqTermSearch namesPerspective ExactMatch hqName
      if Set.null exact
        then do
          hqTermSearch namesPerspective SuffixMatch hqName
        else do
          pure exact
    lookupRelativeHQRefsForTypes :: Ops.NamesPerspective -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
    lookupRelativeHQRefsForTypes namesPerspective hqName = do
      exact <- hqTypeSearch namesPerspective ExactMatch hqName
      if Set.null exact
        then do
          hqTypeSearch namesPerspective SuffixMatch hqName
        else do
          pure exact
    -- Search the codebase for matches to the given hq name.
    -- Supports either an exact match or a suffix match.
    hqTermSearch :: Ops.NamesPerspective -> SearchStrategy -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Referent)
    hqTermSearch namesPerspective searchStrat hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          namedRefs <-
            case searchStrat of
              ExactMatch -> Ops.termRefsForExactName rootHash (coerce $ Name.reverseSegments fqn)
              SuffixMatch -> Ops.termNamesBySuffix rootHash pathSegments (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap
              ( \(NamedRef.ref -> (ref, mayCT)) ->
                  Cv.referent2to1UsingCT (fromMaybe (error "Required constructor type for constructor but it was null") mayCT) ref
              )
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          termRefs <- termReferentsByShortHash codebase sh
          Set.forMaybe termRefs \termRef -> do
            matches <- Ops.termNamesForRefWithinNamespace namesPerspective (Cv.referent1to2 termRef) (Just . coerce $ Name.reverseSegments name)
            -- Return a valid ref if at least one match was found. Require that it be an exact
            -- match if specified.
            if any (\n -> coerce (Name.reverseSegments fqn) == n || searchStrat /= ExactMatch) matches
              then pure (Just termRef)
              else pure Nothing

    -- Search the codebase for matches to the given hq name.
    -- Supports either an exact match or a suffix match.
    hqTypeSearch :: Ops.NamesPerspective -> SearchStrategy -> HQ'.HashQualified Name -> Sqlite.Transaction (Set Reference)
    hqTypeSearch namesPerspective searchStrat hqName = do
      case hqName of
        HQ'.NameOnly name -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          namedRefs <-
            case searchStrat of
              ExactMatch -> Ops.typeRefsForExactName rootHash (coerce $ Name.reverseSegments fqn)
              SuffixMatch -> Ops.typeNamesBySuffix rootHash pathSegments (coerce $ Name.reverseSegments name)
          namedRefs
            & fmap (Cv.reference2to1 . NamedRef.ref)
            & Set.fromList
            & pure
        HQ'.HashQualified name sh -> do
          let fqn = Path.prefixName (Path.Absolute path) name
          typeRefs <- typeReferencesByShortHash sh
          Set.forMaybe typeRefs \typeRef -> do
            matches <- Ops.typeNamesForRefWithinNamespace namesPerspective (Cv.reference1to2 typeRef) (Just . coerce $ Name.reverseSegments name)
            -- Return a valid ref if at least one match was found. Require that it be an exact
            -- match if specified.
            if any (\n -> coerce (Name.reverseSegments fqn) == n || searchStrat /= ExactMatch) matches
              then pure (Just typeRef)
              else pure Nothing

    reversedSegmentsToName :: ReversedName -> Name
    reversedSegmentsToName = Name.fromReverseSegments . coerce

-- | Look up types in the codebase by short hash, and include builtins.
typeReferencesByShortHash :: SH.ShortHash -> Sqlite.Transaction (Set Reference)
typeReferencesByShortHash sh = do
  fromCodebase <- Codebase.typeReferencesByPrefix sh
  let fromBuiltins =
        Set.filter
          (\r -> sh == Reference.toShortHash r)
          Builtin.intrinsicTypeReferences
  pure (fromBuiltins <> Set.map Reference.DerivedId fromCodebase)

-- | Look up terms in the codebase by short hash, and include builtins.
termReferentsByShortHash :: Codebase m v a -> SH.ShortHash -> Sqlite.Transaction (Set Referent)
termReferentsByShortHash codebase sh = do
  fromCodebase <- Codebase.termReferentsByPrefix codebase sh
  let fromBuiltins =
        Set.map Referent.Ref $
          Set.filter
            (\r -> sh == Reference.toShortHash r)
            Builtin.intrinsicTermReferences
  pure (fromBuiltins <> Set.mapMonotonic (over Referent.reference_ Reference.DerivedId) fromCodebase)

-- | Resolves a shorthash into any possible matches.
resolveShortHash :: Codebase m v a -> SH.ShortHash -> Sqlite.Transaction (Set LD.LabeledDependency)
resolveShortHash codebase sh = do
  terms <- Set.map LD.TermReferent <$> termReferentsByShortHash codebase sh
  types <- Set.map LD.TypeReference <$> typeReferencesByShortHash sh
  pure $ terms <> types
