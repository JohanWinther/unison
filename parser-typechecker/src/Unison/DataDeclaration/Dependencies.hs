module Unison.DataDeclaration.Dependencies (labeledDeclDependenciesIncludingSelfAndFieldAccessors) where

import Control.Lens
import Data.Map qualified as Map
import Data.Set.Lens (setOf)
import U.Codebase.Reference qualified as V2Reference
import Unison.DataDeclaration qualified as DD
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.TypeLookup (TypeLookup (..))
import Unison.Typechecker.TypeLookup qualified as TypeLookup
import Unison.Var qualified as Var

-- | Generate the LabeledDependencies for everything in a Decl, including the Decl itself, all
-- its constructors, all referenced types, and all possible record accessors.
--
-- Note that we can't actually tell whether the Decl was originally a record or not, so we
-- include all possible accessors, but they may or may not exist in the codebase.
labeledDeclDependenciesIncludingSelfAndFieldAccessors :: Var.Var v => V2Reference.TypeReference -> (DD.Decl v a) -> Set LD.LabeledDependency
labeledDeclDependenciesIncludingSelfAndFieldAccessors selfRef decl =
  DD.labeledDeclDependenciesIncludingSelf selfRef decl
    <> case decl of
      Left _effect -> mempty
      Right dataDecl -> accessorDependencies selfRef dataDecl

-- | Generate the Referents for field accessors of a Decl, which may or may not exist in the codebase
-- depending on whether the original type was defined as a Record.
accessorDependencies :: forall v a. (Var.Var v) => Reference -> DD.DataDeclaration v a -> Set LD.LabeledDependency
accessorDependencies declRef dd = fromMaybe mempty $ do
  -- This ppe is only used for typechecking errors.
  let ppe = PPE.empty
  typ <- case DD.constructors dd of
    [(_, typ)] -> Just typ
    _ -> Nothing
  -- These names are arbitrary and don't show up anywhere.
  let vars :: [v]
      vars = [Var.freshenId (fromIntegral n) (Var.named "_") | n <- [0 .. Type.arity typ - 1]]
  -- This name isn't important, we just need a name to generate field names from.
  -- The field names are thrown away afterwards.
  let typeName = Var.named "Type"
  let accessors :: [(v, (), Term.Term v ())]
      accessors = DD.generateRecordAccessors (map (,()) vars) typeName declRef
  let typeLookup :: TypeLookup v ()
      typeLookup =
        TypeLookup
          { TypeLookup.typeOfTerms = mempty,
            TypeLookup.dataDecls = Map.singleton declRef (void dd),
            TypeLookup.effectDecls = mempty
          }
  let typecheckingEnv :: Typechecker.Env v ()
      typecheckingEnv =
        Typechecker.Env
          { Typechecker._ambientAbilities = mempty,
            Typechecker._typeLookup = typeLookup,
            Typechecker._termsByShortname = mempty
          }
  accessorsWithTypes :: [(v, Term.Term v (), Type.Type v ())] <-
    for accessors \(v, _a, trm) ->
      case Result.result (Typechecker.synthesize ppe Typechecker.PatternMatchCoverageCheckSwitch'Disabled typecheckingEnv trm) of
        Nothing -> Nothing
        -- Note: Typechecker.synthesize doesn't normalize the output
        -- type. We do so here using `Type.cleanup`, mirroring what's
        -- done when typechecking a whole file and ensuring we get the
        -- same inferred type.
        Just typ -> Just (v, trm, Type.cleanup typ)
  let hashes =
        Hashing.hashTermComponents (Map.fromList . fmap (\(v, trm, typ) -> (v, (trm, typ, ()))) $ accessorsWithTypes)
          & Map.elems
          & setOf (folded . _1 . to (Reference.DerivedId >>> Referent.Ref >>> LD.TermReferent))
  pure hashes
