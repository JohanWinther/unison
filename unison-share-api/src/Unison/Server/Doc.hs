{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Doc where

import Control.Lens (view, (^.))
import Control.Monad
import Data.Aeson (ToJSON)
import Data.Foldable
import Data.Functor
import qualified Data.Map as Map
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Word
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import qualified Unison.Builtin.Decls as Decls
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import qualified Unison.Codebase.Editor.DisplayObject as DO
import qualified Unison.ConstructorReference as ConstructorReference
import qualified Unison.DataDeclaration as DD
import qualified Unison.LabeledDependency as LD
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as DD
import Unison.Server.Orphans ()
import Unison.Server.Syntax (SyntaxText)
import qualified Unison.Server.Syntax as Syntax
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.DeclPrinter as DeclPrinter
import qualified Unison.Syntax.NamePrinter as NP
import qualified Unison.Syntax.TermPrinter as TermPrinter
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.List as List
import Unison.Util.Monoid (foldMapM)
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)

type Nat = Word64

type SSyntaxText = S.SyntaxText' Reference

-- | A doc rendered down to SyntaxText.
type Doc = DocG RenderedSpecialForm

-- | A doc which has been evaluated and includes all information necessary to be rendered.
type EvaluatedDoc v = DocG (EvaluatedSpecialForm v)

type SrcRefs = Ref (UnisonHash, DisplayObject SyntaxText Src)

-- | A doc parameterized by its special forms.
data DocG specialForm
  = Word Text
  | Code (DocG specialForm)
  | CodeBlock Text (DocG specialForm)
  | Bold (DocG specialForm)
  | Italic (DocG specialForm)
  | Strikethrough (DocG specialForm)
  | Style Text (DocG specialForm)
  | Anchor Text (DocG specialForm)
  | Blockquote (DocG specialForm)
  | Blankline
  | Linebreak
  | SectionBreak
  | Tooltip (DocG specialForm) (DocG specialForm)
  | Aside (DocG specialForm)
  | Callout (Maybe (DocG specialForm)) (DocG specialForm)
  | Table [[(DocG specialForm)]]
  | Folded Bool (DocG specialForm) (DocG specialForm)
  | Paragraph [(DocG specialForm)]
  | BulletedList [(DocG specialForm)]
  | NumberedList Nat [(DocG specialForm)]
  | Section (DocG specialForm) [(DocG specialForm)]
  | NamedLink (DocG specialForm) (DocG specialForm)
  | Image (DocG specialForm) (DocG specialForm) (Maybe (DocG specialForm))
  | Special specialForm
  | Join [(DocG specialForm)]
  | UntitledSection [(DocG specialForm)]
  | Column [(DocG specialForm)]
  | Group (DocG specialForm)
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (ToJSON, ToSchema)

type UnisonHash = Text

data Ref a = Term a | Type a
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (ToJSON)

instance ToSchema a => ToSchema (Ref a)

data MediaSource = MediaSource {mediaSourceUrl :: Text, mediaSourceMimeType :: Maybe Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data RenderedSpecialForm
  = Source [SrcRefs]
  | FoldedSource [SrcRefs]
  | Example SyntaxText
  | ExampleBlock SyntaxText
  | Link SyntaxText
  | Signature [SyntaxText]
  | SignatureInline
      SyntaxText
      Eval
      SyntaxText
      SyntaxText
      EvalInline
      SyntaxText
      SyntaxText
  | Embed SyntaxText
  | EmbedInline SyntaxText
  | Video [MediaSource] (Map Text Text)
  | FrontMatter (Map Text [Text])
  | RenderError (RenderError SyntaxText)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

data EvaluatedSpecialForm v
  = ESource [(EvaluatedSrc v)]
  | EFoldedSource [(EvaluatedSrc v)]
  | EExample (Term v ())
  | EExampleBlock (Term v ())
  | ELink (Either (Term v ()) LD.LabeledDependency)
  | ESignature [(Referent, Type v ())]
  | ESignatureInline (Referent, Type v ())
  | -- Result is Nothing if there was an Eval failure
    EEval (Term v ()) (Maybe (Term v ()))
  | -- Result is Nothing if there was an Eval failure
    EEvalInline (Term v ()) (Maybe (Term v ()))
  | EEmbed (Term v ())
  | EEmbedInline (Term v ())
  | EVideo [MediaSource] (Map Text Text)
  | EFrontMatter (Map Text [Text])
  | ERenderError (RenderError (Term v ()))
  deriving stock (Eq, Show, Generic)

-- `Src folded unfolded`
data Src = Src SyntaxText SyntaxText
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, ToSchema)

-- | Evaluate the doc, then render it.
evalAndRenderDoc ::
  forall v m.
  (Var v, Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term v ()))) ->
  (Referent -> m (Maybe (Type v ()))) ->
  (Term v () -> m (Maybe (Term v ()))) ->
  (Reference -> m (Maybe (DD.Decl v ()))) ->
  Term v () ->
  m Doc
evalAndRenderDoc pped terms typeOf eval types tm =
  eval tm >>= \case
    Nothing -> pure $ Word "🆘 doc rendering failed during evaluation"
    Just tm -> evalDoc terms typeOf eval types tm >>= renderDoc pped

-- | Renders the given doc, which must have been evaluated using 'evalDoc'
renderDoc ::
  forall v m.
  (Var v, Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  EvaluatedDoc v ->
  m Doc
renderDoc pped doc = traverse renderSpecial doc
  where
    suffixifiedPPE = PPE.suffixifiedPPE pped
    formatPretty = fmap Syntax.convertElement . P.render (P.Width 70)

    formatPrettyType :: PPE.PrettyPrintEnv -> Type v a -> SyntaxText
    formatPrettyType ppe typ = formatPretty (TypePrinter.prettySyntax ppe typ)

    source :: Term v () -> m SyntaxText
    source tm = pure . formatPretty $ TermPrinter.prettyBlock' True (PPE.suffixifiedPPE pped) tm

    goSignatures :: [(Referent, Type v ())] -> m [P.Pretty SSyntaxText]
    goSignatures types =
      pure . fmap P.group $
        TypePrinter.prettySignaturesST
          (PPE.suffixifiedPPE pped)
          [(r, PPE.termName (PPE.suffixifiedPPE pped) r, ty) | (r, ty) <- types]

    renderSpecial :: EvaluatedSpecialForm v -> m RenderedSpecialForm
    renderSpecial = \case
      ESource srcs -> Source <$> renderSrc srcs
      EFoldedSource srcs -> FoldedSource <$> renderSrc srcs
      EExample trm -> Example <$> source trm
      EExampleBlock trm -> ExampleBlock <$> source trm
      ELink ref ->
        let ppe = PPE.suffixifiedPPE pped
            tm :: Referent -> P.Pretty SSyntaxText
            tm r = (NP.styleHashQualified'' (NP.fmt (S.TermReference r)) . PPE.termName ppe) r
            ty :: Reference -> P.Pretty SSyntaxText
            ty r = (NP.styleHashQualified'' (NP.fmt (S.TypeReference r)) . PPE.typeName ppe) r
         in Link <$> case ref of
              Left trm -> source trm
              Right ld -> case ld of
                LD.TermReferent r -> (pure . formatPretty . tm) r
                LD.TypeReference r -> (pure . formatPretty . ty) r
      ESignature rs -> goSignatures rs <&> \s -> Signature (map formatPretty s)
      ESignatureInline r -> goSignatures [r] <&> \s -> SignatureInline (formatPretty (P.lines s))
      EEval trm result -> do
        renderedTrm <- source trm
        case result of
          Nothing -> pure $ Eval renderedTrm evalErrMsg
          Just renderedResult -> Eval renderedTrm <$> source renderedResult
      EEvalInline trm result -> do
        renderedTrm <- source trm
        case result of
          Nothing -> pure $ EvalInline renderedTrm evalErrMsg
          Just renderedResult -> EvalInline renderedTrm <$> source renderedResult
      EEmbed any -> source any <&> \p -> Embed ("{{ embed {{" <> p <> "}} }}")
      EEmbedInline any -> source any <&> \p -> EmbedInline ("{{ embed {{" <> p <> "}} }}")
      EVideo sources config -> pure $ Video sources config
      EFrontMatter frontMatter -> pure $ FrontMatter frontMatter
      ERenderError (InvalidTerm tm) -> source tm <&> \p -> Embed ("🆘  unable to render " <> p)

    evalErrMsg :: SyntaxText
    evalErrMsg = "🆘  An error occured during evaluation"

    renderSrc :: [EvaluatedSrc v] -> m [Ref (UnisonHash, DisplayObject SyntaxText Src)]
    renderSrc srcs =
      srcs & foldMapM \case
        EvaluatedSrcDecl srcDecl -> case srcDecl of
          MissingDecl r -> pure [(Type (Reference.toText r, DO.MissingObject (SH.unsafeFromText $ Reference.toText r)))]
          BuiltinDecl r -> do
            let name =
                  formatPretty . NP.styleHashQualified (NP.fmt (S.TypeReference r))
                    . PPE.typeName suffixifiedPPE
                    $ r
            pure [Type (Reference.toText r, DO.BuiltinObject name)]
            where

          FoundDecl r decl -> do
            pure $ [Type (Reference.toText r, DO.UserObject (Src folded full))]
            where
              full = formatPretty (DeclPrinter.prettyDecl pped r (PPE.typeName suffixifiedPPE r) decl)
              folded = formatPretty (DeclPrinter.prettyDeclHeader (PPE.typeName suffixifiedPPE r) decl)
        EvaluatedSrcTerm srcTerm -> case srcTerm of
          MissingBuiltinTypeSig r -> pure [(Type (Reference.toText r, DO.BuiltinObject "🆘 missing type signature"))]
          BuiltinTypeSig r typ -> do
            pure $ [Type (Reference.toText r, DO.BuiltinObject (formatPrettyType suffixifiedPPE typ))]
          MissingTerm r -> pure [Term (Reference.toText r, DO.MissingObject (SH.unsafeFromText $ Reference.toText r))]
          FoundTerm ref typ tm -> do
            let name = PPE.termName suffixifiedPPE (Referent.Ref ref)
            let folded =
                  formatPretty . P.lines $
                    TypePrinter.prettySignaturesST suffixifiedPPE [(Referent.Ref ref, name, typ)]
            let full tm@(Term.Ann' _ _) _ =
                  formatPretty (TermPrinter.prettyBinding suffixifiedPPE name tm)
                full tm typ =
                  formatPretty (TermPrinter.prettyBinding suffixifiedPPE name (Term.ann () tm typ))
            pure [Term (Reference.toText ref, DO.UserObject (Src folded (full tm typ)))]

-- | Evaluates the given doc, expanding transclusions, expressions, etc.
evalDoc ::
  forall v m.
  (Var v, Monad m) =>
  (Reference -> m (Maybe (Term v ()))) ->
  (Referent -> m (Maybe (Type v ()))) ->
  (Term v () -> m (Maybe (Term v ()))) ->
  (Reference -> m (Maybe (DD.Decl v ()))) ->
  Term v () ->
  m (EvaluatedDoc v)
evalDoc terms typeOf eval types tm = go tm
  where
    go :: Term v () -> m (EvaluatedDoc v)
    go = \case
      DD.Doc2Word txt -> pure $ Word txt
      DD.Doc2Code d -> Code <$> go d
      DD.Doc2CodeBlock lang d -> CodeBlock lang <$> go d
      DD.Doc2Bold d -> Bold <$> go d
      DD.Doc2Italic d -> Italic <$> go d
      DD.Doc2Strikethrough d -> Strikethrough <$> go d
      DD.Doc2Style s d -> Style s <$> go d
      DD.Doc2Anchor id d -> Anchor id <$> go d
      DD.Doc2Blockquote d -> Blockquote <$> go d
      DD.Doc2Blankline -> pure Blankline
      DD.Doc2Linebreak -> pure Linebreak
      DD.Doc2SectionBreak -> pure SectionBreak
      DD.Doc2Tooltip d1 d2 -> Tooltip <$> go d1 <*> go d2
      DD.Doc2Aside d -> Aside <$> go d
      DD.Doc2Callout Decls.OptionalNone' d -> Callout Nothing <$> go d
      DD.Doc2Callout (Decls.OptionalSome' icon) d -> Callout <$> (Just <$> go icon) <*> go d
      DD.Doc2Table rows -> Table <$> traverse r rows
        where
          r (Term.List' ds) = traverse go (toList ds)
          r _ = pure [Word "🆘 invalid table"]
      DD.Doc2Folded isFolded d d2 -> Folded isFolded <$> go d <*> go d2
      DD.Doc2Paragraph ds -> Paragraph <$> traverse go ds
      DD.Doc2BulletedList ds -> BulletedList <$> traverse go ds
      DD.Doc2NumberedList n ds -> NumberedList n <$> traverse go ds
      DD.Doc2Section title ds -> Section <$> go title <*> traverse go ds
      DD.Doc2NamedLink d1 d2 -> NamedLink <$> go d1 <*> go d2
      DD.Doc2Image d1 d2 Decls.OptionalNone' -> Image <$> go d1 <*> go d2 <*> pure Nothing
      DD.Doc2Image d1 d2 (Decls.OptionalSome' d) -> Image <$> go d1 <*> go d2 <*> (Just <$> go d)
      DD.Doc2Special sf -> Special <$> goSpecial sf
      DD.Doc2Join ds -> Join <$> traverse go ds
      DD.Doc2UntitledSection ds -> UntitledSection <$> traverse go ds
      DD.Doc2Column ds -> Column <$> traverse go ds
      DD.Doc2Group d -> Group <$> go d
      wat -> pure $ Special $ ERenderError (InvalidTerm wat)

    goSignatures :: [Referent] -> m [(Referent, Type v ())]
    goSignatures rs =
      runMaybeT (traverse (MaybeT . typeOf) rs) >>= \case
        Nothing -> error "🆘  codebase is missing type signature for these definitions"
        Just types -> pure (zip rs types)

    goSpecial :: Term v () -> m (EvaluatedSpecialForm v)
    goSpecial = \case
      DD.Doc2SpecialFormFoldedSource (Term.List' es) -> EFoldedSource <$> goSrc (toList es)
      -- Source [Either Link.Type Doc2.Term]
      DD.Doc2SpecialFormSource (Term.List' es) -> ESource <$> goSrc (toList es)
      -- Example Nat Doc2.Term
      -- Examples like `foo x y` are encoded as `Example 2 (_ x y -> foo)`, where
      -- 2 is the number of variables that should be dropped from the rendering.
      -- So this will render as `foo x y`.
      DD.Doc2SpecialFormExample n (DD.Doc2Example vs body) ->
        pure $ EExample ex
        where
          ex = Term.lam' (ABT.annotation body) (drop (fromIntegral n) vs) body
      DD.Doc2SpecialFormExampleBlock n (DD.Doc2Example vs body) ->
        pure $ EExampleBlock ex
        where
          ex = Term.lam' (ABT.annotation body) (drop (fromIntegral n) vs) body

      -- Link (Either Link.Type Doc2.Term)
      DD.Doc2SpecialFormLink e ->
        let tm :: Referent -> (Either a LD.LabeledDependency)
            tm r = Right $ LD.TermReferent r
            ty :: Reference -> (Either a LD.LabeledDependency)
            ty r = Right $ LD.TypeReference r
         in ELink <$> case e of
              DD.EitherLeft' (Term.TypeLink' r) -> pure $ ty r
              DD.EitherRight' (DD.Doc2Term t) ->
                case Term.etaNormalForm t of
                  Term.Referent' r -> pure $ tm r
                  x -> pure $ Left x
              _ -> pure $ Left e
      DD.Doc2SpecialFormSignature (Term.List' tms) ->
        let rs = [r | DD.Doc2Term (Term.Referent' r) <- toList tms]
         in goSignatures rs <&> \s -> ESignature s
      -- SignatureInline Doc2.Term
      DD.Doc2SpecialFormSignatureInline (DD.Doc2Term (Term.Referent' r)) ->
        goSignatures [r] <&> \[s] -> ESignatureInline s
      -- Eval Doc2.Term
      DD.Doc2SpecialFormEval (DD.Doc2Term tm) -> do
        result <- eval tm
        pure $ EEval tm result
      -- EvalInline Doc2.Term
      DD.Doc2SpecialFormEvalInline (DD.Doc2Term tm) -> do
        result <- eval tm
        pure $ EEvalInline tm result
      -- Embed Video
      DD.Doc2SpecialFormEmbedVideo sources config ->
        pure $ EVideo sources' config'
        where
          sources' = [MediaSource url mimeType | DD.Doc2MediaSource (Term.Text' url) (maybeText -> mimeType) <- sources]
          config' = Map.fromList [(k, v) | Decls.TupleTerm' [Term.Text' k, Term.Text' v] <- config]
          maybeText (Term.App' _ (Term.Text' a)) = Just a
          maybeText _ = Nothing

      -- Embed FrontMatter
      DD.Doc2SpecialFormEmbedFrontMatter frontMatter ->
        pure $ EFrontMatter frontMatter'
        where
          frontMatter' = List.multimap [(k, v) | Decls.TupleTerm' [Term.Text' k, Term.Text' v] <- frontMatter]

      -- Embed Any
      DD.Doc2SpecialFormEmbed (Term.App' _ any) ->
        pure $ EEmbed any
      -- EmbedInline Any
      DD.Doc2SpecialFormEmbedInline any ->
        pure $ EEmbedInline any
      tm -> pure $ ERenderError (InvalidTerm tm)

    goSrc :: [Term v ()] -> m [EvaluatedSrc v]
    goSrc es = do
      let toRef (Term.Ref' r) = Set.singleton r
          toRef (Term.RequestOrCtor' r) = Set.singleton (r ^. ConstructorReference.reference_)
          toRef _ = mempty
          goType :: Reference -> m (EvaluatedSrc v)
          goType r@(Reference.Builtin builtin) =
            pure (EvaluatedSrcDecl (BuiltinDecl r))
          goType r = do
            d <- types r
            case d of
              Nothing -> pure (EvaluatedSrcDecl $ MissingDecl r)
              Just decl ->
                pure $ EvaluatedSrcDecl (FoundDecl r decl)

          go ::
            (Set.Set Reference, [EvaluatedSrc v]) ->
            Term v () ->
            m (Set.Set Reference, [EvaluatedSrc v])
          go s1@(!seen, !acc) = \case
            -- we ignore the annotations; but this could be extended later
            DD.TupleTerm' [DD.EitherRight' (DD.Doc2Term tm), _anns] ->
              (seen <> toRef tm,) <$> acc'
              where
                acc' = case tm of
                  Term.Ref' r
                    | Set.notMember r seen ->
                        (: acc) <$> case r of
                          Reference.Builtin _ ->
                            typeOf (Referent.Ref r) <&> \case
                              Nothing -> EvaluatedSrcTerm (MissingBuiltinTypeSig r)
                              Just ty -> EvaluatedSrcTerm (BuiltinTypeSig r ty)
                          ref ->
                            terms ref >>= \case
                              Nothing -> pure . EvaluatedSrcTerm . MissingTerm $ ref
                              Just tm -> do
                                typ <- fromMaybe (Type.builtin () "unknown") <$> typeOf (Referent.Ref ref)
                                pure $ EvaluatedSrcTerm (FoundTerm ref typ tm)
                  Term.RequestOrCtor' (view ConstructorReference.reference_ -> r) | Set.notMember r seen -> (: acc) <$> goType r
                  _ -> pure acc
            DD.TupleTerm' [DD.EitherLeft' (Term.TypeLink' ref), _anns]
              | Set.notMember ref seen ->
                  (Set.insert ref seen,) . (: acc) <$> goType ref
            _ -> pure s1
      reverse . snd <$> foldM go mempty es

data RenderError trm
  = InvalidTerm trm
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

deriving anyclass instance ToSchema trm => ToSchema (RenderError trm)

data EvaluatedSrc v
  = EvaluatedSrcDecl (EvaluatedDecl v)
  | EvaluatedSrcTerm (EvaluatedTerm v)
  deriving stock (Show, Eq, Generic)

data EvaluatedDecl v
  = MissingDecl Reference
  | BuiltinDecl Reference
  | FoundDecl Reference (DD.Decl v ())
  deriving stock (Show, Eq, Generic)

data EvaluatedTerm v
  = MissingTerm Reference
  | BuiltinTypeSig Reference (Type v ())
  | MissingBuiltinTypeSig Reference
  | FoundTerm Reference (Type v ()) (Term v ())
  deriving stock (Show, Eq, Generic)

-- Determines all dependencies which will be required to render a doc.
dependencies :: Ord v => EvaluatedDoc v -> Set LD.LabeledDependency
dependencies = foldMap dependenciesSpecial

-- | Determines all dependencies of a special form
dependenciesSpecial :: forall v. Ord v => EvaluatedSpecialForm v -> Set LD.LabeledDependency
dependenciesSpecial = \case
  ESource srcs -> srcDeps srcs
  EFoldedSource srcs -> srcDeps srcs
  EExample trm -> Term.labeledDependencies trm
  EExampleBlock trm -> Term.labeledDependencies trm
  ELink ref -> either Term.labeledDependencies Set.singleton ref
  ESignature sigtyps -> sigtypDeps sigtyps
  ESignatureInline sig -> sigtypDeps [sig]
  EEval trm mayTrm -> Term.labeledDependencies trm <> foldMap Term.labeledDependencies mayTrm
  EEvalInline trm mayTrm -> Term.labeledDependencies trm <> foldMap Term.labeledDependencies mayTrm
  EEmbed trm -> Term.labeledDependencies trm
  EEmbedInline trm -> Term.labeledDependencies trm
  EVideo {} -> mempty
  EFrontMatter {} -> mempty
  ERenderError (InvalidTerm trm) -> Term.labeledDependencies trm
  where
    sigtypDeps :: [(Referent, Type v a)] -> Set LD.LabeledDependency
    sigtypDeps sigtyps =
      sigtyps & foldMap \(ref, typ) ->
        Set.singleton (LD.TermReferent ref) <> Type.labeledDependencies typ
    srcDeps :: [EvaluatedSrc v] -> Set LD.LabeledDependency
    srcDeps srcs =
      srcs & foldMap \case
        EvaluatedSrcDecl srcDecl -> case srcDecl of
          MissingDecl ref -> Set.singleton (LD.TypeReference ref)
          BuiltinDecl ref -> Set.singleton (LD.TypeReference ref)
          FoundDecl ref decl -> Set.singleton (LD.TypeReference ref) <> DD.labeledDeclDependencies decl
        EvaluatedSrcTerm srcTerm -> case srcTerm of
          MissingTerm ref -> Set.singleton (LD.TermReference ref)
          BuiltinTypeSig ref _ -> Set.singleton (LD.TermReference ref)
          MissingBuiltinTypeSig ref -> Set.singleton (LD.TermReference ref)
          FoundTerm ref typ trm -> Set.singleton (LD.TermReference ref) <> Type.labeledDependencies typ <> Term.labeledDependencies trm
