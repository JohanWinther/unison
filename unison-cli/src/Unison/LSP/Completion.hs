{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.Completion where

import Control.Lens hiding (List)
import Control.Monad.Reader
import Data.String.Here.Uninterpolated (here)
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Text.FuzzyFind as Fuzzy
import qualified U.Codebase.Sqlite.Queries as Q
import qualified Unison.Codebase as Codebase
import qualified Unison.Debug as Debug
import Unison.LSP.Types
import qualified Unison.LSP.VFS as VFS
import Unison.Prelude
import qualified Unison.Server.Endpoints.FuzzyFind as FZF
import qualified Unison.Server.Syntax as Server
import qualified Unison.Server.Types as Backend
import Unison.Sqlite (runTransaction)

-- | Rudimentary auto-completion handler
--
-- TODO:
-- * Rewrite this to use an index rather than fuzzy searching ALL names
-- * Respect ucm's current path
-- * Provide namespaces as auto-complete targets
-- * Auto-complete minimally suffixed names
-- * Include docs in completion details?
completionHandler :: RequestMessage 'TextDocumentCompletion -> (Either ResponseError (ResponseResult 'TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m respond =
  respond =<< do
    mayPrefix <- VFS.completionPrefix (m ^. params)
    case mayPrefix of
      Nothing -> pure . Right . InL . List $ []
      Just (_range, prefix) -> do
        let isIncomplete = True -- TODO: be smarter about this
        Right . InR . CompletionList isIncomplete . List <$> namespaceCompleter prefix
  where
    -- matches <- expand range prefix
    -- let isIncomplete = True -- TODO: be smarter about this
    -- pure . Right . InR . CompletionList isIncomplete . List $ snippetCompletions prefix range <> matches

    _resultToCompletion :: Range -> Text -> FZF.FoundResult -> CompletionItem
    _resultToCompletion range prefix = \case
      FZF.FoundTermResult (FZF.FoundTerm {namedTerm = Backend.NamedTerm {termName, termType}}) -> do
        (mkCompletionItem Nothing termName)
          { _detail = (": " <>) . Text.pack . Server.toPlain <$> termType,
            _kind = Just CiVariable,
            _insertText = Text.stripPrefix prefix termName,
            _textEdit = Just $ CompletionEditText (TextEdit range termName)
          }
      FZF.FoundTypeResult (FZF.FoundType {namedType = Backend.NamedType {typeName, typeTag}}) ->
        let (detail, kind) = case typeTag of
              Backend.Ability -> ("Ability", CiInterface)
              Backend.Data -> ("Data", CiClass)
         in (mkCompletionItem Nothing typeName)
              { _detail = Just detail,
                _kind = Just kind
              }
    _expand :: Range -> Text -> Lsp [CompletionItem]
    _expand range prefix = do
      -- We should probably write a different fzf specifically for completion, but for now, it
      -- expects the unique pieces of the query to be different "words".
      let query = Text.unwords . Text.splitOn "." $ prefix
      cb <- asks codebase
      lspBackend (FZF.serveFuzzyFind cb Nothing Nothing Nothing Nothing (Just $ Text.unpack query)) >>= \case
        Left _be -> pure []
        Right results ->
          pure . fmap (_resultToCompletion range prefix . snd) . take 150 . sortOn (Fuzzy.score . fst) $ results

snippetCompletions :: Text -> Range -> [CompletionItem]
snippetCompletions prefix range =
  [ ("handler", handlerTemplate),
    ("cases", casesTemplate),
    ("match-with", matchWithTemplate)
  ]
    & filter (Text.isPrefixOf prefix . fst)
    & fmap toCompletion
  where
    toCompletion :: (Text, Text) -> CompletionItem
    toCompletion (pat, snippet) =
      (mkCompletionItem (Just CiSnippet) pat)
        { _insertTextFormat = Just Snippet,
          _insertTextMode = Just AdjustIndentation,
          _textEdit = Just $ CompletionEditText (TextEdit range snippet)
        }
    handlerTemplate =
      [here|
handle${1:Ability} : Request (${1:Ability} ${2}) a -> a
handle${1:Ability} = cases
  {${3} -> continue} -> do
    ${4}
    |]
    casesTemplate =
      [here|
cases
  ${1} -> do
    ${2}
    |]
    matchWithTemplate =
      [here|
match ${1} with
  ${2} -> do
    ${3}
    |]

mkCompletionItem :: Maybe CompletionItemKind -> Text -> CompletionItem
mkCompletionItem kind lbl =
  CompletionItem
    { _label = lbl,
      _kind = kind,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _xdata = Nothing
    }

namespaceCompleter :: Text -> Lsp [CompletionItem]
namespaceCompleter prefix = do
  Env {codebase} <- ask
  liftIO $ Codebase.withConnection codebase \conn -> runTransaction conn $ do
    matchingNamespaces <- fmap (mkCompletionItem (Just CiModule)) <$> Q.namespacesByPrefix prefix
    matchingTermsAndTypes <- fmap (mkCompletionItem (Just CiModule)) <$> Q.termAndTypeNamesCompletion prefix
    Debug.debugM Debug.LSP "Namespaces" matchingNamespaces
    Debug.debugM Debug.LSP "Terms and Types" matchingTermsAndTypes
    Debug.debugM Debug.LSP "Prefix" prefix
    pure $ (matchingTermsAndTypes <> matchingNamespaces)
