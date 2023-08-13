{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.CancelRequest where

import Control.Lens
import Control.Monad.Reader
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens as LSP
import Language.LSP.Protocol.Types
import Unison.LSP.Types
import UnliftIO.STM

-- | Allows a client to cancel work from a previous request.
cancelRequestHandler :: NotificationMessage 'CancelRequest -> Lsp ()
cancelRequestHandler m = do
  cancelMap <- asks cancellationMapVar >>= readTVarIO
  let reqId' = case m ^. params of
        CancelParams id' -> SomeLspId id'
  case Map.lookup reqId' cancelMap of
    Just cancel -> liftIO cancel
    Nothing -> pure ()
