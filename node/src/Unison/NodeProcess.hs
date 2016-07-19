{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}

module Unison.NodeProcess where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TSem
import Control.Monad.IO.Class
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.Serialize.Get (Get)
import GHC.Generics
import System.IO (stdin, hSetBinaryMode)
import Unison.BlockStore (BlockStore(..), Series(..))
import Unison.Cryptography (Cryptography)
import Unison.Hash.Extra ()
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Serialize.Get as Get
import qualified Unison.Cryptography as C
import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.Block as Block
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote

data Keypair k = Keypair { public :: k, private :: B.ByteString } deriving Generic
instance Serial k => Serial (Keypair k)

make :: ( BA.ByteArrayAccess key
        , Serial signature
        , Serial term
        , Serial hash
        , Serial thash
        , Serial h
        , Eq h
        , Serial key
        , Serial signKey
        , Ord thash)
     => P.Protocol term hash h thash
     -> (Keypair key -> Cryptography key symmetricKey signKey skp signature hash Remote.Cleartext)
     -> Get (BlockStore h -> Mux.Multiplex (Remote.Language term thash))
     -> IO ()
make protocol mkCrypto makeSandbox = do
  hSetBinaryMode stdin True
  -- todo - read private key, then node hash from stdin
  (nodeSeries, rem) <- Mux.deserializeHandle1 stdin (Get.runGetPartial deserialize B.empty)
  interrupt <- atomically $ newTSem 0
  Mux.runStandardIO (Mux.seconds 5) rem (atomically $ waitTSem interrupt) $ do
    blockStore <- P.blockStoreProxy protocol
    Just (keypair, universe, node, sandbox) <- -- todo: lifetime, budget, children
      liftIO . Block.get blockStore . Block.serial Nothing . Block.fromSeries . Series $ nodeSeries
    makeSandbox <- either fail pure $ Get.runGetS makeSandbox sandbox
    sandbox <- makeSandbox blockStore
    let crypto = mkCrypto keypair
    let skHash = Put.runPutS (serialize $ C.hash crypto [Put.runPutS (serialize $ private keypair)])
    -- todo: load this from persistent store also
    connectionSandbox <- pure $ Remote.ConnectionSandbox (\_ -> pure True) (\_ -> pure True)
    env <- liftIO $ Remote.makeEnv universe node blockStore
    _ <- Mux.fork $ Remote.server crypto connectionSandbox env sandbox protocol
    _ <- Mux.fork $ do
      (destroy, cancel) <- Mux.subscribeTimed (Mux.seconds 60) (P._destroyIn protocol)
      Mux.repeatWhile $ do
        sig <- destroy
        case sig of
          -- todo: constant time equality needed here?
          Just sig | skHash == Put.runPutS (serialize sig) -> do
            cancel
            Mux.send (Mux.Channel Mux.Type skHash) ()
            -- no other cleanup needed; container will reclaim resources and eventually
            -- kill off linked child nodes
            liftIO $ atomically (signalTSem interrupt)
            pure False
          _ -> pure True
    pure ()
