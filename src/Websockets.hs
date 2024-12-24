{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets
  ( startApp,
    Seats,
    playerRoute,
    Connections,
    sendJSON,
    receiveJSON,
    receiveJSONOrRetry,
    repeatUntilValid,
  )
where

import Control.Concurrent
  ( MVar,
    forkIO,
    putMVar,
    takeMVar,
    threadDelay,
  )
import Control.Monad (forever, (<=<))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Foldable (traverse_)
import Data.Map (Map, (!))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets
  ( Connection,
    withPingThread,
  )
import Network.WebSockets.Connection (receiveData, sendTextData)
import Servant

startApp :: (MonadIO f, Ord player) => Seats player -> (Seats player -> Application) -> (Connections player -> IO ()) -> f ()
startApp state app playGame = liftIO $ do
  _ <- forkIO $ hostGames state playGame
  putStrLn "Starting server on http://localhost:8080"
  run 8080 (app state)

type Seats player = Map player (MVar Connection)

type Connections player = Map player Connection

waitForConnections :: (MonadIO f) => Seats player -> f (Connections player)
waitForConnections seats = traverse (liftIO . takeMVar) seats

hostGames :: (MonadIO f, Ord player) => Seats player -> (Connections player -> f ()) -> f ()
hostGames seats playGame = do
  connections <- waitForConnections seats
  playGames $ playGame connections <* receiveReset connections

receiveReset :: (MonadIO f, Ord player) => Connections player -> f (Map player Reset)
receiveReset connections = liftIO $ mapConcurrently (receiveJSONOrRetry) connections

data Reset = Reset deriving (Generic, Show)

instance FromJSON Reset

playGames :: (Monad f) => f () -> f ()
playGames playGame = playGame *> playGames playGame

playerRoute :: (MonadIO m, MonadError ServerError m, Ord player) => player -> Seats player -> Connection -> m ()
playerRoute player seats conn = keepAlive conn communication
  where
    communication = liftIO $ putMVar (seats ! player) conn *> waitForever

waitForever :: IO ()
waitForever = forever $ threadDelay maxBound

keepAlive :: (MonadError e m, MonadIO m) => Connection -> ExceptT e IO c -> m c
keepAlive conn =
  liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT

sendJSON :: (ToJSON a, MonadIO m) => Connection -> a -> m ()
sendJSON conn a = liftIO $ sendTextData conn (encode a)

receiveJSON ::
  (MonadIO m, MonadError ServerError m, FromJSON a) => Connection -> m a
receiveJSON = stringError . eitherDecode <=< liftIO . receiveData

receiveJSONMaybe ::
  (FromJSON a, Show a, MonadIO m) =>
  Connection ->
  m (Maybe a) 
receiveJSONMaybe conn = do
  d <- liftIO . receiveData $ conn
  maybeDecode d

receiveJSONOrRetry ::
  (MonadIO m, FromJSON a, Show a) =>
  Connection ->
  m a
receiveJSONOrRetry conn = repeatUntilValid $ receiveJSONMaybe conn

repeatUntilValid :: (Monad f) => f (Maybe a) -> f a
repeatUntilValid fa = do
  maybeA <- fa
  case maybeA of
    Just a -> return a
    Nothing -> repeatUntilValid fa

maybeDecode :: (FromJSON a, MonadIO m) => B.ByteString -> m (Maybe a)
maybeDecode s = case eitherDecode s of
  Left e -> Nothing <$ (liftIO $ putStrLn e)
  Right a -> pure (Just a)

stringError :: (MonadError ServerError m) => Either String a -> m a
stringError (Left s) = throwError err400 {errBody = B.fromString s}
stringError (Right a) = pure a
