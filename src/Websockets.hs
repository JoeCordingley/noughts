{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets (
    startApp,
    Seats,
    playerRoute,
    Connections,
    sendJSON,
    receiveJSON,
)
where

import Control.Concurrent (
    MVar,
    forkIO,
    putMVar,
 )
import Control.Monad ((<=<))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as B
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (
    Connection,
    withPingThread,
 )
import Network.WebSockets.Connection (receiveData, sendTextData)
import Servant

startApp :: MonadIO f => Seats player -> (Seats player -> Application) -> (Connections player -> IO ()) -> f ()
startApp state app playGame = liftIO $ do
    _ <- forkIO $ hostGames state playGame
    putStrLn "Starting server on http://localhost:8080"
    run 8080 (app state)

type Seats player = player -> MVar Connection

type Connections player = player -> Connection

waitForConnections :: Seats player -> f (Connections player)
waitForConnections = undefined

-- updatePlayers :: (MonadIO f) => Connections player -> GameStatus -> f ()
-- updatePlayers = undefined

hostGames :: MonadIO f => Seats player -> (Connections player -> f ()) -> f ()
hostGames seats playGame = do
    connections <- waitForConnections seats
    playGames $ playGame connections

playGames :: (Monad f) => f () -> f ()
playGames playGame = playGame *> playGames playGame

-- playGames :: (Monad f) => StateT game f () -> game -> f ()
-- playGames playGame startingGame = evalStateT playGame startingGame *> playGames playMove

playerRoute :: (MonadIO m, MonadError ServerError m) => player -> Seats player -> Connection -> m ()
playerRoute player seats conn = keepAlive conn communication
  where
    communication = liftIO $ putMVar (seats player) conn

keepAlive :: (MonadError e m, MonadIO m) => Connection -> ExceptT e IO c -> m c
keepAlive conn =
    liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT

sendJSON :: (ToJSON a, MonadIO m) => Connection -> a -> m ()
sendJSON conn a = liftIO $ sendTextData conn (encode a)

receiveJSON ::
    (MonadIO m, MonadError ServerError m, FromJSON a) => Connection -> m a
receiveJSON = stringError . eitherDecode <=< liftIO . receiveData

stringError :: (MonadError ServerError m) => Either String a -> m a
stringError (Left s) = throwError err400{errBody = B.fromString s}
stringError (Right a) = pure a
