{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Websockets (
    startApp,
) where

import Network.WebSockets (
    Connection,
    withPingThread,
 )
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.Server (ServerError)

import Control.Concurrent (
    MVar,
    forkIO,
    newEmptyMVar,
    newMVar,
    putMVar,
 )
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai.Handler.Warp (run)

import Control.Monad ((<=<))

startApp :: IO ()
startApp = do
    state <- initialState
    forkIO $ hostGame state
    putStrLn "Starting server on http://localhost:8080"
    run 8080 (app state)

hostGame :: Seats -> IO ()
hostGame seats = do
    players <- waitForPlayers seats
    playGames players

data Game
type Players f = Player -> Game -> f Game

waitForPlayers :: Seats -> f (Players f)
waitForPlayers = undefined

playGames :: Applicative f => Players f -> f ()
playGames players = playGame players *> playGames players

playGame :: Players f -> f ()
playGame players = undefined

app :: Seats -> Application
app state = serve api (server state)

type Seats = Player -> MVar Connection

data Player = Nought | Cross

api :: Proxy WebSocketApi
api = Proxy

type WebSocketApi = ("join" :> "O" :> WebSocket) :<|> ("join" :> "X" :> WebSocket)

server :: Seats -> Server WebSocketApi
server seats = playerRoute Nought seats :<|> playerRoute Cross seats

playerRoute :: (MonadIO m, MonadError ServerError m) => Player -> Seats -> Connection -> m ()
playerRoute player seats conn = keepAlive conn communication
  where
    communication = liftIO $ putMVar (seats player) conn

initialState :: IO Seats
initialState = f <$> newEmptyMVar <*> newEmptyMVar
  where
    f = firstSecond Nought Cross

firstSecond :: a -> a -> b -> b -> a -> b
firstSecond = undefined

keepAlive :: (MonadError e m, MonadIO m) => Connection -> ExceptT e IO c -> m c
keepAlive conn =
    liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT
