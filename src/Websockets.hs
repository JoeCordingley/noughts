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
import Control.Monad.State (MonadState, StateT (..), evalStateT, state)

startApp :: IO ()
startApp = do
    state <- initialState
    forkIO $ hostGame state
    putStrLn "Starting server on http://localhost:8080"
    run 8080 (app state)

hostGame :: Seats -> IO ()
hostGame seats = do
    connections <- waitForConnections seats
    playGames $ StateT . (playMove . getPlay $ getMove connections)

getPlay :: Monad f => GetMove f -> GetPlay f
getPlay getMove player game = repeatUntilValid $ fmap applyMove $ getMove player game
  where
    applyMove move = undefined

getMove :: Connections -> GetMove f
getMove = undefined

data CompletionStatus = Finished | Playing
type PlayMove f = Player -> f CompletionStatus
data Move

type Seats = Player -> MVar Connection
type Connections = Player -> Connection
type GetPlay f = Player -> Game -> f Game
type GetMove f = Player -> Game -> f Move

waitForConnections :: Seats -> f Connections
waitForConnections = undefined

playMove :: Monad f => GetPlay f -> Player -> Game -> f (CompletionStatus, Game)
playMove receiveMove player game = do
    newGame <- receiveMove player game
    return (completionStatus newGame, newGame)

repeatUntilValid :: Monad f => f (Maybe a) -> f a
repeatUntilValid fa = do
    maybeA <- fa
    case maybeA of
        Just a -> return a
        Nothing -> repeatUntilValid fa

completionStatus :: Game -> CompletionStatus
completionStatus FinishedGame = Finished
completionStatus PlayingGame = Playing

data Game = FinishedGame | PlayingGame

startingGame :: Game
startingGame = undefined

playGames :: Monad f => PlayMove (StateT Game f) -> f ()
playGames playMove = evalStateT (playGame playMove) startingGame *> playGames playMove

playGame :: Monad f => PlayMove f -> f ()
playGame playMove = play Nought
  where
    play player = do
        game <- playMove player
        case game of
            Finished -> return ()
            Playing -> play (nextPlayer player)
    nextPlayer Nought = Cross
    nextPlayer Cross = Nought

app :: Seats -> Application
app state = serve api (server state)

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
    f first second = g
      where
        g Nought = first
        g Cross = second

keepAlive :: (MonadError e m, MonadIO m) => Connection -> ExceptT e IO c -> m c
keepAlive conn =
    liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT
