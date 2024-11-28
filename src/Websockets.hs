{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets
  ( startApp,
  )
where

import Control.Concurrent
  ( MVar,
    forkIO,
    newEmptyMVar,
    putMVar,
  )
import Control.Monad (liftM, (<=<))
import Control.Monad.Error.Class (MonadError, liftEither)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), evalStateT, gets, modifyM)
import Game
import Network.Wai.Handler.Warp (run)
import Network.WebSockets
  ( Connection,
    withPingThread,
  )
import Servant
import Servant.API.WebSocket (WebSocket)

startApp :: IO ()
startApp = do
  state <- initialState
  _ <- forkIO $ hostGame state
  putStrLn "Starting server on http://localhost:8080"
  run 8080 (app state)

getPlay :: (Monad f) => GetMove f -> GetPlay f
getPlay getMove player game = repeatUntilValid $ fmap applyMove $ getMove player game
  where
    applyMove move = boardLens move f game
      where
        f Nothing = Just $ Just player
        f _ = Nothing

getMove :: Connections -> GetMove f
getMove = undefined

data CompletionStatus = Finished | Playing

type PlayMove f = Player -> f CompletionStatus

type Seats = Player -> MVar Connection

type Connections = Player -> Connection

type GetPlay f = Player -> Game -> f Game

type GetMove f = Player -> Game -> f Move

waitForConnections :: Seats -> f Connections
waitForConnections = undefined

moveAndUpdate :: (MonadIO f) => (GameStatus -> f ()) -> GetPlay f -> Player -> StateT Game f GameStatus
moveAndUpdate update receiveMove player = do
  modifyM (receiveMove player)
  status <- gets (gameStatus player)
  lift $ update status
  return status

updatePlayers :: (MonadIO f) => Connections -> GameStatus -> f ()
updatePlayers = undefined

repeatUntilValid :: (Monad f) => f (Maybe a) -> f a
repeatUntilValid fa = do
  maybeA <- fa
  case maybeA of
    Just a -> return a
    Nothing -> repeatUntilValid fa

completionStatus :: GameStatus -> CompletionStatus
completionStatus (Right _) = Finished
completionStatus (Left _) = Playing

hostGame :: Seats -> IO ()
hostGame seats = do
  connections <- waitForConnections seats
  playGames $ fmap completionStatus . (moveAndUpdate (updatePlayers connections) . getPlay $ getMove connections)

playGames :: (Monad f) => PlayMove (StateT Game f) -> f ()
playGames playMove = evalStateT (playGame playMove) startingGame *> playGames playMove

playGame :: (Monad f) => PlayMove f -> f ()
playGame playMove = play Nought
  where
    play player = do
      game <- playMove player
      case game of
        Finished -> return ()
        Playing -> play (nextPlayer player)

app :: Seats -> Application
app seats = serve api (server seats)

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
