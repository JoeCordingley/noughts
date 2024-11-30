{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets (
    startApp,
    Seats,
    playerRoute,
    Connections,
)
where

import Control.Concurrent (
    MVar,
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
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (
    Connection,
    withPingThread,
 )
import Servant
import Servant.API.WebSocket (WebSocket)

startApp :: (Seats player -> Application) -> (Connections player -> IO ()) -> IO ()
startApp app playGame = do
    state <- initialState
    _ <- forkIO $ hostGames state playGame
    putStrLn "Starting server on http://localhost:8080"
    run 8080 (app state)

initialState :: MonadIO f => f (Seats player)
initialState = undefined

-- initialState = f <$> newEmptyMVar <*> newEmptyMVar
--  where
--    f first second = g
--      where
--        g Nought = first
--        g Cross = second

-- getMove :: Connections -> GetMove f
-- getMove = undefined

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
