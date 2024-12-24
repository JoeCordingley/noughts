{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets.Noughts (app, playNoughts, seats) where

import Control.Concurrent (newEmptyMVar)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson.Key (fromString)
import Data.Map ((!))
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Game.Noughts
import Servant
import Servant.API.WebSocket (WebSocket)
import Websockets

playNoughts :: Connections Player -> IO ()
playNoughts connections = updateAll connections initialStatus *> (play $ updateAll connections <=< moveStateful getPlay)
  where
    getPlay player game = repeatUntilValid $ fmap (applyMove player game) $ getMove connections player game

getMove :: Connections Player -> GetMove IO
getMove connections player _ = do
  receiveJSONOrRetry conn
  where
    conn = connections ! player

type WebSocketApi = ("join" :> "O" :> WebSocket) :<|> ("join" :> "X" :> WebSocket)

api :: Proxy WebSocketApi
api = Proxy

server :: Seats Player -> Server WebSocketApi
server seats = playerRoute O seats :<|> playerRoute X seats

app :: Seats Player -> Application
app seats = serve api (server seats)

seats :: (MonadIO f) => f (Seats Player)
seats = liftIO $ f <$> newEmptyMVar <*> liftIO newEmptyMVar
  where
    f first second = Map.fromList [(O, first), (X, second)]

updateAll :: (MonadIO f) => Connections Player -> GameStatus -> f GameStatus
updateAll connections status = status <$ traverse updateEach [O, X]
  where
    updateEach player = sendJSON (connections ! player) $ updateStatus player status

type IndexedSquare a = (Move, a)

withIndex :: [a] -> [IndexedSquare a]
withIndex = zip moves

data UpdateStatus = WonStatus [FinishedSquare] | Continuing [Maybe Player] Bool | DrawnStatus [Maybe Player] 

instance ToJSON UpdateStatus where
  toJSON = \case
    WonStatus squares -> object [fromString "won" .= withIndex squares]
    Continuing board currentPlayer -> object [fromString "board" .= withIndex board, fromString "currentPlayer" .= currentPlayer]
    DrawnStatus board -> object [fromString "drawn" .= withIndex board]

updateStatus :: Player -> GameStatus -> UpdateStatus
updateStatus player = \case
  WonGame squares -> WonStatus squares
  DrawnGame board -> DrawnStatus $ board
  ContinuingGame board currentPlayer -> Continuing board (player == currentPlayer)
