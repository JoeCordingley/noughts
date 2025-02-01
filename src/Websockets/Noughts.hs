{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets.Noughts (app, playNoughts, prepareSeats) where

import Control.Concurrent (newEmptyMVar)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (StateT (..))
import Data.Aeson (ToJSON (..), object, toJSON, (.=))
import Data.Aeson.Key (fromString)
import Data.Map ((!))
import qualified Data.Map as Map
import Game.Noughts
import Servant
import Servant.API.WebSocket (WebSocket)
import Websockets

playNoughts :: Connections Player -> IO ()
playNoughts connections =
    updateAll connections (ContinuingGame O) *> play (updateAll connections <=< moveStateful (getMoveFromConnection connections))

getMoveFromConnection :: Connections Player -> GetMove IO
getMoveFromConnection connections player = do
    receiveJSONOrRetry conn
  where
    conn = connections ! player

moveStateful :: (MonadFail f) => GetMove f -> Player -> StateT Game f GameStatus
moveStateful getMove player = StateT $ \game -> do
    move <- getMove player
    newGame <- maybe (fail "invalid move") return $ applyMove player move game
    return (gameStatus player newGame, newGame)

type WebSocketApi = ("join" :> "O" :> WebSocket) :<|> ("join" :> "X" :> WebSocket)

api :: Proxy WebSocketApi
api = Proxy

server :: Seats Player -> Server WebSocketApi
server seats = playerRoute O seats :<|> playerRoute X seats

app :: Seats Player -> Application
app seats = serve api (server seats)

prepareSeats :: (MonadIO f) => f (Seats Player)
prepareSeats = liftIO $ f <$> newEmptyMVar <*> liftIO newEmptyMVar
  where
    f first second = Map.fromList [(O, first), (X, second)]

updateAll :: (MonadIO f) => Connections Player -> GameStatus -> f GameStatus
updateAll connections status = status <$ traverse updateEach [O, X]
  where
    updateEach player = sendJSON (connections ! player) $ updateStatus player status

type IndexedSquare a = (Move, a)

withIndex :: [a] -> [IndexedSquare a]
withIndex = zip moves

data UpdateStatus = WonStatus [Bool] | Continuing Bool | DrawnStatus

instance ToJSON UpdateStatus where
    toJSON = \case
        WonStatus squares -> object [fromString "won" .= withIndex squares]
        Continuing currentPlayer -> object [fromString "currentPlayer" .= currentPlayer]
        DrawnStatus -> toJSON "drawn"

updateStatus :: Player -> GameStatus -> UpdateStatus
updateStatus player = \case
    WonGame squares -> WonStatus squares
    DrawnGame -> DrawnStatus
    ContinuingGame currentPlayer -> Continuing (player == currentPlayer)
