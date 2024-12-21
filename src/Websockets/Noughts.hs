{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets.Noughts (app, playNoughts, initialState) where

import Control.Concurrent (newEmptyMVar)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Game.Noughts
import Servant
import Servant.API.WebSocket (WebSocket)
import Websockets

playNoughts :: Connections Player -> IO ()
playNoughts connections = play $ updateAll connections <=< moveStateful getPlay
  where
    getPlay player game = repeatUntilValid $ fmap (applyMove player game) $ getMove connections player game

getMove :: Connections Player -> GetMove IO
getMove connections player _ = do
  sendJSON conn YourMove
  receiveJSONOrFail conn
  where
    conn = connections player

type WebSocketApi = ("join" :> "O" :> WebSocket) :<|> ("join" :> "X" :> WebSocket)

api :: Proxy WebSocketApi
api = Proxy

server :: Seats Player -> Server WebSocketApi
server seats = playerRoute Nought seats :<|> playerRoute Cross seats

app :: Seats Player -> Application
app seats = serve api (server seats)

repeatUntilValid :: (Monad f) => f (Maybe a) -> f a
repeatUntilValid fa = do
  maybeA <- fa
  case maybeA of
    Just a -> return a
    Nothing -> repeatUntilValid fa

initialState :: (MonadIO f) => f (Seats Player)
initialState = liftIO $ f <$> newEmptyMVar <*> liftIO newEmptyMVar
  where
    f first second = Map.fromList [(Nought, first), (Cross, second)]

updateAll :: (MonadIO f) => Connections Player -> GameStatus -> f GameStatus
updateAll connections status = status <$ traverse updateEach [Nought, Cross]
  where
    updateEach player = update (connections player)
      where
        update connection = sendJSON connection $ status

data MoveRequest = YourMove deriving (Generic)

instance ToJSON MoveRequest
