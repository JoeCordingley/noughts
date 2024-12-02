{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Websockets.Noughts (app, playNoughts, initialState) where

import Control.Concurrent (newEmptyMVar)
import Control.Lens (view)
import Control.Monad ((<=<))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import GHC.Generics (Generic)
import Game.Noughts
import Servant
import Servant.API.WebSocket (WebSocket)
import Websockets

playNoughts :: Connections Player -> IO ()
playNoughts connections = play $ updateAll connections <=< moveStateful getPlay
  where
    getPlay player game = repeatUntilValid $ fmap (applyMove player game) $ getMove connections player game

getMove :: (MonadIO f, MonadError ServerError f) => Connections Player -> GetMove f
getMove connections player game = do
    sendJSON conn $ moveRequest game
    receiveJSON conn
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

initialState :: MonadIO f => f (Seats Player)
initialState = liftIO $ f <$> newEmptyMVar <*> liftIO newEmptyMVar
  where
    f first second = g
      where
        g Nought = first
        g Cross = second

updateAll :: Applicative f => Connections Player -> GameStatus -> f GameStatus
updateAll connections status = status <$ traverse updateEach [Nought, Cross]
  where
    updateEach player = update (connections player)
      where
        update connection = undefined

data MoveRequest = MoveRequest {yourMove :: Board} deriving (Generic, Show)
instance ToJSON MoveRequest

moveRequest :: Game -> MoveRequest
moveRequest game =
    MoveRequest $
        Board
            { nw = mark <$> view (t . l) game
            , n = mark <$> view (t . c) game
            , ne = mark <$> view (t . r) game
            , w = mark <$> view (m . l) game
            , rose = mark <$> view (m . c) game
            , e = mark <$> view (m . r) game
            , sw = mark <$> view (b . l) game
            , s = mark <$> view (b . c) game
            , se = mark <$> view (b . r) game
            }
data Board = Board
    { nw :: Maybe Mark
    , n :: Maybe Mark
    , ne :: Maybe Mark
    , w :: Maybe Mark
    , rose :: Maybe Mark
    , e :: Maybe Mark
    , sw :: Maybe Mark
    , s :: Maybe Mark
    , se :: Maybe Mark
    }
    deriving (Generic, Show)
instance ToJSON Board
data Mark = O | X deriving (Generic, Show)
instance ToJSON Mark
mark :: Player -> Mark
mark Nought = O
mark Cross = X
instance FromJSON Move

-- updateAll connections = traverse updateEach [Nought, Cross] where
