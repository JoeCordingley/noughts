{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Noughts (Player (..))
import Servant
import Servant.API.WebSocket (WebSocket)
import Websockets

main :: IO ()
main = startApp app playNoughts

playNoughts :: (Connections Noughts.Player -> IO ())
playNoughts = undefined

type WebSocketApi = ("join" :> "O" :> WebSocket) :<|> ("join" :> "X" :> WebSocket)

api :: Proxy WebSocketApi
api = Proxy

server :: Seats Noughts.Player -> Server WebSocketApi
server seats = playerRoute Noughts.Nought seats :<|> playerRoute Noughts.Cross seats

app :: Seats Noughts.Player -> Application
app seats = serve api (server seats)
