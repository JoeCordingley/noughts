{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Websockets (startApp)
import Websockets.Noughts

main :: IO ()
main = do
    state <- prepareSeats
    startApp state app playNoughts
