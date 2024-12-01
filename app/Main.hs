{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Websockets (startApp)
import Websockets.Noughts

main :: IO ()
main = do
    state <- initialState
    startApp state app playNoughts
