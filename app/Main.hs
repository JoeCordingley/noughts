{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Websockets (startApp)
import Websockets.Noughts

main :: IO ()
main = do
  state <- seats
  startApp state app playNoughts
