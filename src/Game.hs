{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Player (..),
    Game,
    GameStatus,
    gameStatus,
    nextPlayer,
    startingGame,
    nw,
    n,
    ne,
    w,
    c,
    e,
    sw,
    s,
    se,
    Move (..),
    boardLens,
  )
where

import Control.Lens
import Data.Maybe (fromMaybe)

data Player = Nought | Cross

type Game = Board (Maybe Player)

type BoardLens = forall a. Lens (Board a) (Board a) a a

data Board a = Board
  { _ne :: a,
    _n :: a,
    _nw :: a,
    _w :: a,
    _c :: a,
    _e :: a,
    _sw :: a,
    _s :: a,
    _se :: a
  }

makeLenses ''Board

startingGame :: Game
startingGame =
  Board
    { _ne = Nothing,
      _n = Nothing,
      _nw = Nothing,
      _w = Nothing,
      _c = Nothing,
      _e = Nothing,
      _sw = Nothing,
      _s = Nothing,
      _se = Nothing
    }

winningLines :: [[Move]]
winningLines =
  [ [NW, N, NE],
    [W, C, E],
    [SW, S, SE],
    [NW, C, SE],
    [NE, C, SW]
  ]

nextPlayer :: Player -> Player
nextPlayer Nought = Cross
nextPlayer Cross = Nought

data Move = NW | N | NE | W | C | E | SW | S | SE

boardLens :: Move -> BoardLens
boardLens move = case move of
  NW -> nw
  N -> n
  NE -> ne
  W -> w
  C -> c
  E -> e
  SW -> sw
  S -> s
  SE -> se

type GameStatus = Either FinishedGame Game

type FinishedGame = Board FinishedSquare

data FinishedSquare = FinishedSquare {winningSquare :: Bool, mark :: Maybe Player}

gameStatus :: Player -> Game -> GameStatus
gameStatus player game = case wonGame player game of
  Just finished -> Left finished
  Nothing -> Right game

wonGame :: Player -> Game -> Maybe FinishedGame
wonGame player game = join <$> foldr f Nothing winningLines
  where
    f line maybeFinished = maybe (tryLine empty) tryLine maybeFinished
      where
        tryLine = undefined
    empty = undefined
    join = undefined
