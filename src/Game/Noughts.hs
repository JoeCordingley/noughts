{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Noughts (
    Player (..),
    Game,
    GameStatus,
    gameStatus,
    nextPlayer,
    startingGame,
    Move (..),
    boardLens,
    play,
    moveStateful,
    GetMove,
    applyMove,
    t,
    m,
    b,
    l,
    c,
    r,
)
where

import Control.Lens
import Control.Monad.Trans.State (StateT (..), evalStateT, gets, modifyM)
import Data.Maybe (isJust)
import Data.Semigroup (Any (..), Endo (..))
import GHC.Generics (Generic)
import Game (CompletionStatus (..), PlayMove, Players (..), playGame)

data Player = Nought | Cross deriving (Eq, Show, Generic)

type Game = Board (Maybe Player)

type BoardLens a = Lens' (Board a) a

data Board a = Board {_t :: Line a, _m :: Line a, _b :: Line a} deriving (Show, Generic)

instance Functor Board where
    fmap f (Board x y z) = Board (fmap f x) (fmap f y) (fmap f z)

instance Applicative Board where
    pure x = Board (pure x) (pure x) (pure x)
    Board fx fy fz <*> Board x y z = Board (fx <*> x) (fy <*> y) (fz <*> z)

instance Semigroup a => Semigroup (Board a) where
    Board x1 y1 z1 <> Board x2 y2 z2 = Board (x1 <> x2) (y1 <> y2) (z1 <> z2)

data Line a = Line {_l :: a, _c :: a, _r :: a} deriving (Show, Generic)

instance Functor Line where
    fmap f (Line x y z) = Line (f x) (f y) (f z)

instance Applicative Line where
    pure x = Line x x x
    Line fx fy fz <*> Line x y z = Line (fx x) (fy y) (fz z)

instance Semigroup a => Semigroup (Line a) where
    Line x1 y1 z1 <> Line x2 y2 z2 = Line (x1 <> x2) (y1 <> y2) (z1 <> z2)
instance Foldable Line where
    foldMap f (Line x y z) = f x <> f y <> f z
instance Foldable Board where
    foldMap f (Board x y z) = foldMap f x <> foldMap f y <> foldMap f z

makeLenses ''Line
makeLenses ''Board

startingGame :: Game
startingGame = pure Nothing

winningLines :: [[Move]]
winningLines =
    [ [NW, N, NE]
    , [W, C, E]
    , [SW, S, SE]
    , [NW, C, SE]
    , [NE, C, SW]
    ]

instance Players Player where
    nextPlayer Nought = Cross
    nextPlayer Cross = Nought
    firstPlayer = Nought

data Move = NW | N | NE | W | C | E | SW | S | SE deriving (Generic, Show)

boardLens :: Move -> BoardLens a
boardLens move = case move of
    NW -> t . l
    N -> t . c
    NE -> t . r
    W -> m . l
    C -> m . c
    E -> m . r
    SW -> b . l
    S -> b . c
    SE -> b . r

data GameStatus = WonGame (Board FinishedSquare) | ContinuingGame Game | DrawnGame Game

data FinishedSquare = FinishedSquare Bool (Maybe Player) deriving (Show)

type WinningSquares = MarkedSquares
type MarkedSquares = Board Bool

noneMarked :: MarkedSquares
noneMarked = pure False

gameStatus :: Player -> Game -> GameStatus
gameStatus player game = case wonGame of
    Just winningSquares -> WonGame $ FinishedSquare <$> winningSquares <*> game
    Nothing -> if full then DrawnGame game else ContinuingGame game
  where
    wonGame :: Maybe WinningSquares
    wonGame =
        fmap2 getAny $
            foldMap (fmap2 Any . winningLine) winningLines
      where
        fmap2 = fmap . fmap
        winningLine :: [Move] -> Maybe WinningSquares
        winningLine = fmap ((&) noneMarked . appEndo) . foldMap (fmap Endo . setMarked)
          where
            setMarked :: Move -> Maybe (MarkedSquares -> MarkedSquares)
            setMarked move =
                if view (boardLens move) game == Just player
                    then Just $ set (boardLens move) True
                    else Nothing
    full = all isJust game

completionStatus :: GameStatus -> CompletionStatus
completionStatus (WonGame _) = Finished
completionStatus (DrawnGame _) = Finished
completionStatus (ContinuingGame _) = Playing

type GetMove f = Player -> Game -> f Move
type GetPlay f = Player -> Game -> f Game

moveStateful :: Monad f => GetPlay f -> Player -> StateT Game f GameStatus
moveStateful receiveMove player =
    modifyM (receiveMove player) *> gets (gameStatus player)

applyMove :: Player -> Game -> Move -> Maybe Game
applyMove player game move = boardLens move f game
  where
    f Nothing = Just $ Just player
    f _ = Nothing

play :: Monad f => (Player -> StateT Game f GameStatus) -> f ()
play moveStateful = evalStateT (playGame (fmap completionStatus . moveStateful)) startingGame
