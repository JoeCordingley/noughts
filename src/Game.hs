module Game
  ( CompletionStatus (..),
    playGame,
    Players (..),
    PlayMove,
  )
where

type PlayMove player f = player -> f CompletionStatus

data CompletionStatus = Finished | Playing

class Players player where
  firstPlayer :: player
  nextPlayer :: player -> player

playGame :: (Monad f, Players player) => PlayMove player f -> f ()
playGame playMove = play firstPlayer
  where
    play player = do
      game <- playMove player
      case game of
        Finished -> return ()
        Playing -> play (nextPlayer player)
