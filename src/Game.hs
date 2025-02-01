module Game (
    CompletionStatus (..),
    playGame,
    PlayMove,
)
where

type PlayMove player f = player -> f (CompletionStatus player)

data CompletionStatus player = Finished | Playing player

playGame :: Monad f => PlayMove player f -> player -> f ()
playGame playMove firstPlayer = play firstPlayer
  where
    play player = do
        game <- playMove player
        case game of
            Finished -> return ()
            Playing nextPlayer -> play nextPlayer
