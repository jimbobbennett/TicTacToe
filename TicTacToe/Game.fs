namespace TicTacToe

type Player = X | O with static member Swap = function X -> O | O -> X
type GameCell = Empty | Full of Player
