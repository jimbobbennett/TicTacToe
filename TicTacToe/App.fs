﻿namespace TicTacToe

open Elmish
open Elmish.XamarinForms
open Xamarin.Forms

/// Represents a player and a player's move
type Player = 
    | X 
    | O 
    member p.Swap = match p with X -> O | O -> X

/// Represents the game state contents of a single cell
type GameCell = 
    | Empty 
    | Full of Player
    member x.CanPlay = (x = Empty)

/// Represents the result of a game
type GameResult = 
    | StillPlaying 
    | Win of Player
    | Draw

/// Represents a position on the board
type Pos = int * int

/// Represents an update to the game
type Msg =
    | Play of Pos
    | Restart

/// Represents the state of the game board
type Board = Map<Pos, GameCell>

/// Represents the elements of a possibly-winning row
type Row = GameCell list

/// Represents the state of the game
type Model =
    { 
      NextUp: Player
      Board: Board
    }

/// The model, update and view content of the app. This is placed in an 
/// independent model to facilitate unit testing.
module App = 

    let positions = 
        [ for x in 0 .. 2 do 
            for y in 0 .. 2 do 
               yield (x, y) ]

    let initialBoard = 
        Map.ofList [ for p in positions -> p, Empty ]

    let init () = 
        { NextUp = X
          Board = initialBoard }

    /// Check if there are any more moves available in the game
    let anyMoreMoves m = m.Board |> Map.exists (fun _ c -> c = Empty)
    
    let lines =
        [
            // rows
            for row in 0 .. 2 do yield [(row,0); (row,1); (row,2)]
            // columns
            for col in 0 .. 2 do yield [(0,col); (1,col); (2,col)]
            // diagonals
            yield [(0,0); (1,1); (2,2)]
            yield [(0,2); (1,1); (2,0)]
        ]

    /// Determine if a line is a winning line.
    let getLine (board: Board) line =
        line |> List.map (fun p -> board.[p])

    /// Determine if a line is a winning line.
    let getLineWinner line =
        if line |> List.forall (function Full X -> true | _ -> false) then Some X
        elif line |> List.forall (function Full O -> true | _ -> false) then Some O
        else None

    /// Determine the game result, if any.
    let getGameResult model =
        match lines |> Seq.tryPick (getLine model.Board >> getLineWinner) with
        | Some p -> Win p
        | _ -> 
           if anyMoreMoves model then StillPlaying
           else Draw

    /// Get a message to show the current game result
    let getMessage model = 
        match getGameResult model with 
        | StillPlaying -> sprintf "%O's turn" model.NextUp
        | Win p -> sprintf "%O wins!" p
        | Draw -> "It is a draw!"

    /// The 'update' function to update the model
    let update gameOver msg model =
        let newModel = 
            match msg with
            | Play pos -> 
                { model with Board = model.Board.Add(pos, Full model.NextUp)
                             NextUp = model.NextUp.Swap }
            | Restart -> 
                init()

        // Make an announcement in the middle of the game. 
        let result = getGameResult newModel
        if result <> StillPlaying then 
            gameOver (getMessage newModel)

        // Return the new model.
        newModel

    /// A helper used in the 'view' function to get the name 
    /// of the Xaml resource for the image for a player
    let imageForPlayer player =
        match player with
        | X -> "Cross"
        | O -> "Nought"

    /// A helper to get the suffix used in the Xaml for a position on the board.
    let uiText (row,col) = sprintf "%d%d" row col

    /// A condition used in the 'view' function to check if we can play in a cell.
    /// The visual contents of a cell depends on this condition.
    let canPlay model cell = (cell = Empty) && (getGameResult model = StillPlaying)

    /// The 'view' function giving the Xaml bindings from the model to the view
    let view () =
        TicTacToePage (), 
        [ yield "TurnMessage" |> Binding.oneWay (fun m -> getMessage m)
          yield "Restart" |> Binding.msg Restart
          for pos in positions do 
              yield ("Play" + uiText pos) |> Binding.msg (Play pos)
              yield ("CanPlay" + uiText pos) |> Binding.oneWay (fun m -> canPlay m m.Board.[pos] )
              yield ("Image" + uiText pos) |> Binding.oneWay (fun m -> match m.Board.[pos] with | Empty -> "" | Full p -> imageForPlayer p )
        ]


/// Stitch the model, update and view content into a single app.
type App() =
    inherit Application()

    // Display a modal message giving the game result. This is doing a UI
    // action in the model update, which is ok for modal messages. We factor
    // this dependency out to allow unit testing of the 'update' function. 
    let gameOver msg =
        Application.Current.MainPage.DisplayAlert("Game over", msg, "OK") |> ignore

    let page = 
        Program.mkSimple App.init (App.update gameOver) (fun _ _ -> App.view())
        |> Program.withConsoleTrace
        |> Program.run
        
    do base.MainPage <- new NavigationPage(page, BarBackgroundColor = Color.LightBlue, BarTextColor = Color.Black)

