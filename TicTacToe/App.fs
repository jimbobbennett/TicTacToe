namespace TicTacToe

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
    | XWins 
    | OWins 
    | Draw

/// Represents an update to the game
type Msg =
    | PlayTL
    | PlayTC
    | PlayTR
    | PlayML
    | PlayMC
    | PlayMR
    | PlayBL
    | PlayBC
    | PlayBR
    | Restart

/// Represents the state of the game board
type Board = 
    { 
      TopLeft: GameCell
      TopCenter: GameCell
      TopRight: GameCell
      MiddleLeft: GameCell
      MiddleCenter: GameCell
      MiddleRight: GameCell
      BottomLeft: GameCell
      BottomCenter: GameCell
      BottomRight: GameCell
    }

/// Represents the elements of a possibly-winning row
type Row = 
    { 
      First: GameCell
      Second: GameCell
      Third: GameCell 
    }

/// Represents the state of the game
type Model =
    { 
      NextUp: Player
      Board: Board
    }

/// The model, update and view content of the app. This is placed in an 
/// independent model to facilitate unit testing.
module App = 

    let initialBoard = 
        { TopLeft = Empty; TopCenter = Empty; TopRight = Empty
          MiddleLeft = Empty; MiddleCenter = Empty; MiddleRight = Empty
          BottomLeft = Empty; BottomCenter = Empty; BottomRight = Empty }

    let init () = 
        { NextUp = X
          Board = initialBoard }

    /// Check if there are any more moves available in the game
    let anyMoreMoves board = 
        board.TopLeft.CanPlay || board.TopCenter.CanPlay || board.TopRight.CanPlay ||
        board.MiddleLeft.CanPlay || board.MiddleCenter.CanPlay || board.MiddleRight.CanPlay ||
        board.BottomLeft.CanPlay || board.BottomCenter.CanPlay || board.BottomRight.CanPlay
    
    let getWinLines m =
        [ // rows
          { First = m.TopLeft; Second = m.TopCenter; Third = m.TopRight }
          { First = m.MiddleLeft; Second = m.MiddleCenter; Third = m.MiddleRight }
          { First = m.BottomLeft; Second = m.BottomCenter; Third = m.BottomRight }

          // columns
          { First = m.TopLeft; Second = m.MiddleLeft; Third = m.BottomLeft }
          { First = m.TopCenter; Second = m.MiddleCenter; Third = m.BottomCenter }
          { First = m.TopRight; Second = m.MiddleRight; Third = m.BottomRight }

          // diagonals
          { First = m.TopLeft; Second = m.MiddleCenter; Third = m.BottomRight }
          { First = m.TopRight; Second = m.MiddleCenter; Third = m.BottomLeft }
        ]

    /// Determine if a line is a winning line.
    let getLineWinner l =
        match l.First, l.Second, l.Third with
        | Full X, Full X, Full X -> XWins
        | Full O, Full O, Full O -> OWins
        | _ -> StillPlaying
                    
    /// Determine the game result, if any.
    let getGameResult m =
        let winLines = getWinLines m.Board |> Seq.map getLineWinner

        let xWins = winLines |> Seq.tryFind (fun r -> r = XWins)
        match xWins with
        | Some p -> p
        | _ -> 

        let oWins = winLines |> Seq.tryFind (fun r -> r = OWins)
        match oWins with         
        | Some p -> p 
        | _ -> 

        match anyMoreMoves m.Board with
        | true -> StillPlaying
        | false -> Draw

    /// Get a message to show the current game result
    let getMessage model = 
        match getGameResult model with 
        | StillPlaying -> sprintf "%O's turn" model.NextUp
        | XWins -> "X wins!"
        | OWins -> "O Wins!"
        | Draw -> "It is a draw!"

    /// The 'update' function to update the model
    let update gameOver msg model =
        let newModel = 
            match msg with
            | PlayTL -> { model with Board = { model.Board with TopLeft = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayTC -> { model with Board = { model.Board with TopCenter = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayTR -> { model with Board = { model.Board with TopRight = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayML -> { model with Board = { model.Board with MiddleLeft = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayMC -> { model with Board = { model.Board with MiddleCenter = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayMR -> { model with Board = { model.Board with MiddleRight = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayBL -> { model with Board = { model.Board with BottomLeft = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayBC -> { model with Board = { model.Board with BottomCenter = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | PlayBR -> { model with Board = { model.Board with BottomRight = Full model.NextUp }; NextUp = model.NextUp.Swap }
            | Restart -> init()

        // Make an announcement in the middle of the game. 
        let result = getGameResult newModel
        if result <> StillPlaying then gameOver (getMessage newModel)

        // Return the new model.
        newModel

    /// A helper used in the 'view' function to get the name 
    /// of the Xaml resource for the image for a player
    let imageForPlayer player =
        match player with
        | X -> "Cross"
        | O -> "Nought"

    /// A condition used in the view function to checkif we can play in a cell.
    /// The visual contents of the cell depend on this condition.
    let canPlay model cell =
         match cell with 
         | Full _ -> false
         | Empty -> 
         match getGameResult model with
         | StillPlaying -> true
         | _ -> false

    /// The 'view' function giving the Xaml bindings from the model to the view
    let view () =
        TicTacToePage (), 
        [ "TurnMessage" |> Binding.oneWay getMessage
          "Restart" |> Binding.msg Restart
          "PlayTL" |> Binding.msg PlayTL
          "PlayTC" |> Binding.msg PlayTC
          "PlayTR" |> Binding.msg PlayTR
          "PlayML" |> Binding.msg PlayML
          "PlayMC" |> Binding.msg PlayMC
          "PlayMR" |> Binding.msg PlayMR
          "PlayBL" |> Binding.msg PlayBL
          "PlayBC" |> Binding.msg PlayBC
          "PlayBR" |> Binding.msg PlayBR
          "CanPlayTL" |> Binding.oneWay (fun m -> canPlay m m.Board.TopLeft )
          "CanPlayTC" |> Binding.oneWay (fun m -> canPlay m m.Board.TopCenter )
          "CanPlayTR" |> Binding.oneWay (fun m -> canPlay m m.Board.TopRight)
          "CanPlayML" |> Binding.oneWay (fun m -> canPlay m m.Board.MiddleLeft )
          "CanPlayMC" |> Binding.oneWay (fun m -> canPlay m m.Board.MiddleCenter )
          "CanPlayMR" |> Binding.oneWay (fun m -> canPlay m m.Board.MiddleRight )
          "CanPlayBL" |> Binding.oneWay (fun m -> canPlay m m.Board.BottomLeft )
          "CanPlayBC" |> Binding.oneWay (fun m -> canPlay m m.Board.BottomCenter )
          "CanPlayBR" |> Binding.oneWay (fun m -> canPlay m m.Board.BottomRight )
          "ImageTL" |> Binding.oneWay (fun m -> match m.Board.TopLeft with Empty -> "" | Full p -> imageForPlayer p )
          "ImageTC" |> Binding.oneWay (fun m -> match m.Board.TopCenter with Empty -> "" | Full p -> imageForPlayer p )
          "ImageTR" |> Binding.oneWay (fun m -> match m.Board.TopRight with Empty -> "" | Full p -> imageForPlayer p )
          "ImageML" |> Binding.oneWay (fun m -> match m.Board.MiddleLeft with Empty -> "" | Full p -> imageForPlayer p )
          "ImageMC" |> Binding.oneWay (fun m -> match m.Board.MiddleCenter with Empty -> "" | Full p -> imageForPlayer p )
          "ImageMR" |> Binding.oneWay (fun m -> match m.Board.MiddleRight with Empty -> "" | Full p -> imageForPlayer p )
          "ImageBL" |> Binding.oneWay (fun m -> match m.Board.BottomLeft with Empty -> "" | Full p -> imageForPlayer p )
          "ImageBC" |> Binding.oneWay (fun m -> match m.Board.BottomCenter with Empty -> "" | Full p -> imageForPlayer p )
          "ImageBR" |> Binding.oneWay (fun m -> match m.Board.BottomRight with Empty -> "" | Full p -> imageForPlayer p )
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
        
    do base.MainPage <- new NavigationPage(page, BarBackgroundColor = Color.LightBlue)

