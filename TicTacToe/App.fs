namespace TicTacToe

open Elmish
open Elmish.XamarinForms
open Xamarin.Forms

type Player = X | O with static member Swap = function X -> O | O -> X
type GameCell = Empty | Full of Player
type GameResult = StillPlaying | XWins | OWins | Draw

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

type Board = 
    { 
        TopLeft : GameCell; TopCenter : GameCell; TopRight : GameCell; 
        MiddleLeft : GameCell; MiddleCenter : GameCell; MiddleRight : GameCell; 
        BottomLeft : GameCell; BottomCenter : GameCell; BottomRight : GameCell; 
    }

type Row = { First : GameCell; Second : GameCell; Third : GameCell; }

type Model =
    {
        Turn : Player
        Cells : Board
    }

type App() =
    inherit Application()

    let init () = 
        { 
            Turn = X; 
            Cells = {
                        TopLeft = Empty; TopCenter = Empty; TopRight = Empty;
                        MiddleLeft = Empty; MiddleCenter = Empty; MiddleRight = Empty; 
                        BottomLeft = Empty; BottomCenter = Empty; BottomRight = Empty;
                    }
        }

    let canPlayRow x y z = x = Empty || y = Empty || z = Empty

    let anyMoreMoves m = 
        canPlayRow m.Cells.TopLeft m.Cells.TopCenter m.Cells.TopRight ||
        canPlayRow m.Cells.MiddleLeft m.Cells.MiddleCenter m.Cells.MiddleRight ||
        canPlayRow m.Cells.BottomLeft m.Cells.BottomCenter m.Cells.BottomRight
    
    let getWinLines m =
        [
            // rows
            { First = m.TopLeft; Second = m.TopCenter; Third = m.TopRight; }
            { First = m.MiddleLeft; Second = m.MiddleCenter; Third = m.MiddleRight; }
            { First = m.BottomLeft; Second = m.BottomCenter; Third = m.BottomRight; }

            // columns
            { First = m.TopLeft; Second = m.MiddleLeft; Third = m.BottomLeft; }
            { First = m.TopCenter; Second = m.MiddleCenter; Third = m.BottomCenter; }
            { First = m.TopRight; Second = m.MiddleRight; Third = m.BottomRight; }

            // diagonals
            { First = m.TopLeft; Second = m.MiddleCenter; Third = m.BottomRight; }
            { First = m.TopRight; Second = m.MiddleCenter; Third = m.BottomLeft; }
        ]

    let getLineWinner l =
        match l.First, l.Second, l.Third with
        | Full(X), Full(X), Full(X) -> XWins
        | Full(O), Full(O), Full(O) -> OWins
        | _ -> StillPlaying
                    
    let getGameResult m =
        let winLines = getWinLines m.Cells |> Seq.map getLineWinner
        let x = winLines |> Seq.tryFind (fun r -> r = XWins)
        let o = winLines |> Seq.tryFind (fun r -> r = OWins)

        match x with
        | Some p -> p
        | _ -> match o with         
               | Some p -> p 
               | _ -> match anyMoreMoves m with
                      | true -> StillPlaying
                      | false -> Draw

    let getMessage m = 
        match getGameResult m with 
        | StillPlaying -> sprintf "%O's turn" m.Turn
        | XWins -> "X wins!"
        | OWins -> "O Wins!"
        | Draw -> "It is a draw!"

    let announce m =
        App.Current.MainPage.DisplayAlert("Game over", m, "OK") |> ignore

    let imageForPlayer p =
        match p with
        | X -> "Cross"
        | O -> "Nought"

    let update msg m =
        let model = match msg with
                    | PlayTL -> { m with Cells = {m.Cells with TopLeft = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayTC -> { m with Cells = {m.Cells with TopCenter = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayTR -> { m with Cells = {m.Cells with TopRight = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayML -> { m with Cells = {m.Cells with MiddleLeft = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayMC -> { m with Cells = {m.Cells with MiddleCenter = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayMR -> { m with Cells = {m.Cells with MiddleRight = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayBL -> { m with Cells = {m.Cells with BottomLeft = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayBC -> { m with Cells = {m.Cells with BottomCenter = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | PlayBR -> { m with Cells = {m.Cells with BottomRight = Full m.Turn }; Turn = Player.Swap m.Turn }
                    | Restart -> init()
        let result = getGameResult model
        if (result <> StillPlaying) then announce (getMessage model)
        model

    let canPlay m c =
         match c with 
         | Full _ -> false
         | Empty -> match getGameResult m with
                    | StillPlaying -> true
                    | _ -> false

    let view () =
            TicTacToePage (), 
            [
                "TurnMessage" |> Binding.oneWay (fun m -> getMessage m)
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
                "CanPlayTL" |> Binding.oneWay (fun m -> canPlay m m.Cells.TopLeft )
                "CanPlayTC" |> Binding.oneWay (fun m -> canPlay m m.Cells.TopCenter )
                "CanPlayTR" |> Binding.oneWay (fun m -> canPlay m m.Cells.TopRight)
                "CanPlayML" |> Binding.oneWay (fun m -> canPlay m m.Cells.MiddleLeft )
                "CanPlayMC" |> Binding.oneWay (fun m -> canPlay m m.Cells.MiddleCenter )
                "CanPlayMR" |> Binding.oneWay (fun m -> canPlay m m.Cells.MiddleRight )
                "CanPlayBL" |> Binding.oneWay (fun m -> canPlay m m.Cells.BottomLeft )
                "CanPlayBC" |> Binding.oneWay (fun m -> canPlay m m.Cells.BottomCenter )
                "CanPlayBR" |> Binding.oneWay (fun m -> canPlay m m.Cells.BottomRight )
                "ImageTL" |> Binding.oneWay (fun m -> match m.Cells.TopLeft with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageTC" |> Binding.oneWay (fun m -> match m.Cells.TopCenter with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageTR" |> Binding.oneWay (fun m -> match m.Cells.TopRight with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageML" |> Binding.oneWay (fun m -> match m.Cells.MiddleLeft with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageMC" |> Binding.oneWay (fun m -> match m.Cells.MiddleCenter with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageMR" |> Binding.oneWay (fun m -> match m.Cells.MiddleRight with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageBL" |> Binding.oneWay (fun m -> match m.Cells.BottomLeft with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageBC" |> Binding.oneWay (fun m -> match m.Cells.BottomCenter with | Empty -> "" | Full p -> imageForPlayer p )
                "ImageBR" |> Binding.oneWay (fun m -> match m.Cells.BottomRight with | Empty -> "" | Full p -> imageForPlayer p )
            ]

    do
        let page = Program.mkSimple init update (fun _ _ -> view())
                    |> Program.withConsoleTrace
                    |> Program.run

        let navPage = new NavigationPage(page)
        navPage.BarBackgroundColor <- Color.LightBlue
        navPage.BarTextColor <- Color.Black
        base.MainPage <- navPage

