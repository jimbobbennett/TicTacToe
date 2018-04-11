namespace TicTacToe

open Elmish
open Elmish.XamarinForms
open Xamarin.Forms

type Msg =
    | XWins
    | OWins
    | Draw
    | PlayTL
    | PlayTC
    | PlayTR
    | PlayML
    | PlayMC
    | PlayMR
    | PlayBL
    | PlayBC
    | PlayBR

type Location = TL | TC | TR | ML | MC | MR | BL | BC | BR

type Board = 
    { 
        TopLeft : GameCell; TopCenter : GameCell; TopRight : GameCell; 
        MiddleLeft : GameCell; MiddleCenter : GameCell; MiddleRight : GameCell; 
        BottomLeft : GameCell; BottomCenter : GameCell; BottomRight : GameCell; 
    }

type Model =
    {
        Turn : Player
        Cells : Board
    }


type App() =
    inherit Application()

    let makePlayerMove m l =
            match l with
            | TL -> { m with Cells = {m.Cells with TopLeft = Full m.Turn }; Turn = Player.Swap m.Turn }
            | TC -> { m with Cells = {m.Cells with TopCenter = Full m.Turn }; Turn = Player.Swap m.Turn }
            | TR -> { m with Cells = {m.Cells with TopRight = Full m.Turn }; Turn = Player.Swap m.Turn }
            | ML -> { m with Cells = {m.Cells with MiddleLeft = Full m.Turn }; Turn = Player.Swap m.Turn }
            | MC -> { m with Cells = {m.Cells with MiddleCenter = Full m.Turn }; Turn = Player.Swap m.Turn }
            | MR -> { m with Cells = {m.Cells with MiddleRight = Full m.Turn }; Turn = Player.Swap m.Turn }
            | BL -> { m with Cells = {m.Cells with BottomLeft = Full m.Turn }; Turn = Player.Swap m.Turn }
            | BC -> { m with Cells = {m.Cells with BottomCenter = Full m.Turn }; Turn = Player.Swap m.Turn }
            | BR -> { m with Cells = {m.Cells with BottomRight = Full m.Turn }; Turn = Player.Swap m.Turn }

    let init () = 
        { 
            Turn = X; 
            Cells = {
                        TopLeft = Empty; TopCenter = Empty; TopRight = Empty;
                        MiddleLeft = Empty; MiddleCenter = Empty; MiddleRight = Empty; 
                        BottomLeft = Empty; BottomCenter = Empty; BottomRight = Empty;
                    }
        }

    let update msg model =
        match msg with
        | XWins -> model
        | OWins -> model
        | Draw -> model
        | PlayTL -> makePlayerMove model TL
        | PlayTC -> makePlayerMove model TC
        | PlayTR -> makePlayerMove model TR
        | PlayML -> makePlayerMove model ML
        | PlayMC -> makePlayerMove model MC
        | PlayMR -> makePlayerMove model MR
        | PlayBL -> makePlayerMove model BL
        | PlayBC -> makePlayerMove model BC
        | PlayBR -> makePlayerMove model BR

    let view () =
            TicTacToePage (), 
            [
                "TurnMessage" |> Binding.oneWay (fun m -> sprintf "%O's turn" m.Turn)
                "PlayTL" |> Binding.msg PlayTL
                "PlayTC" |> Binding.msg PlayTC
                "PlayTR" |> Binding.msg PlayTR
                "PlayML" |> Binding.msg PlayML
                "PlayMC" |> Binding.msg PlayMC
                "PlayMR" |> Binding.msg PlayMR
                "PlayBL" |> Binding.msg PlayBL
                "PlayBC" |> Binding.msg PlayBC
                "PlayBR" |> Binding.msg PlayBR
                "CellTL" |> Binding.oneWay (fun m -> m.Cells.TopLeft)
                "CellTC" |> Binding.oneWay (fun m -> m.Cells.TopCenter)
                "CellTR" |> Binding.oneWay (fun m -> m.Cells.TopRight)
            ]

    do
        let page = Program.mkSimple init update (fun _ _ -> view())
                    |> Program.withConsoleTrace
                    |> Program.run

        base.MainPage <- page
