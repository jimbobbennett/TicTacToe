namespace TicTacToe

open Elmish
open Elmish.XamarinForms
open Xamarin.Forms

type Player = 
    | X 
    | O 
    member x.Swap = match x with X -> O | O -> X

type GameCell = 
    | Empty 
    | Full of Player

type GameResult = 
    | StillPlaying 
    | XWins 
    | OWins 
    | Draw

type Pos = int * int
type Msg =
    | Play of Pos
    | Restart

type Board = Map<Pos, GameCell>

type Row = GameCell list

type Model =
    { Turn : Player
      Cells : Board }

type App() =
    inherit Application()

    let positions = [ for x in 0 .. 2 do for y in 0 .. 2 do yield (x, y) ]
    let init () = 
        { 
            Turn = X; 
            Cells = Map.ofList [ for p in positions -> p, Empty ]
        }

    let canPlayRow x y z = x = Empty || y = Empty || z = Empty

    let anyMoreMoves m = m.Cells |> Map.exists (fun _ c -> c = Empty)
    
    let getWinLines () =
        [
            // rows
            for row in 0 .. 2 do yield [(row,0); (row,1); (row,2)]
            // columns
            for col in 0 .. 2 do yield [(0,col); (1,col); (2,col)]
            // diagonals
            yield [(0,0); (1,1); (2,2)]
            yield [(0,2); (1,1); (2,0)]
        ]

    let getLineWinner (cells: Board) line =
        if line |> List.forall (fun p -> match cells.[p] with Full X -> true | _ -> false) then  XWins
        elif line |> List.forall (fun p -> match cells.[p] with Full O -> true | _ -> false) then  OWins
        else StillPlaying
                    
    let getGameResult m =
        let winLines = getWinLines () |> Seq.map (getLineWinner m.Cells)
        let x = winLines |> Seq.tryFind (fun r -> r = XWins)
        let o = winLines |> Seq.tryFind (fun r -> r = OWins)

        match x with
        | Some p -> p
        | _ -> 
        match o with         
        | Some p -> p 
        | _ -> 
        match anyMoreMoves m with
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

    let fillTile m f = 
        { m with Cells = f m.Cells (Full m.Turn); Turn = m.Turn.Swap }

    let update msg m =
        let model = 
            match msg with
            | Play pos -> { m with Cells = m.Cells.Add(pos, Full m.Turn); Turn = m.Turn.Swap }
            | Restart -> init()
        let result = getGameResult model
        if (result <> StillPlaying) then announce (getMessage model)
        model

    let canPlay m c =
        match c with 
        | Full _ -> false
        | Empty -> 
        match getGameResult m with
        | StillPlaying -> true
        | _ -> false

    let uiText (row,col) = 
        (match row with 0 -> "T" | 1 -> "M" | 2 -> "B" | _ -> failwith "huh?") + 
        (match col with 0 -> "L" | 1 -> "C" | 2 -> "R" | _ -> failwith "huh?")

    let view () =
            TicTacToePage (), 
            [ yield "TurnMessage" |> Binding.oneWay (fun m -> getMessage m)
              yield "Restart" |> Binding.msg Restart
              for pos in positions do 
                  yield ("Play" + uiText pos) |> Binding.msg (Play pos)
                  yield ("CanPlay" + uiText pos) |> Binding.oneWay (fun m -> canPlay m m.Cells.[pos] )
                  yield ("Image" + uiText pos) |> Binding.oneWay (fun m -> match m.Cells.[pos] with | Empty -> "" | Full p -> imageForPlayer p )
            ]

    do
        let page = 
            Program.mkSimple init update (fun _ _ -> view())
            |> Program.withConsoleTrace
            |> Program.run

        let navPage = new NavigationPage(page)
        navPage.BarBackgroundColor <- Color.LightBlue
        navPage.BarTextColor <- Color.Black
        base.MainPage <- navPage

