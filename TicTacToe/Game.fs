namespace TicTacToe

type Player = X | O with static member Swap = function X -> O | O -> X
type GameCell = Empty | Full of Player
//type XLocation = Left | Center | Right
//type YLocation = Top | Middle | Bottom

//type Location = TL | TC | TR | ML | MC | MR | BL | BC | BR

//type GameState() =
    //let mutable turn = X
    //let cells = (
    //                (ref Empty, ref Empty, ref Empty),
    //                (ref Empty, ref Empty, ref Empty),
    //                (ref Empty, ref Empty, ref Empty)
    //            )

    //let pos1 (x, _, _) = x
    //let pos2 (_, y, _) = y
    //let pos3 (_, _, z) = z

    //let makePlayerMove p x y =
    //    let row = match y with 
    //              | Top -> cells |> pos1
    //              | Middle -> cells |> pos2
    //              | Bottom -> cells |> pos3 
    //    let cell = match x with
    //               | Left -> row |> pos1
    //               | Center -> row |> pos2
    //               | Right -> row |> pos3
    //    cell := Full p
    //    ()

    //let makeMove (s:Location) =
    //    match s with
    //    | TL -> makePlayerMove turn Left Top
    //    | TC -> makePlayerMove turn Center Top
    //    | TR -> makePlayerMove turn Right Top
    //    | ML -> makePlayerMove turn Left Middle
    //    | MC -> makePlayerMove turn Center Middle
    //    | MR -> makePlayerMove turn Right Middle
    //    | BL -> makePlayerMove turn Left Bottom
    //    | BC -> makePlayerMove turn Center Bottom
    //    | BR -> makePlayerMove turn Right Bottom
    //    turn <- Player.Swap turn

    //member this.Cells = cells

    //member this.Turn
    //    with get() = turn
    //    and set(value) = turn <- value

    //member this.MakeMove s =
        //makeMove s
