namespace TicTacToe

open Xamarin.Forms
open Xamarin.Forms.Xaml

type TicTacToePage() =
    inherit ContentPage()
    let _ = base.LoadFromXaml(typeof<TicTacToePage>)

    override this.OnSizeAllocated(width, height) =
        base.OnSizeAllocated(width, height)
        let gameBoard : Grid = this.FindByName("GameBoard")
       
        gameBoard.HeightRequest <- (width - 80.)
        gameBoard.WidthRequest <- (width - 80.)

