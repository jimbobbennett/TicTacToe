namespace TicTacToe

open Xamarin.Forms

type InvertedBooleanConverter () =
    interface IValueConverter with 
        member this.Convert(value,targetType,parameter,culture) =
            let b = value :?> bool
            not b :> obj

        member this.ConvertBack(value,targetType,parameter,culture) =
            let b = value :?> bool
            not b :> obj