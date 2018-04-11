namespace TicTacToe

open System
open System.Windows.Input
open Xamarin.Forms

type CellControl () =
    inherit ContentView()

    let buildImage s =
        let i = new Image()
        i.Source <- ImageSource.FromResource s
        i.Aspect <- Aspect.AspectFit
        i.IsVisible <- false
        i

    let button = new Button()
    let noughtImage = buildImage "Nought"
    let crossImage = buildImage "Cross"

    do
        button.BackgroundColor <- Color.LightGray

        let grid = new Grid()
        grid.Children.Add button
        grid.Children.Add noughtImage
        grid.Children.Add crossImage

    let updateFromPlayer p (i : Image) x o =
        match p with
        | X -> i.IsVisible <- x
        | O -> i.IsVisible <- o

    member this.UpdateFromGameCell g =
        match g with
        | Empty _ -> noughtImage.IsVisible <- false
                     crossImage.IsVisible <- false
                     button.IsVisible <- true
        | Full p -> updateFromPlayer p noughtImage true false
                    updateFromPlayer p crossImage false true 

    static member OnGameCellChanged (bindable : BindableObject) (oldValue : obj) (newValue : obj) : unit =
        let control = bindable :?> CellControl
        let n = newValue :?> GameCell
        control.UpdateFromGameCell n
        ()

    static member GameCellProperty = BindableProperty.Create ("GameCell", typeof<GameCell>, typeof<CellControl>, 
                                                              Empty, BindingMode.OneWay, Unchecked.defaultof<BindableProperty.ValidateValueDelegate>, 
                                                              new BindableProperty.BindingPropertyChangedDelegate(CellControl.OnGameCellChanged), 
                                                              Unchecked.defaultof<BindableProperty.BindingPropertyChangingDelegate>, Unchecked.defaultof<BindableProperty.CoerceValueDelegate>)
    static member CommandProperty = BindableProperty.Create ("Command", typeof<ICommand>, typeof<CellControl>, Unchecked.defaultof<ICommand>)


    member this.GameCell
        with get() : GameCell = this.GetValue CellControl.GameCellProperty :?> GameCell
        and set(value: GameCell) = 
            this.SetValue(CellControl.GameCellProperty, value)
            this.UpdateFromGameCell value

    member this.Command
        with get() : ICommand = this.GetValue CellControl.CommandProperty :?> ICommand
        and set(value : ICommand) = 
            this.SetValue(CellControl.CommandProperty, value)
            button.Command <- value
