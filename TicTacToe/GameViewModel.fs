namespace TicTacToe

open System.ComponentModel
open System.Windows.Input

type GameViewModel () =
    let ev = new Event<_,_>()
    let gameState = GameState()

    let createCommand action canExecute=
                let event1 = Event<_, _>()
                {
                    new ICommand with
                        member this.CanExecute(obj) = canExecute(obj)
                        member this.Execute(obj) = action(obj)
                        member this.add_CanExecuteChanged(handler) = event1.Publish.AddHandler(handler)
                        member this.remove_CanExecuteChanged(handler) = event1.Publish.AddHandler(handler)
                }

    interface INotifyPropertyChanged with 
        [<CLIEvent>]
        member x.PropertyChanged = ev.Publish

    member this.TurnMessage = 
        sprintf "%O's turn" gameState.Turn

    member this.MakeMoveCommand = createCommand
                                    (fun p -> gameState.MakeMove (p :?> Location))
                                    (fun _ -> true)
                