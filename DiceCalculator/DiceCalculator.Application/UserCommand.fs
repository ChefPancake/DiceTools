namespace DiceCalculator.Application

open System.Windows.Input
open System

type UserCommand (action:Action) =
    let canExecuteChangedEvent = new Event<EventHandler, EventArgs>()

    interface ICommand with
        member this.CanExecute(parameter: obj): bool = true
        [<CLIEvent>]
        member this.CanExecuteChanged: IEvent<EventHandler,EventArgs> = 
            canExecuteChangedEvent.Publish
        member this.Execute(parameter: obj): unit = 
            match action = null with
            | true -> ()
            | false -> action.Invoke()
        
