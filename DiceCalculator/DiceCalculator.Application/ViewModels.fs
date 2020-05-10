namespace DiceCalculator.Application.ViewModels

open DiceCalculator.Application
open DiceCalculator.Domain.Core
open DiceCalculator.Storage.FileIO
open DiceCalculator.Profiles.Profile
open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.Threading

type public MainViewModel() =
    let propChangedEvent = 
        new Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    let mutable profile: DiceProfile option = None
    let mutable symbolNames: ObservableCollection<string> =
        new ObservableCollection<string>()
    
    member this.Symbols
        with get () = symbolNames
        and private set (value) = 
            symbolNames <- value
            this.RaisePropChanged "Symbols"
    
    member private this.LoadProfile () =
        let filePath = 
            FilePath.Create @"C:\Users\kenda\Documents\ProfileTest.dcp"
        let fileName = 
            Result.bind FileName.FromFilePath filePath
        let profileName =
            Result.map FileName.Value fileName
            |> Result.bind NonEmptyString100.Create
        match filePath, profileName with
        | Ok filePath, Ok profileName ->
            ReadLinesFromFileAsync CancellationToken.None filePath
            |> Async.RunSynchronously
            |> Result.map Array.toList
            |> Result.bind (LoadProfileFromText profileName)
            |> Result.map (fun p -> 
                profile <- Some p
                this.Symbols <-
                    p.Symbols
                    |> List.map (fun s -> s |> (Symbol.Value >> NonEmptyString100.Value))
                    |> (fun s -> new ObservableCollection<string>(s |> List.toSeq))
                )
            //TODO: do something with errors
            |> ignore
        | _,_ ->
            //TODO: do something with errors
            () 
            //Error "Unable to load profile, invalid file path"

    member private this.RaisePropChanged propName =
        propChangedEvent.Trigger(this, new PropertyChangedEventArgs(propName))

    member this.LoadCommand: UserCommand =
        new UserCommand(new Action(this.LoadProfile))

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged: IEvent<PropertyChangedEventHandler,PropertyChangedEventArgs> = 
            propChangedEvent.Publish

        


            
            