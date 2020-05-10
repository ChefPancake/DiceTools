namespace DiceCalculator.Application.ViewModels

open DiceCalculator.Application
open DiceCalculator.Domain.Core
open DiceCalculator.Storage.FileIO
open DiceCalculator.Profiles.Profile
open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.Threading

type BaseViewModel() =
    let propChangedEvent = 
        new Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    member internal this.RaisePropChanged propName =
        propChangedEvent.Trigger(this, new PropertyChangedEventArgs(propName))

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged: IEvent<PropertyChangedEventHandler,PropertyChangedEventArgs> = 
            propChangedEvent.Publish

type public DieViewModel() =
    inherit BaseViewModel()

    let mutable sides = 
        new ObservableCollection<string>()
    let mutable name =
        String.Empty
        
    static member internal FromNamedDie (named:NamedDie) =
        let mutable vm = new DieViewModel()

        let countString count =
            match count with
            | 1 -> ""
            | count -> String.Format("{0}x",count)

        let sideToString (dieSide:DieSide) =
            match dieSide.Symbols with
            | None -> ""
            | Some symbols ->
                NonEmptyList.groupBy id symbols
                |> NonEmptyList.map (fun (symbol, symbols) -> 
                    String.Format("{0}{1}", NonEmptyList.length symbols |> countString, Symbol.Value symbol |> NonEmptyString100.Value))
                |> NonEmptyList.toSeq
                |> String.concat "; "

        vm.Sides <-
            NonEmptyList.map (fun side -> sideToString side) (named.Die.Sides)
            |> NonEmptyList.toSeq
            |> (fun s -> new ObservableCollection<string>(s))
        vm.Name <- NonEmptyString100.Value named.Name
        vm

    member this.Name
        with get() = name
        and set value = 
            name <- value
            this.RaisePropChanged "Name"

    member this.Sides 
        with get() = sides
        and set value =
            sides <- value
            this.RaisePropChanged "Sides"

type public DiceProfileViewModel() =
    inherit BaseViewModel()

    let mutable symbolNames: ObservableCollection<string> =
        new ObservableCollection<string>()
    let mutable profileName: string = String.Empty
    let mutable dice =
        new ObservableCollection<DieViewModel>()
    
    static member internal FromProfile diceProfile =
        let mutable vm = new DiceProfileViewModel()
        let toObservableCollection (transform:'a->'b) items =
            items
            |> List.map transform
            |> (fun s -> new ObservableCollection<'b>(List.toSeq s))

        vm.Symbols <-
            toObservableCollection
                (Symbol.Value >> NonEmptyString100.Value)
                diceProfile.Symbols
        vm.Dice <-
            toObservableCollection
                DieViewModel.FromNamedDie
                diceProfile.Dice
        vm.ProfileName <-
            diceProfile.Name |> NonEmptyString100.Value
        vm

    member this.ProfileName
        with get() = profileName
        and private set value =
            profileName <- value
            this.RaisePropChanged "ProfileName"

    member this.Symbols
        with get () = symbolNames
        and private set value = 
            symbolNames <- value
            this.RaisePropChanged "Symbols"

    member this.Dice
        with get () = dice
        and private set value = 
            dice <- value
            this.RaisePropChanged "Dice"

type public MainViewModel() =
    inherit BaseViewModel()

    let mutable profiles = 
        new ObservableCollection<DiceProfileViewModel>()
    let mutable selectedProfile = null

    member this.Profiles
        with get() = profiles
        and set value =
            profiles <- value
            this.RaisePropChanged "Profiles"

    member this.SelectedProfile
        with get() = selectedProfile
        and set value = 
            selectedProfile <- value
            this.RaisePropChanged "SelectedProfile"

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
                this.Profiles.Add(DiceProfileViewModel.FromProfile(p)))
            //TODO: do something with errors
            |> ignore
        | _,_ ->
            //TODO: do something with errors
            () 

    member this.LoadCommand: UserCommand =
        new UserCommand(new Action(this.LoadProfile))
        