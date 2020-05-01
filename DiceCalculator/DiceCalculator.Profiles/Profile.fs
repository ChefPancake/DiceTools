namespace DiceCalculator.Profiles

open DiceCalculator.Domain.Core
open System
open System.IO

module Profile =
    type NamedDie = {
        Name:NonEmptyString100
        Die: Die
    }

    type DiceProfile = {
        Name: NonEmptyString100
        Symbols: Symbol list
        Dice: NamedDie list
    }

    type private ParsedSymbols = { 
        ParsedSymbols: Symbol list
    }
    type private ParsedSymbolsAndDice = {
        ParsedSymbols: Symbol list
        ParsedDice: NamedDie list
    }
    type private ParsedSymbolsAndDiceName = {
        ParsedSymbols: Symbol list
        ParsedDice: NamedDie list
        ParsedDieName: NonEmptyString100
    }

    type private ParserState = 
    | ReadingSymbolsHeader
    | ReadingSymbols of ParsedSymbols
    | ReadingDiceName of ParsedSymbolsAndDice
    | ReadingDiceSides of ParsedSymbolsAndDiceName
    | Complete of DiceProfile

    let private ReadHeader line =
        match line.Equals "Symbols:" with
        | true -> 
            { ParsedSymbols.ParsedSymbols = [] } 
            |> ReadingSymbols
            |> Ok
        | false ->
            Error "Failed to parse symbols"

    let private ReadSymbols line (symbols:ParsedSymbols) =
        match line with
        | x when x.Equals "Dice:" -> 
            {
                ParsedSymbolsAndDice.ParsedSymbols = symbols.ParsedSymbols
                ParsedDice = []
            }
            |> ReadingDiceName
            |> Ok
        | x when String.IsNullOrWhiteSpace x ->
            symbols
            |> ReadingSymbols
            |> Ok
        | x ->
            NonEmptyString100.Create x
            |> Result.map (fun symbolName -> 
                {
                    ParsedSymbols.ParsedSymbols = 
                        List.singleton (Symbol symbolName)
                        |> List.append symbols.ParsedSymbols
                }
                |> ReadingSymbols)

    let private ReadDiceName (line:string) (symbolsAndDice: ParsedSymbolsAndDice) profileName =
        match line with
        | x when x.StartsWith '[' ->
            Error "Missing dice name while parsing"
        | x when String.IsNullOrWhiteSpace x ->
            {
                DiceProfile.Name = profileName
                DiceProfile.Dice = symbolsAndDice.ParsedDice
                DiceProfile.Symbols = symbolsAndDice.ParsedSymbols
            } 
            |> ParserState.Complete
            |> Ok
        | x ->
            NonEmptyString100.Create x
            |> Result.map (fun dieName -> 
            {
                ParsedSymbolsAndDiceName.ParsedDieName = dieName
                ParsedSymbols = symbolsAndDice.ParsedSymbols
                ParsedDice = symbolsAndDice.ParsedDice
            }
            |> ReadingDiceSides)

    //TODO: clean this guy up
    let private ReadDice (line:string) symbolsAndDiceName =
        match line with
        | x when x.StartsWith '[' && x.EndsWith ']' ->
            let sides = line.TrimStart('[').TrimEnd(']').Split(";")
            let parseSymbol (sym:string) =
                NonEmptyString100.Create sym
                |> Result.map Symbol
            let parseSide (side:string) =
                let symbols = 
                    side.TrimStart('[').TrimEnd(']').Split(",")
                    |> Array.map parseSymbol
                let symbolOptions =
                    match Array.length symbols with 
                    | 0 -> None
                    | _ ->
                        let hasErrors = 
                            Array.filter 
                                (fun y -> 
                                    match y with
                                    | Ok _ -> false
                                    | Error _ -> true) 
                                symbols
                            |> Array.length
                            |> (fun y -> y > 0)
                        match hasErrors with 
                        | false ->
                            let symbolList =
                                Array.choose 
                                    (fun y ->
                                        match y with
                                        | Ok sym -> sym |> Some
                                        | Error _ -> None)
                                    symbols
                                |> Array.toList
                            match NonEmptyList.Create symbolList with
                            | Ok symbolList ->
                                Some symbolList
                            | Error _ -> None
                        | true ->
                            None
                {
                    DieSide.Symbols = symbolOptions
                } 
            Array.map parseSide sides
            |> Array.toList
            |> NonEmptyList.Create
            |> Result.map (fun x ->
                {
                    Die.Sides = x
                })
            |> Result.map (fun x ->
                let newDie =
                    {
                        NamedDie.Name = symbolsAndDiceName.ParsedDieName
                        NamedDie.Die = x
                    }
                let newDice =
                    List.singleton newDie
                    |> List.append symbolsAndDiceName.ParsedDice
                {
                    ParsedSymbolsAndDice.ParsedSymbols = symbolsAndDiceName.ParsedSymbols
                    ParsedSymbolsAndDice.ParsedDice = newDice
                }
                |> ParserState.ReadingDiceName)
        | _ -> Error "Error parsing die"

    let private ParseNextState profileName state (line:string) =
        let trimmed = line.Trim()
        Result.bind (fun state -> 
            match state with
            | ReadingSymbolsHeader -> 
                ReadHeader trimmed
            | ReadingSymbols symbols -> 
                ReadSymbols trimmed symbols
            | ReadingDiceName symbolsAndDice -> 
                ReadDiceName trimmed symbolsAndDice profileName
            | ReadingDiceSides symbolsAndDiceName ->
                ReadDice trimmed symbolsAndDiceName
            | Complete _ ->
                Error "Invalid state")
            state

    let LoadProfileFromText profileName (text:string list) =
        let parserStateToProfile state =
            match state with 
            | ParserState.Complete profile ->
                Ok profile
            | _ -> 
                Error "Failed to parse file. Attempt to parse returned invalid state"
        List.fold
            (ParseNextState profileName)
            (Ok ReadingSymbolsHeader)
            text
        |> Result.bind parserStateToProfile





