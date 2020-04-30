namespace DiceCalculator.Storage
open DiceCalculator.Domain.Core
open System
open System.IO

module Profile =
    type FilePath = private FilePath of string
    module FilePath =
        let Create pathStr =
            try
                Path.GetFullPath pathStr
                |> FilePath
                |> Ok
            with
            | _ -> Error ("Unable to parse \"" + pathStr + "\" as filePath")
        let Value (FilePath filePath) = filePath

    type NamedDie = {
        Name:NonEmptyString100
        Die: Die
    }

    type DiceProfile = {
        Name: NonEmptyString100
        Symbols: Symbol list
        Dice: NamedDie list
    }
        
    let SaveProfileToDisk filepath diceProfile =
        0

    type ParsedSymbols = { 
        ParsedSymbols: Symbol list
    }
    type ParsedSymbolsAndDice = {
        ParsedSymbols: Symbol list
        ParsedDice: NamedDie list
    }
    type ParsedSymbolsAndDiceName = {
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

    let rec private ParseProfileFileAsync profileName (file:StreamReader) (state: ParserState): Async<Result<ParserState, string>> =
        async {
            try 
                let! line = 
                    file.ReadLineAsync() 
                    |> Async.AwaitTask
                let trimmed = line.Trim()
                let nextState =
                    match state with
                    | ReadingSymbolsHeader -> 
                        match trimmed.Equals "Symbols:" with
                        | true -> 
                            { ParsedSymbols.ParsedSymbols = [] } 
                            |> ReadingSymbols
                            |> Ok
                        | false ->
                            Error "Failed to parse symbols"
                    | ReadingSymbols symbols ->
                        match trimmed with
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
                    | ReadingDiceName symbolsAndDice ->
                        match trimmed with
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
                    | ReadingDiceSides symbolsAndDiceName ->
                        match trimmed with
                        | x when x.StartsWith '[' && x.EndsWith ']' ->
                            let sides = trimmed.TrimStart('[').TrimEnd(']').Split(";")
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
                    | Complete _ ->
                        Error "Invalid state"
                return!
                    match nextState with
                    | Error err -> async { return Error err }
                    | Ok next -> 
                        match next with 
                        | ParserState.Complete _ -> 
                            async { return Ok next }
                        | _ -> ParseProfileFileAsync profileName file next
            with 
            | ex -> 
                return Error ex.Message
        }

    let LoadProfileFromDiskAsync filePath =
        let rawPath = FilePath.Value filePath
        match File.Exists rawPath with
        | true ->
            async {
                let name = 
                    Path.GetFileNameWithoutExtension rawPath 
                    |> NonEmptyString100.Create

                let parserStateToProfile state =
                    match state with 
                    | ParserState.Complete profile ->
                        Ok profile
                    | _ -> 
                        Error "Failed to parse file. Attempt to parse returned invalid state"

                match name with
                | Ok name ->
                    use file = File.OpenText rawPath
                    let! parseResult = ParseProfileFileAsync name file ReadingSymbolsHeader
                    return Result.bind parserStateToProfile parseResult                    
                | Error err -> return Error err                 
            }
        | false -> 
            async { return Error "File does not exist" }






