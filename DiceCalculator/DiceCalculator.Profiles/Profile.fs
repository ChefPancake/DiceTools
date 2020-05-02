namespace DiceCalculator.Profiles

open DiceCalculator.Domain.Core
open System
open System.IO

module Profile =
    type NamedDie = {
        Name:NonEmptyString100
        Die: Die
    }

    type NamedDicePool = NamedDicePool of NamedDie list

    //TODO: add target symbols
    type DiceVDice = {
        DicePool1: NamedDicePool
        //TargetSymbol1: Symbol
        DicePool2: NamedDicePool
        //TargetSymbol2: Symbol
    }

    type DiceVThreshold = {
        DicePool: NamedDicePool
        //TargetSymbol: Symbol
        Threshold: HitThreshold
    }

    type RollTest =
    | DiceVDice of DiceVDice
    | DiceVThreshold of DiceVThreshold

    type DiceProfile = {
        Name: NonEmptyString100
        Symbols: Symbol list
        Dice: NamedDie list
        Tests: RollTest list
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
    type private ParsedSymbolsDiceAndTests = {
        ParsedSymbols: Symbol list
        ParsedDice: NamedDie list
        ParsedTests: RollTest list
    }

    type private ParserState = 
    | ReadingSymbolsHeader
    | ReadingSymbols of ParsedSymbols
    | ReadingDiceName of ParsedSymbolsAndDice
    | ReadingDiceSides of ParsedSymbolsAndDiceName
    | ReadingTests of ParsedSymbolsDiceAndTests
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

    let private AllOrNoneResults (items:Result<'a,string> list): Result<'a list, string> =
        let filtered =
            List.choose
                (fun (x:Result<'a,string>) -> 
                    match x with
                    | Ok y -> Some y
                    | Error _ -> None)
                items
        match List.length filtered = List.length items with
        | true -> Ok filtered
        | false -> Error ""

    let private TryParseAsInt (intString:string) =
        let mutable parsed = -1
        match Int32.TryParse (intString, &parsed) with
        | true -> Some parsed
        | false -> None

    let private ReadTest (line:string) (symbolsDiceAndTests:ParsedSymbolsDiceAndTests) =
        match line with 
        | x when x.StartsWith '[' && x.EndsWith ']' ->
            let split = x.Replace(" ", String.Empty).TrimStart('[').TrimEnd(']').Split("][")
            match Array.length split = 2 with
            | false -> Error "Unable to parse dice test"
            | true -> 
                //the left must always have two elements
                let firstSplit = split.[0].Split(',')
                
                let selectDie (item:NonEmptyString100) =
                    let matches = 
                        List.filter
                            (fun (x:NamedDie) -> x.Name = item)
                            symbolsDiceAndTests.ParsedDice
                    match List.length matches with  
                    | 1 -> List.head matches |> Ok
                    | 0 -> Error ("Die named " + (NonEmptyString100.Value item) + " does not match existing definitions")
                    | _ -> Error ("Die list contains more than one entry with name " + (NonEmptyString100.Value item))
                let checkAndSelectDie nameText =
                    NonEmptyString100.Create nameText
                    |> Result.bind selectDie
                let firstSide =
                    match firstSplit with
                    | [||] -> Error "Left side of test must contain at least one dice pool"
                    | _ -> 
                        Array.map
                            checkAndSelectDie
                            firstSplit
                        |> Array.toList
                        |> AllOrNoneResults
                        |> Result.map NamedDicePool                        
                        
                let secondSplit = split.[0].Split(',')
                match Array.length secondSplit with
                | 0 -> Error "Right side must have at least one item"
                | 1 -> 
                    let firstItem = secondSplit.[0]
                    let chars = firstItem.ToCharArray()
                    //attempt to parse as a threshold
                    let threshold =
                        match Array.length chars with
                        | x when x < 2 -> None
                        | _ ->
                            match (chars.[0], chars.[1]) with
                            | ('=',_) ->
                                let intParse = chars.[1..].ToString()
                                TryParseAsInt intParse
                                |> Option.bind (fun int -> 
                                    match PositiveInt.Create int with
                                    | Ok pi -> Some pi
                                    | Error _ -> None)
                                |> Option.map HitThreshold.Exactly
                            | ('<','=') ->
                                let intParse = chars.[2..].ToString()
                                TryParseAsInt intParse
                                |> Option.bind (fun int ->
                                    match PositiveInt.Create int with
                                    | Ok pi -> Some pi
                                    | Error _ -> None)
                                |> Option.map HitThreshold.AtMost
                            | ('>','=') ->
                                let intParse = chars.[2..].ToString()
                                TryParseAsInt intParse
                                |> Option.bind (fun int ->
                                    match PositiveInt.Create int with
                                    | Ok pi -> Some pi
                                    | Error _ -> None)
                                |> Option.map HitThreshold.AtLeast
                            | (_,_) -> None
                    match threshold with 
                    | Some thresh -> 
                        let toAdd =
                            Result.map (fun dice ->
                                {
                                    DiceVThreshold.DicePool = dice
                                    Threshold = thresh
                                }
                                |> RollTest.DiceVThreshold
                                |> List.singleton) 
                                firstSide
                        Result.map
                            (fun threshAsList ->
                                {
                                    ParsedSymbolsDiceAndTests.ParsedDice = symbolsDiceAndTests.ParsedDice
                                    ParsedSymbolsDiceAndTests.ParsedSymbols = symbolsDiceAndTests.ParsedSymbols
                                    ParsedSymbolsDiceAndTests.ParsedTests = List.append symbolsDiceAndTests.ParsedTests threshAsList
                                }
                            )
                            toAdd
                    | None -> 
                        checkAndSelectDie firstItem
                        |> Result.bind (fun die -> 
                            let secondPool = List.singleton die |> NamedDicePool
                            let toAdd = 
                                Result.map 
                                    (fun dice ->
                                        {
                                            DiceVDice.DicePool1 = dice
                                            DiceVDice.DicePool2 = secondPool
                                        }
                                        |> RollTest.DiceVDice)
                                    firstSide
                            Result.map
                                (fun firstPool -> 
                                {
                                    ParsedSymbolsDiceAndTests.ParsedDice = symbolsDiceAndTests.ParsedDice
                                    ParsedSymbolsDiceAndTests.ParsedSymbols = symbolsDiceAndTests.ParsedSymbols
                                    ParsedSymbolsDiceAndTests.ParsedTests = List.append symbolsDiceAndTests.ParsedTests (List.singleton firstPool)
                                })
                                toAdd)
                | _ -> Error ""
                |> Result.map ParserState.ReadingTests
                //the right may have one or two elements
                    //if 1, it must be numeric
                //for all sides with two elements, first must be a positive int, second must be a die name that matches the list above
        | _ -> ParserState.Complete

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
            | ReadingTests symbolsDiceAndTests ->
                ReadTest trimmed symbolsDiceAndTests
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





