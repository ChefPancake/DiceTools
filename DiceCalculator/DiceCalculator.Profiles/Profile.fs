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

    type DiceVDice = {
        DicePool1: NamedDicePool
        TargetSymbol1: Symbol
        DicePool2: NamedDicePool
        TargetSymbol2: Symbol
    }

    type DiceVThreshold = {
        DicePool: NamedDicePool
        TargetSymbol: Symbol
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

    let private ReadDiceName (line:string) (symbolsAndDice: ParsedSymbolsAndDice) =
        match line with
        | x when x.StartsWith '[' ->
            Error "Missing dice name while parsing"
        | x when String.IsNullOrWhiteSpace x ->
            ParserState.ReadingDiceName symbolsAndDice |> Ok
        | x when x = "Tests:" ->
            {
                ParsedSymbolsDiceAndTests.ParsedDice = symbolsAndDice.ParsedDice
                ParsedSymbolsDiceAndTests.ParsedSymbols = symbolsAndDice.ParsedSymbols
                ParsedSymbolsDiceAndTests.ParsedTests = []
            } 
            |> ParserState.ReadingTests
            |> Ok
        | x ->
            NonEmptyString100.Create x
            |> Result.map (fun dieName -> 
            {
                ParsedSymbolsAndDiceName.ParsedDieName = dieName
                ParsedSymbols = symbolsAndDice.ParsedSymbols
                ParsedDice = symbolsAndDice.ParsedDice
            }
            |> ParserState.ReadingDiceSides)

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
        | true -> Ok parsed
        | false -> Error ("Unable to parse " + intString + " as Int32" )

    let private CheckAndSelectDie diceList nameText =
        let selectDie (item:NonEmptyString100) =
            let matches = 
                List.filter
                    (fun (x:NamedDie) -> x.Name = item)
                    diceList
            match List.length matches with  
            | 1 -> List.head matches |> Ok
            | 0 -> Error ("Die named " + (NonEmptyString100.Value item) + " does not match existing definitions")
            | _ -> Error ("Die list contains more than one entry with name " + (NonEmptyString100.Value item))
        NonEmptyString100.Create nameText
        |> Result.bind selectDie

    let private TryParseThreshold (chars: char []) =
        let stringToThreshold (chars: char []) thresholdVariant =
            let intParse = chars |> System.String
            TryParseAsInt intParse
            |> Result.bind PositiveInt.Create
            |> Result.map thresholdVariant
        match (chars.[0], chars.[1]) with
        | ('=',_) ->
            stringToThreshold chars.[1..] HitThreshold.Exactly
        | ('<','=') ->
            stringToThreshold chars.[2..] HitThreshold.AtMost
        | ('>','=') ->
            stringToThreshold chars.[2..] HitThreshold.AtLeast
        | (_,_) -> Error "Invalid threshold value"

    let private ParseDicePoolString (dice:NamedDie list) (str:string):Result<NamedDicePool, string> = 
        let diceNames = str.Split(',')
        match diceNames with
        | [||] -> Error "Left side of test must contain at least one die in pool"
        | _ -> 
            Array.map
                (CheckAndSelectDie dice)
                diceNames
            |> Array.toList
            |> AllOrNoneResults
            |> Result.map NamedDicePool       
        
    let private ParseSymbolString (symbols:Symbol list) (str:string):Result<Symbol, string> =
        let symbolStrings = List.map Symbol.Value symbols
        NonEmptyString100.Create str
        |> Result.bind (fun x -> 
            match List.contains x symbolStrings with 
            | true -> Symbol x |> Ok 
            | false -> Error "Unable to parse as targetSymbol")

    let private ReadTest (line:string) (symbolsDiceAndTests:ParsedSymbolsDiceAndTests) profileName =
        match line with 
        | x when x.StartsWith '[' && x.EndsWith ']' ->
            let split = x.Replace(" ", String.Empty).TrimStart('[').TrimEnd(']').Split("]v[")
            match Array.length split = 2 with
            | false -> Error "Unable to parse dice test"
            | true -> 
                let firstSplit = split.[0].Split(';')
                let newTest = 
                    match Array.length firstSplit with
                    | 2 ->
                        let trimmedFirstDice = (firstSplit.[0].TrimStart('[').TrimEnd(']'))
                        let firstDice =
                            ParseDicePoolString 
                                symbolsDiceAndTests.ParsedDice 
                                trimmedFirstDice
                        let firstSymbol =   
                            ParseSymbolString 
                                symbolsDiceAndTests.ParsedSymbols 
                                firstSplit.[1]
                        match split.[1].Contains(';') with
                        | false ->
                            let threshold = 
                                let chars = split.[1].ToCharArray()
                                match Array.length chars with
                                | _ -> TryParseThreshold chars
                                | x when x < 2 -> Error "Invalid threshold value"

                            match firstDice, firstSymbol, threshold with
                            | Ok d1, Ok s1, Ok t ->
                                {
                                    DiceVThreshold.DicePool = d1
                                    DiceVThreshold.TargetSymbol = s1
                                    DiceVThreshold.Threshold = t
                                }
                                |> RollTest.DiceVThreshold
                                |> Ok
                            | _,_,_ -> Error "Unable to parse test"
                        | true ->
                            let secondSplit = split.[1].Split(';')
                            let trimmedSecondDice = (secondSplit.[0].TrimStart('[').TrimEnd(']'))
                            let secondDice =
                                ParseDicePoolString
                                    symbolsDiceAndTests.ParsedDice
                                    trimmedSecondDice
                            let secondSymbol =
                                ParseSymbolString
                                    symbolsDiceAndTests.ParsedSymbols
                                    secondSplit.[1]

                            match firstDice, secondDice, firstSymbol, secondSymbol with
                            | Ok d1, Ok d2, Ok s1, Ok s2 ->
                                {
                                    DicePool1 = d1
                                    DicePool2 = d2
                                    TargetSymbol1 = s1
                                    TargetSymbol2 = s2
                                }
                                |> RollTest.DiceVDice
                                |> Ok
                            | _,_,_,_ ->
                                Error "Unable to parse test"
                    | _ -> Error "Left side of test must be a targeted dice pool"
                let addToTestList test =    
                    {
                        ParsedSymbolsDiceAndTests.ParsedSymbols = symbolsDiceAndTests.ParsedSymbols
                        ParsedSymbolsDiceAndTests.ParsedDice = symbolsDiceAndTests.ParsedDice
                        ParsedSymbolsDiceAndTests.ParsedTests = 
                            List.append symbolsDiceAndTests.ParsedTests (List.singleton test)
                    }
                    |> ParserState.ReadingTests
                Result.map addToTestList newTest
        | _ -> 
            {
                DiceProfile.Name = profileName
                DiceProfile.Dice = symbolsDiceAndTests.ParsedDice
                DiceProfile.Symbols = symbolsDiceAndTests.ParsedSymbols
                DiceProfile.Tests = symbolsDiceAndTests.ParsedTests
            } 
            |> ParserState.Complete
            |> Ok

    let private ParseNextState profileName state (line:string) =
        let trimmed = line.Trim()
        Result.bind (fun state -> 
            match state with
            | ReadingSymbolsHeader -> 
                ReadHeader trimmed
            | ReadingSymbols symbols -> 
                ReadSymbols trimmed symbols
            | ReadingDiceName symbolsAndDice -> 
                ReadDiceName trimmed symbolsAndDice
            | ReadingDiceSides symbolsAndDiceName ->
                ReadDice trimmed symbolsAndDiceName
            | ReadingTests symbolsDiceAndTests ->
                ReadTest trimmed symbolsDiceAndTests profileName
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





