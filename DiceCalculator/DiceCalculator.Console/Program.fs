// Learn more about F# at http://fsharp.org

open DiceCalculator.Domain.Core
open DiceCalculator.Profiles.Profile
open DiceCalculator.Storage.FileIO
open DiceCalculator.Rolls.DiceRolls
open System.Threading
open FSharp.Collections.ParallelSeq

type WinTieLoss = {
    Win: double
    Tie: double
    Loss: double
}

let RunTest test =
    let getPercents successes ties attempts =
        let successes = PositiveInt.Value successes |> double
        let ties = PositiveInt.Value ties |> double
        let attempts = PositiveInt.Value attempts |> double
        match attempts with
        | 0.0 -> Error "cannot divide by 0"
        | a -> 
            {
                Win = successes / a
                Tie = ties / a
                Loss = (a - successes - ties) / a
            }
            |> Ok
    match test with
    | RollTest.DiceVThreshold dvt ->
        let rollOdds = 
            NamedDicePool.ToDicePool dvt.DicePool
            |> Result.map (RollDice >> (OddsOfSymbol dvt.TargetSymbol dvt.Threshold))
        Result.bind 
            (fun odds -> 
                getPercents 
                    odds.Successes 
                    PositiveInt.Zero 
                    odds.Attempts) 
            rollOdds
    | RollTest.DiceVDice dvd ->
        let roll1 =
            NamedDicePool.ToDicePool dvd.DicePool1
            |> Result.map RollDice
        let roll2 =
            NamedDicePool.ToDicePool dvd.DicePool2
            |> Result.map RollDice
        let rollOdds =
            match roll1, roll2 with
            | Ok roll1, Ok roll2->
                OddsAgainstRoll
                    dvd.TargetSymbol1
                    roll1
                    dvd.TargetSymbol2
                    roll2
                |> Ok
            | Error err,_ | _,Error err -> 
                //TODO: aggregate these
                Error err
        Result.bind 
            (fun (odds:OddsAgainstDie) -> 
                getPercents 
                    odds.Wins 
                    odds.Ties 
                    odds.TotalCompares) 
            rollOdds

let DisplayResult (result:WinTieLoss) =
    printfn "Win: %f    Loss %f    Tie: %f" result.Win result.Loss result.Tie

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 1 ->
        printfn "Loading profile..."
        let filePath = FilePath.Create argv.[0]
        let fileName = Result.bind FileName.FromFilePath filePath
        let profileName =
            Result.bind
                (FileName.Value >> NonEmptyString100.Create)
                fileName
        let readLines = 
            ((ReadLinesFromFileAsync CancellationToken.None) 
            >> Async.RunSynchronously)
        let profile =
            match filePath, profileName with
            | Ok filePath, Ok profileName ->
                readLines filePath
                |> Result.map Array.toList
                |> Result.bind (LoadProfileFromText profileName)
            | Error err,_ | _, Error err-> Error err
        printfn "Running tests..."
        let getResults prof =
            prof.Tests
            |> PSeq.map RunTest
            |> PSeq.toList
        let results = 
            Result.map getResults profile
        match results with
        | Ok res ->
            do List.map
                (Result.map DisplayResult)
                res 
            |> ignore
        | Error err -> 
            printfn "Failed to run tests: %s" err
    | x -> printfn "Expected 1 argument, was given %d" x
    0 // return an integer exit code
