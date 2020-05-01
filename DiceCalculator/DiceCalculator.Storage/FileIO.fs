namespace DiceCalculator.Storage
open DiceCalculator.Domain.Core
open System
open System.IO

module FileIO =
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

    type FileName = private FileName of string
    module FileName = 
        let Create nameStr =
            match String.IsNullOrWhiteSpace(nameStr) with
            | true -> Error "FileName cannot be empty"
            | false -> FileName nameStr |> Ok
        let FromFilePath (FilePath filePath) =
            try 
                Path.GetFileNameWithoutExtension filePath
                |> FileName
                |> Ok
            with
            | ex -> 
                Error ("Failed to parse filename: " + ex.Message)
        let Value (FileName fileName) = fileName

    let ReadLinesFromFileAsync filePath cancel =
        async {
            let rawPath = FilePath.Value filePath
            match File.Exists rawPath with
            | true ->
                let! lines =
                    File.ReadAllLinesAsync(rawPath, cancel)
                    |> Async.AwaitTask 
                return Ok lines
            | false -> 
                return Error "File does not exist" 
        }
