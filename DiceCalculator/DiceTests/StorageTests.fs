﻿namespace DiceTests

open System
open System.IO
open System.Text
open Microsoft.VisualStudio.TestTools.UnitTesting
open DiceCalculator.Domain.Core
open DiceCalculator.Storage.Profile

[<TestClass>]
type StorageTests () =

    let UnwrapOrFail res =
        match res with 
        | Ok x -> x
        | Error err -> failwith err

    [<TestMethod>]
    member this.LoadProfile () =
        //assemble
        let mutable stringBuilder=
            new StringBuilder ()
        stringBuilder <- stringBuilder.AppendLine "Symbols:"
        stringBuilder <- stringBuilder.AppendLine " Pip1"
        stringBuilder <- stringBuilder.AppendLine " Pip2"
        stringBuilder <- stringBuilder.AppendLine ""
        stringBuilder <- stringBuilder.AppendLine "Dice:"
        stringBuilder <- stringBuilder.AppendLine "Die1"
        stringBuilder <- stringBuilder.AppendLine "[[];[];[Pip1,Pip1];[Pip2]]"
        stringBuilder <- stringBuilder.AppendLine ""

        let dirPath = "C://Tests//"
        let filePath = Path.Combine (dirPath, "TestProfile.dcp")
        Directory.CreateDirectory dirPath |> ignore
        File.WriteAllText (filePath, stringBuilder.ToString())

        //action
        let filePath = FilePath.Create filePath |> UnwrapOrFail
        let profile = LoadProfileFromDiskAsync filePath |> Async.RunSynchronously |> UnwrapOrFail

        //assert
        let pip1 = 
            List.head profile.Symbols 
            |> Symbol.Value 
            |> NonEmptyString100.Value
        let pip2 = 
            List.tail profile.Symbols 
            |> List.head 
            |> Symbol.Value 
            |> NonEmptyString100.Value
        let die =
            List.head profile.Dice
        let dieName = 
            die.Name |> NonEmptyString100.Value
        let sides = die.Die.Sides
        let side1 = 
            NonEmptyList.head sides
        let side2 = 
            NonEmptyList.tail sides 
            |> Option.map NonEmptyList.head
        let side3 = 
            NonEmptyList.tail sides 
            |> Option.bind NonEmptyList.tail 
            |> Option.map NonEmptyList.head
        let side4 = 
            NonEmptyList.tail sides 
            |> Option.bind NonEmptyList.tail 
            |> Option.bind NonEmptyList.tail 
            |> Option.map NonEmptyList.head
        let selectSymbols (sideOption:DieSide option) =
            match sideOption with
            | Some side -> side.Symbols
            | None -> None

        Assert.AreEqual ("Pip1", pip1)
        Assert.AreEqual ("Pip2", pip2)
        Assert.AreEqual ("Die1", dieName)
        Assert.AreEqual (4, NonEmptyList.length sides )
        Assert.AreEqual (None, side1.Symbols)
        Assert.AreEqual (None, selectSymbols side2)
        Assert.AreEqual (Some 2, selectSymbols side3 |> Option.map NonEmptyList.length)
        Assert.AreEqual (Some "Pip1", selectSymbols side3 |> Option.map (NonEmptyList.head >> Symbol.Value >> NonEmptyString100.Value))
        Assert.AreEqual (Some 1, selectSymbols side4 |> Option.map NonEmptyList.length)
        Assert.AreEqual (Some "Pip2", selectSymbols side4 |> Option.map (NonEmptyList.head >> Symbol.Value >> NonEmptyString100.Value))



        