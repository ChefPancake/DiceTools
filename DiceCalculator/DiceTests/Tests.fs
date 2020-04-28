namespace DiceTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open DiceCalculator.Domain
open DiceCalculator.Rolls

[<TestClass>]
type TestClass () =

    let UnwrapOrFail res =
        match res with 
        | Ok x -> x
        | Error err -> failwith err


    [<TestMethod>]
    member this.SingleDieRolled () =
        
        let symString = NonEmptyString100.Create "Pip" |> UnwrapOrFail
        let sym = Symbol symString
        let sides = 
            [ { DieSide.Symbols = None }; { DieSide.Symbols = NonEmptyList.singleton sym |> Some } ] 
            |> NonEmptyList.Create
            |> UnwrapOrFail
        let die = { Die.Sides = sides }
        let dicePool = { DicePool.Dice = NonEmptyList.singleton die}

        let results = GenerateRollResults dicePool

        Assert.AreEqual(
            NonEmptyList.length sides,
            NonEmptyList.length results.Rolls);

    [<TestMethod>]
    member this.TwoDiceTwoSides () =
        
        let symString = NonEmptyString100.Create "Pip" |> UnwrapOrFail
        let sym = Symbol symString
        let sides = 
            [ { DieSide.Symbols = None }; { DieSide.Symbols = NonEmptyList.singleton sym |> Some } ] 
            |> NonEmptyList.Create
            |> UnwrapOrFail
        let die1 = { Die.Sides = sides }

        let dicePool = 
            [{ Die.Sides = sides}; { Die.Sides = sides}]
            |> NonEmptyList.Create
            |> UnwrapOrFail
            |> (fun x -> {DicePool.Dice = x })

        let results = GenerateRollResults dicePool

        Assert.AreEqual(
            (NonEmptyList.length sides) * (NonEmptyList.length sides),
            NonEmptyList.length results.Rolls);
