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
        //assemble
        let symString = NonEmptyString100.Create "Pip" |> UnwrapOrFail
        let sym = Symbol symString
        let sides = 
            [ { DieSide.Symbols = None }; { DieSide.Symbols = NonEmptyList.singleton sym |> Some } ] 
            |> NonEmptyList.Create
            |> UnwrapOrFail
        let die = { Die.Sides = sides }
        let dicePool = { DicePool.Dice = NonEmptyList.singleton die}

        //action
        let results = RollDice dicePool

        //assert
        Assert.AreEqual(
            NonEmptyList.length sides,
            NonEmptyList.length results.Rolls);

    [<TestMethod>]
    member this.TwoDiceTwoSidesRolled () =
        //assemble
        let symString = NonEmptyString100.Create "Pip" |> UnwrapOrFail
        let sym = Symbol symString
        let sides = 
            [ { DieSide.Symbols = None }; { DieSide.Symbols = NonEmptyList.singleton sym |> Some } ] 
            |> NonEmptyList.Create
            |> UnwrapOrFail
        let dicePool = 
            [ { Die.Sides = sides}; { Die.Sides = sides} ]
            |> NonEmptyList.Create
            |> UnwrapOrFail
            |> (fun x -> {DicePool.Dice = x })

        //action
        let results = RollDice dicePool

        //assert
        Assert.AreEqual(
            (NonEmptyList.length sides) * (NonEmptyList.length sides),
            NonEmptyList.length results.Rolls);

    [<TestMethod>]
    member this.TwoDiceRolledExactlyOneHit () =
        //assemble
        let symString = NonEmptyString100.Create "Pip" |> UnwrapOrFail
        let sym = Symbol symString
        let sides = 
            [ { DieSide.Symbols = None }; { DieSide.Symbols = NonEmptyList.singleton sym |> Some } ] 
            |> NonEmptyList.Create
            |> UnwrapOrFail
        let dicePool = 
            [ { Die.Sides = sides}; { Die.Sides = sides} ]
            |> NonEmptyList.Create
            |> UnwrapOrFail
            |> (fun x -> {DicePool.Dice = x })
        let results = RollDice dicePool
        let threshold = 
            PositiveInt.Create 1 
            |> UnwrapOrFail
            |> HitThreshold.Exactly 

        //action
        let odds = OddsOfSymbol sym threshold results

        //assert
        Assert.AreEqual(4, PositiveInt.Value odds.Attempts)
        Assert.AreEqual(2, PositiveInt.Value odds.Successes)

    [<TestMethod>]
    member this.TwoDiceRolledOneOrMoreHit () =
        //assemble
        let symString = NonEmptyString100.Create "Pip" |> UnwrapOrFail
        let sym = Symbol symString
        let sides = 
            [ { DieSide.Symbols = None }; { DieSide.Symbols = NonEmptyList.singleton sym |> Some } ] 
            |> NonEmptyList.Create
            |> UnwrapOrFail
        let dicePool = 
            [ { Die.Sides = sides}; { Die.Sides = sides} ]
            |> NonEmptyList.Create
            |> UnwrapOrFail
            |> (fun x -> {DicePool.Dice = x })
        let results = RollDice dicePool
        let threshold = 
            PositiveInt.Create 1 
            |> UnwrapOrFail
            |> HitThreshold.AtLeast 

        //action
        let odds = OddsOfSymbol sym threshold results

        //assert
        Assert.AreEqual(4, PositiveInt.Value odds.Attempts)
        Assert.AreEqual(3, PositiveInt.Value odds.Successes)

