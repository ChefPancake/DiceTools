namespace DiceTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open DiceCalculator.Domain.Core
open DiceCalculator.Rolls.DiceRolls

[<TestClass>]
type RollTests () =

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

    [<TestMethod>]
    member this.CompareDiceTwoSides () =
        //assemble
        let symString = NonEmptyString100.Create "Pip" |> UnwrapOrFail
        let sym = Symbol symString
        let sides = 
            [ { DieSide.Symbols = None }; { DieSide.Symbols = NonEmptyList.singleton sym |> Some } ] 
            |> NonEmptyList.Create
            |> UnwrapOrFail
        let dicePool1 = 
            [ { Die.Sides = sides}; { Die.Sides = sides} ]
            |> NonEmptyList.Create
            |> UnwrapOrFail
            |> (fun x -> {DicePool.Dice = x })
        let dicePool2 = 
            [ { Die.Sides = sides}; { Die.Sides = sides} ]
            |> NonEmptyList.Create
            |> UnwrapOrFail
            |> (fun x -> {DicePool.Dice = x })
        let roll1 = RollDice dicePool1
        let roll2 = RollDice dicePool2

        //action
        let compareOdds = OddsAgainstRoll sym roll1 sym roll2

        //assert
        Assert.AreEqual(16, PositiveInt.Value compareOdds.TotalCompares)
        Assert.AreEqual(6, PositiveInt.Value compareOdds.Ties)
        Assert.AreEqual(5, PositiveInt.Value compareOdds.Wins)
        Assert.AreEqual(5, PositiveInt.Value compareOdds.Losses)
        

