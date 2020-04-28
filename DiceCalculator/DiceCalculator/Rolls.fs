namespace DiceCalculator
open Domain

module internal Rolls =
    let Cartesian xs ys = 
        xs 
        |> NonEmptyList.collect (fun x -> 
            ys 
            |> NonEmptyList.map (fun y ->
                NonEmptyList.singleton y
                |> NonEmptyList.append x))

    let GenerateRollResults (dicePool: DicePool): RollResults =
        let start = 
            NonEmptyList.map 
                NonEmptyList.singleton 
                (NonEmptyList.head dicePool.Dice
                |> (fun x -> x.Sides))

        let toRoll x = {Roll.Sides = x}
        let toRollResults x = {RollResults.Rolls = x}

        match NonEmptyList.tail dicePool.Dice with
        | None -> 
            start
            |> NonEmptyList.map toRoll
            |> toRollResults
        | Some dice ->
            let dieSides = NonEmptyList.map (fun (x:Die) -> x.Sides) dice
            NonEmptyList.fold
                Cartesian
                start
                dieSides
            |> NonEmptyList.map toRoll
            |> toRollResults

