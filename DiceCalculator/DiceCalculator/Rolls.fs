namespace DiceCalculator
open Domain

module internal Rolls =
    let private Cartesian xs ys = 
        xs 
        |> NonEmptyList.collect (fun x -> 
            ys 
            |> NonEmptyList.map (fun y ->
                NonEmptyList.singleton y
                |> NonEmptyList.append x))

    let RollDice (dicePool: DicePool): RollResults =
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

    let OddsOfSymbol (symbol: Symbol) (threshold: HitThreshold) (rollResults: RollResults) =
        let attempts = NonEmptyList.lengthPositive rollResults.Rolls
        let countSymbols (sym:Symbol) (side:DieSide) =
            match side.Symbols with
            | None -> 0
            | Some symbols -> 
                match NonEmptyList.filter 
                    (fun (x: Symbol) -> x.GetHashCode() = sym.GetHashCode()) 
                    symbols with
                | Some s -> NonEmptyList.length s
                | None -> 0
        let countRoll (roll:Roll):int =
            NonEmptyList.map
                (fun (x:DieSide) -> countSymbols symbol x )
                roll.Sides
            |> NonEmptyList.fold (+) 0
        let totals =
            rollResults.Rolls
            |> NonEmptyList.map countRoll
        let selector = 
            match threshold with
            | HitThreshold.Exactly ex -> 
                fun x -> x = PositiveInt.Value ex
            | HitThreshold.AtLeast al -> 
                fun x -> x >= PositiveInt.Value al
            | HitThreshold.AtMost am -> 
                fun x -> x <= PositiveInt.Value am
        let successes = 
            match NonEmptyList.filter selector totals with
            | Some items -> 
                NonEmptyList.lengthPositive items
            | None -> PositiveInt.Zero
        { 
            Attempts = attempts
            Successes = successes
        }