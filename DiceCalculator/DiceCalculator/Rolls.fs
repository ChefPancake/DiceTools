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
            NonEmptyList.head dicePool.Dice
            |> (fun x -> x.Sides)
            |> NonEmptyList.map NonEmptyList.singleton

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

    let private SymbolTotals symbol rollResults =
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

        rollResults.Rolls
        |> NonEmptyList.map countRoll


    let OddsOfSymbol (symbol: Symbol) (threshold: HitThreshold) (rollResults: RollResults) =
        let attempts = NonEmptyList.lengthPositive rollResults.Rolls
        let totals =
            SymbolTotals
                symbol
                rollResults
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

    type private WinsLosses =
    | Win
    | Tie
    | Loss

    let OddsAgainstRoll symbol roll1 roll2 =
        let roll1Totals =
            SymbolTotals
                symbol
                roll1
        let roll2Totals =
            SymbolTotals
                symbol
                roll2
        let overall =
            roll1Totals
            |> NonEmptyList.collect(fun x -> 
                roll2Totals
                |> NonEmptyList.map(fun y -> 
                    match x with
                    | z when z > y -> Win
                    | z when z < y -> Loss
                    | _ -> Tie))
        let attempts = 
            NonEmptyList.lengthPositive overall
        let wins = 
            match NonEmptyList.filter (fun x -> x = Win) overall with
            | Some w -> NonEmptyList.lengthPositive w
            | None -> PositiveInt.Zero
        let losses = 
            match NonEmptyList.filter (fun x -> x = Loss) overall with
            | Some w -> NonEmptyList.lengthPositive w
            | None -> PositiveInt.Zero
        let ties = 
            match NonEmptyList.filter (fun x -> x = Tie) overall with
            | Some w -> NonEmptyList.lengthPositive w
            | None -> PositiveInt.Zero
        {
            Wins = wins
            Losses = losses
            Ties = ties
            TotalCompares = attempts
        }

        

