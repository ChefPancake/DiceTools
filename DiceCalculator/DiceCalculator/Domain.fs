namespace DiceCalculator

module internal Domain =
    type NonEmptyList<'a> = private NonEmptyList of 'a list

    module NonEmptyList =
        let Create items =
            match List.length items with
            | 0 -> Error "Cannot create NonEmptyList with 0 items"
            | _ -> NonEmptyList items |> Ok
        let Value (NonEmptyList items) = items
        let map f (NonEmptyList items) =
            List.map f items
            |> NonEmptyList
        let head (NonEmptyList items) =
            List.head items
        let fold f start (NonEmptyList items) = 
            List.fold f start items
        let singleton item =
            List.singleton item
            |> NonEmptyList
        let collect mapping (NonEmptyList items) =
            let f = mapping >> Value
            List.collect f items
            |> NonEmptyList
        let append (NonEmptyList items) (NonEmptyList item) =
            List.append items item
            |> NonEmptyList
        let tail (NonEmptyList items) = 
            match List.tail items with 
            | [] -> None
            | x -> NonEmptyList x |> Some
        let length (NonEmptyList items) =
            List.length items

    type NonEmptyString100 = private NonEmptyString100 of string
    module NonEmptyString100 = 
        let Create str =
            match String.length str with 
            | 0 -> Error "String cannot be empty"
            | x when x > 100 -> Error "String cannot exceed 100 characters"
            | _ -> NonEmptyString100 str |> Ok

    type Symbol = Symbol of NonEmptyString100
    module Symbol =
        let Value (Symbol name) = name

    type DieSide = {
        Symbols: NonEmptyList<Symbol> option
    }

    type Die = {
        Sides: NonEmptyList<DieSide>
    }

    type DicePool = {
        Dice: NonEmptyList<Die>
    }
    
    type Roll = {
        Sides: NonEmptyList<DieSide>
    }

    type RollResults = {
        Rolls: NonEmptyList<Roll>  
    }
