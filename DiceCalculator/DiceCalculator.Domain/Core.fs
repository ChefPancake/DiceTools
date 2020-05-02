namespace DiceCalculator.Domain

open System.IO

module Core =
    type PositiveInt = private PositiveInt of int
    module PositiveInt =
        let Create value =
            match value with
            | x when x < 0 -> Error "Value cannot be negative"
            | x -> PositiveInt x |> Ok
        let Value (PositiveInt value) = value
        let Zero = PositiveInt 0

    type NonEmptyList<'a> = private NonEmptyList of 'a list
    module NonEmptyList =
        let Create items =
            match items with
            | [] -> Error "Cannot create NonEmptyList with 0 items"
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
        let lengthPositive (NonEmptyList items) = 
            List.length items |> PositiveInt
        let choose chooser (NonEmptyList items) =
            match List.choose chooser items with
            | [] -> None
            | chosen -> NonEmptyList chosen |> Some
        let filter chooser (NonEmptyList items) = 
            match List.filter chooser items with
            | [] -> None
            | chosen -> NonEmptyList chosen |> Some

    type NonEmptyString100 = private NonEmptyString100 of string
    module NonEmptyString100 = 
        let Create str =
            match String.length str with 
            | 0 -> Error "String cannot be empty"
            | x when x > 100 -> Error "String cannot exceed 100 characters"
            | _ -> NonEmptyString100 str |> Ok
        let Value (NonEmptyString100 value) = value

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

    type HitThreshold =
        | Exactly of PositiveInt
        | AtLeast of PositiveInt
        | AtMost of PositiveInt

    
