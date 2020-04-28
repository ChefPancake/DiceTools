namespace DiceCalculator

module AssemblyInfo =
    open System.Runtime.CompilerServices
    [<assembly: InternalsVisibleTo("DiceTests")>]
    do()

module Say =
    let hello name =
        printfn "Hello %s" name
