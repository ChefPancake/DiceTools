namespace DiceCalculator

module AssemblyInfo =
    open System.Runtime.CompilerServices
    [<assembly: InternalsVisibleTo("DiceTests")>]
    do()
