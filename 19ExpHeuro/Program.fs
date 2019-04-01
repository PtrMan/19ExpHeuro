// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open BaseStructures
open Dev

[<EntryPoint>]
let main argv = 
  devTestInterpretedHeuristic ()

  printfn "%A" argv
  0 // return an integer exit code
