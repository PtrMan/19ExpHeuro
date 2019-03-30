
// This file contains datastructures for heuristic representation, interpretation, manipulation 
// of heuristics which are represented as tokens.
//
// (see Docu metaHeuristics.md)

// interpreter for heuristic

// all names have a "1" in it's name because it is the first flexible representation of heuristics

module HInterpreter1

open System
open System.Collections.Generic

open BaseStructures

let removeAt (idx:int) (arr:Variant[]): Variant[] =
  let before = Array.sub arr 0 idx
  let after = Array.sub arr (idx+1) (Array.length arr - idx - 1)
  Array.append before after

// /param manipulatedHeuristic is the concept of the manipulated heuristic
// /param repr representation of the heuristic which is interpreted
let heuristicAlgo1Interpreter (manipulatedHeuristic: Concept) (repr:string list) =
  // remove something
  let doRemove (repr:string list) =
    // compute index of removed item
    let removedIdx = match repr.[0] with
    | "any" -> // select any index by random
      -1 // TODO < pull number from global rng >
    | "0" -> 0
    | "1" -> 1
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | "5" -> 5
    // TODO< generalize by trying to covert it to a integer >

    | _ ->
      let msg = "[w9] heuristicAlgo1Interpreter().remove(): unknown index name='" + repr.[0] + "'!"
      printfn "%s" msg
      0
    
    let cmdType = repr.[1]

    match cmdType with
    | "precondition" -> // remove precondition at idx
      printfn "[d9] remove precondition at idx=%i" removedIdx

      printfn "[d ] not implemented!" // TODO TODO TODO TODO< implement >
    | "action" -> // remove action at idx
      printfn "[d9] remove action at idx=%i" removedIdx


      let mutable heuristicActionsArr: Variant[] = conceptRetSlotOrNull manipulatedHeuristic [|"heuristicActions"|] |> retVariantArrOrDefault
      if removedIdx < Array.length heuristicActionsArr then
        heuristicActionsArr <- removeAt removedIdx heuristicActionsArr
        slotPut manipulatedHeuristic.slots [|"heuristicActions"|] "" (makeArr heuristicActionsArr)

        printfn "[d9]  removed!"

    | _ ->
      printfn "[w9] heuristicAlgo1Interpreter().remove(): unknown cmd type!"
    

  
  if List.length repr > 0 then

    match repr.Head with
    | "remove" -> // remove something
      doRemove repr.Tail
  else
    printfn "[w9] heuristicAlgo1Interpreter(): repr is empty!"
