
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

// helper structure for passing into heuristicAlgo1Interpreter
type HeuristicAlgo1Info = {
  manipulatedHeuristic: Concept;
  repr:string list
}

// /param manipulatedHeuristic is the concept of the manipulated heuristic
// /param repr representation of the heuristic which is interpreted
let heuristicAlgo1Interpreter (info:HeuristicAlgo1Info) =
  // remove something
  let doRemove (repr:string list) =
    // returns the numeric index of the code
    let retIdx (code:string) (numberOfElement:int): int =
      // compute index of removed item
      match code with
      | "any" -> // select any index by random
        rng.Next(numberOfElement)
      | str -> int str

      //IMPL< commented because not necessary and old code >
      //| _ ->
      //  let msg = "[w9] heuristicAlgo1Interpreter().remove(): unknown index name='" + repr.[0] + "'!"
      //  printfn "%s" msg
      //  0
    
    let cmdType = repr.[1]

    match cmdType with
    | "precondition" -> // remove precondition at idx
      let removedIdx = retIdx repr.[0] 0 // TODO< compute number of elements >
      printfn "[d9] remove precondition at idx=%i" removedIdx

      printfn "[d ] not implemented!" // TODO TODO TODO TODO< implement >
    | "action" -> // remove action at idx
      let mutable heuristicActionsArr: Variant[] = conceptRetSlotOrNull info.manipulatedHeuristic [|"heuristicActions"|] |> retVariantArrOrDefault
      
      let numberOfElements = Array.length heuristicActionsArr
      let removedIdx = retIdx repr.[0] numberOfElements

      printfn "[d9] remove action at idx=%i" removedIdx
      
      if removedIdx < Array.length heuristicActionsArr then
        heuristicActionsArr <- removeAt removedIdx heuristicActionsArr
        slotPut info.manipulatedHeuristic.slots [|"heuristicActions"|] "" (makeArr heuristicActionsArr) |> ignore

        printfn "[d9]  removed!"

    | _ ->
      printfn "[w9] heuristicAlgo1Interpreter().remove(): unknown cmd type!"
    

  
  if List.length info.repr > 0 then

    match info.repr.Head with
    | "remove" -> // remove something
      doRemove info.repr.Tail
    | _ ->
      printfn "[w9] heuristicAlgo1Interpreter():  %s is a unknown command" info.repr.Head
  else
    printfn "[w9] heuristicAlgo1Interpreter(): repr is empty!"
