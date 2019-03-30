module BaseStructures

open System
open System.Collections.Generic

// clamp value to range
let clamp min_ max_ value = max min_ value |> min max_

// helper to return the path as a dot
let convPath (path:string[]) =
  String.concat "." path


let dist (arr:float[]) = Array.map2 (*) arr arr |> Array.sum |> sqrt

// computes the usefulness "Worth" of a overall task
// [Lenat phd dissertation page pdf 47]
// TODO< use for usefulness / "Worth" computation of task >
let calcTaskUsefulness (reasonRating:float[]) (act:float) (facet:float) (concept:float) =
  (dist reasonRating) * (0.2*act + 0.3*facet + 0.5*concept)


// Symbolic experession for symbolic descriptions and later Predicate Calculus etc
type Symbl =
| SymblName of string // name or something
| SymblFn of string * Symbl list // function like "a(b, c)"
| SymblBinary of Symbl * string * Symbl // binary relation like "a = b"
| SymblProd of Symbl list // a product / array

// convert symbol to string
let rec convSymblToStrRec (s:Symbl): string =
  match s with
  | SymblName name -> name
  | SymblFn (name, args) -> // function like "a(b, c)"
    let stringOfArgs = List.map convSymblToStrRec args |> Seq.fold (+) ","
    name + "(" + stringOfArgs + ")"
  | SymblBinary (l, rel, r) ->
    (convSymblToStrRec l) + " " + rel + " " + (convSymblToStrRec r)
  | SymblProd args -> // a product / array
    let stringOfArgs = List.map convSymblToStrRec args |> Seq.fold (+) ","
    "(" + stringOfArgs + ")"

type Variant =
| VariantNull // null value - used for not setted or not returned values
| VariantInt of int64
| VariantString of string
| VariantFloat of float
| VariantArr of Variant[]
| VariantFn of (Variant[]->Variant)
| VariantSymbl of Symbl // used to communicate with Symbols
| VariantDict of Dictionary<string, Variant>

let isVariantSymbl (v:Variant): bool =
  match v with
  | VariantSymbl _ ->  true
  | _ -> false

let checkVariantSameType (a:Variant) (b:Variant): bool =
  match a with
  | VariantNull ->
    match b with
    | VariantNull -> true
    | _ -> false
  | VariantInt _ ->
    match b with
    | VariantInt _ -> true
    | _ -> false
  | VariantString _ ->
    match b with
    | VariantString _ -> true
    | _ -> false
  | VariantFloat _ ->
    match b with
    | VariantFloat _ -> true
    | _ -> false
  | VariantArr _ ->
    match b with
    | VariantArr _ -> true
    | _ -> false
  | VariantFn _ ->
    match b with
    | VariantFn _ -> true
    | _ -> false
  | VariantSymbl _ ->
    match b with
    | VariantSymbl _ -> true
    | _ -> false
  | VariantDict _ ->
    match b with
    | VariantDict _ -> true
    | _ -> false

let rec checkVariantEq (a:Variant) (b:Variant) =
  if not (checkVariantSameType a b) then
    false
  else
    match a with
    | VariantNull -> true // null is always equal
    | VariantSymbl aSymbl ->
      match b with
      | VariantSymbl bSymbl -> aSymbl = bSymbl
      | _ -> false
    | VariantString aStr ->
      match b with
      | VariantString bStr -> aStr = bStr
      | _ -> false
    | VariantInt aInt ->
      match b with
      | VariantInt bInt -> aInt = bInt
      | _ -> false
    | VariantFloat aVal ->
      match b with
      | VariantFloat bVal -> aVal = bVal // IMPL TODO< we should really check if the absolute difference is in range >
      | _ -> false
    | VariantFn _ -> false // can't compare functions
    | VariantArr aArr -> // array
      match b with
      | VariantArr bArr ->

        if (Array.length aArr) = (Array.length bArr) then
          let mutable idx = 0;
          let mutable res = true;
          while idx < (Array.length aArr) do
            res <- res && (checkVariantEq (aArr.[idx]) (bArr.[idx]));
            idx <- idx + 1;
          res
        else
          false
      | _ -> false
    
    // TODO< check dict for equality
 
 // TODO< remove functions and rewrite code to build variants directly >

let makeSymbl (value: Symbl) =
  VariantSymbl value

let makeString (value: string) =
  VariantString value

let makeNull =
  VariantNull

let makeLong (value: int64) = 
  VariantInt value

let makeFloat (value: float) = 
  VariantFloat value

let makeArr (content: Variant[]) =
  VariantArr content

let makeFn (fn_ : Variant[]->Variant) =
  VariantFn fn_

let isArr (var:Variant) =
  match var with
  | VariantArr _ -> true
  | _ -> false

let isNull (var:Variant) =
  match var with
  | VariantNull -> true
  | _ -> false

let isSymbl (var:Variant) =
  match var with
  | VariantSymbl _ -> true
  | _ -> false

// return array of variant or default array which is empty array
let retVariantArrOrDefault (var:Variant): Variant[] =
  match var with
  | VariantArr arr -> arr
  | _ -> [||]

// interprets the Variant as a string and returns it, returns empty string if it is not a string
let retVariantStringOrDefault (var:Variant): string =
  match var with
  | VariantString str -> str
  | _ -> ""

let retVariantRealOrDefault (var:Variant): float =
  match var with
  | VariantFloat value -> value
  | _ -> 0.0

// convert string variant array to native string array
let convStrVariantArrToStrArr (v:Variant): string[] =
  match v with
  | VariantArr vArr ->
    let mutable arr:string[] = [||]
    for i in vArr do
      match i with
      | VariantString valString -> // IMPL< check for string >
        arr <- Array.append arr [|valString|];
      | _ -> ()
    arr
  | _ -> [||]







// -1 to disable debug messages
let mutable debugVerbosity = 10;

// best is to keep it that high to enable all warnings
let mutable warningVerbosity = 100;

let mutable infoVerbosity = 100;

let debug (verbosity:int) (msg:string) =
  let levelAsString = match verbosity with
  | 0 -> " "
  | _ -> (string)verbosity
  
  if verbosity <= debugVerbosity then
    printfn "[d%s] %s" levelAsString msg


let info (verbosity:int) (msg:string) =
  let levelAsString = match verbosity with
  | 0 -> " "
  | _ -> (string)verbosity
  
  if verbosity <= infoVerbosity then
    printfn "[i%s] %s" levelAsString msg

let warning (verbosity:int) (msg:string) =
  let levelAsString = match verbosity with
  | 0 -> " "
  | _ -> (string)verbosity
  
  if verbosity <= warningVerbosity then
    printfn "[w%s] %s" levelAsString msg

let fatal (msg:string) =
  printfn "[fatal] %s" msg
  exit 1
  ()

let assert_ (b:bool) (msg:string) =
  if not b then
    fatal (String.Format ("assertation failed! msg={0}", msg))




// short names of facets/slots
let strGen = "generalization";
let strSpec = "specialization";


// epoch counter for SSA
// used to decide when to create a new SlotHistoryEntry 
let mutable ssaEpoch = 1L;

// history entry of a slot
// history is necessary for SSA
type SlotHistoryEntry = struct
  // SSA epoch of the creation time of this history entry
  // is used to determine if we have to create a new history entry because a new value was written to it
  val creationSsaEpoch: int64

  val value: string->Variant // unitname is passed as arg

  // TODO REFACTOR< make to list >
  // TODO LATER< generalize with some PLL like language >  
  // heuristics are "tacked" to slots, as described in Lenat's dissertation physical page 55
  // heuristics have their own concepts like in EURISKO [EURISKO Micro pdf page 7]
  val heuristicNames: Symbl[]
  
  new (creationSsaEpoch_, value_, heuristicNames_) = {
    creationSsaEpoch = creationSsaEpoch_
    value = value_
    heuristicNames = heuristicNames_
  }
end

type Slot = struct
  val name : string[] // is a array because
                      // Lenat called it "facet" (of concepts) in AM. Facet's can have "sub-facets"

  val mutable getCounter : int64 // counts how many get operations were done overall
  val mutable getCounterHistory: int64[] // historic track keeping of get counters

  // history of values for SSA
  val mutable history: SlotHistoryEntry[]

  new (name_) =
    {name=name_;getCounter=0L;getCounterHistory=[||]; history=[||];}
end

let slotRetTopHistoryEntry (slot: Slot) =
  slot.history.[Array.length slot.history - 1]

// returns the current value
// indirection is necessary because of SSA
let slotRetTopValue (slot: Slot) (subject: string): Variant =
  let topHistoryEntry = slot.history.[Array.length slot.history - 1]
  (topHistoryEntry.value subject)

let slotRetTopHeuristicNames (slot: Slot) (subject: string): Symbl[] =
  let topHistoryEntry = slot.history.[Array.length slot.history - 1]
  topHistoryEntry.heuristicNames


// TODO REFACTOR< rename to slotsRet >
// /param name attribute name to be retrieved
// /param subject object/subect for the call
let slotGet (slots: Slot[]) (name: string[]) (subject: string): Variant =
  match Array.tryFindIndex (fun (e:Slot) -> e.name = name) slots with
    | Some idx ->
       slots.[idx].getCounter <- slots.[idx].getCounter + 1L; // bump get counter
       slotRetTopValue slots.[idx] subject
    | None   -> (makeNull)

let slotGetHeuristicNames (slots: Slot[]) (name: string[]) (subject: string): Symbl[] =
  match Array.tryFindIndex (fun (e:Slot) -> e.name = name) slots with
  | Some idx ->
      slots.[idx].getCounter <- slots.[idx].getCounter + 1L; // bump get counter
      slotRetTopHeuristicNames slots.[idx] subject
  | None   -> [||]

// updates the value
//
// if the slot has the same SSA-epoch - then we simply overwrite the value
// else - then we create a new history entry
// a special case if if no history entries exist - then we simply create it
let _slotPutAtIdx (slots: Slot[]) (slotIdx:int) (updateSlotHistoryEntryFn:(SlotHistoryEntry->SlotHistoryEntry)) (createSlotHistoryEntryFn:(unit->SlotHistoryEntry)) =
  // creates a new history entry and appends it
  let appendSlotHistoryEntry () =
    let createdHistoryEntry = (createSlotHistoryEntryFn ())
    slots.[slotIdx].history <- Array.append slots.[slotIdx].history [|createdHistoryEntry|]
    
  if Array.isEmpty slots.[slotIdx].history then
    appendSlotHistoryEntry () // special case when no history entry exists yet
  else
    let topEntry = slotRetTopHistoryEntry slots.[slotIdx]
    if topEntry.creationSsaEpoch = ssaEpoch then
      // we can just overwrite it

      let oldSlotHistoryEntry = slots.[slotIdx].history.[Array.length slots.[slotIdx].history - 1]
      
      slots.[slotIdx].history.[Array.length slots.[slotIdx].history - 1] <- (updateSlotHistoryEntryFn oldSlotHistoryEntry)
    else
      // we need to create a entry and append it
      appendSlotHistoryEntry ()

let slotPut (slots: Slot[]) (name: string[]) (subject: string) (value:Variant): bool =
  match Array.tryFindIndex (fun (e:Slot) -> e.name = name) slots with
    | Some idx ->
      // function which updates a slotHistoryEntry
      let updateSlotHistoryEntryFn (oldSlotHistoryEntry: SlotHistoryEntry): SlotHistoryEntry =
        new SlotHistoryEntry(oldSlotHistoryEntry.creationSsaEpoch, (fun x -> value), oldSlotHistoryEntry.heuristicNames)
      
      let createSlotHistoryEntryFn (): SlotHistoryEntry =
        new SlotHistoryEntry(ssaEpoch, (fun x -> value), [||])
      
      _slotPutAtIdx slots idx updateSlotHistoryEntryFn createSlotHistoryEntryFn
      true
    | None   ->
      printfn "[w ] slotPut failed! name=%s" (convPath name)
      false
      


let slotPutHeuristicNames (slots: Slot[]) (name: string[]) (subject: string) (heuristicNames:Symbl[]): bool =
  match Array.tryFindIndex (fun (e:Slot) -> e.name = name) slots with
  | Some idx ->
    // function which updates a slotHistoryEntry
    let updateSlotHistoryEntryFn (oldSlotHistoryEntry: SlotHistoryEntry): SlotHistoryEntry =
      new SlotHistoryEntry(oldSlotHistoryEntry.creationSsaEpoch, oldSlotHistoryEntry.value, heuristicNames)
    
    let createSlotHistoryEntryFn () =
      new SlotHistoryEntry(ssaEpoch, (fun x -> makeNull), heuristicNames)
    
    _slotPutAtIdx slots idx updateSlotHistoryEntryFn createSlotHistoryEntryFn
    true
  | None ->
    printfn "[w ] slotPutHeuristicNames failed! name=%s" (convPath name)
    false

let slotHas (slots: Slot[]) (name: string[]) =
  // TODO< search all slots and return true if it has name >
  let mutable has = false;
  
  let mutable i = 0;
  while i < slots.Length do
    if slots.[i].name = name then
      has <- true;
      //break;// true;
    i <- i+1;
  has
  



type Concept = struct
  // slots as defined in PLL
  val mutable slots: Slot[]

  new (slots_) = {slots=slots_;}
end

// helper to return the name of a concept
let retConceptName (concept:Concept): Symbl =
  // TODO< check for correct type (must be string)
  match (slotGet concept.slots [|"name"|] "") with
  | VariantSymbl name -> name
  | _ ->
    warning 0 "concept has no name!!!" // IMPl< should this be fatal? >
    SymblName ""

// return the value of the slot or a invariant null
let conceptRetSlotOrNull (concept:Concept) (slotAdress:string[]): Variant = 
  if slotHas concept.slots slotAdress then
    slotGet concept.slots slotAdress ""
  else
    makeNull // IMPL< return null as default >

// helper for concept
// 
// returns if the domain and range point to the same concepts
let conceptIsOperationWithSameDomainAndRange (concept:Concept) =
  let conceptName = conceptRetSlotOrNull concept [|"name"|];

  // TODO< check if it isa Operation or if a generalization isa Operation >
  
  let domainRanges: Variant = conceptRetSlotOrNull concept [|"domain-range"|];
  
  

  let mutable idx = 0
  let mutable res = false
  while idx < (retVariantArrOrDefault domainRanges |> Array.length) do
    let iDomainRange: Variant = (retVariantArrOrDefault domainRanges).[idx];
    
    match iDomainRange with
    | VariantSymbl domainRange -> // IMPL< must be Symb* ! >
      match domainRange with
      | SymblBinary (l, "-->", r) ->
        match l with
        | SymblProd lp ->
          res <- List.forall (fun a -> a = r) lp
        | _ ->
          warning 0 (String.Format ("conceptIsOperationWithSameDomainAndRange() called for concept={0} which had invalid symbolic description!", conceptName));
      | _ ->
        warning 0 (String.Format ("conceptIsOperationWithSameDomainAndRange() called for concept={0} which had invalid symbolic description!", conceptName));
    | _ ->
      warning 0 (String.Format ("conceptIsOperationWithSameDomainAndRange() called for concept={0} which had no valid range!", conceptName));

    idx <- idx + 1
  
  res
