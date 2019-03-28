



// references
// AM
// [Lenat phd dissertation]    "AM: An artificial intelligence approach to discovery in mathematics as heuristic search"

// [AM CASE]                   "AM: A Case Study in AI Methodology"

// EURISKO
// [EURISKO Micro]             "Heuristic Search for New Microcircuit Structures: An Application of Artificial Intelligence"
// [I]
// [II]
// [III]                       "Eurisko: A program that learns new heuristics and domain concepts: The nature of Heuristics III: Program design and results"


open System
open System.Collections.Generic





// helper to return the path as a dot
let convPath (path:string[]) =
  String.concat "." path


let dist (arr:float[]) = Array.map2 (*) arr arr |> Array.sum |> sqrt

// computes the usefulness "Worth" of a overall task
// [Lenat phd dissertation page pdf 47]
// TODO< use for usefulness / "Worth" computation of task >
let calcUsefulness (reasonRating:float[]) (act:float) (facet:float) (concept:float) =
  (dist reasonRating) * (0.2*act + 0.3*facet + 0.5*concept)


// Symbolic experession for symbolic descriptions and later Predicate Calculus etc
type Symbl =
| SymblName of string // name or something
| SymblFn of string * Symbl list // function like "a(b, c)"
| SymblBinary of Symbl * string * Symbl // binary relation like "a = b"


// IMPL TODO< refactor to type without a struct >
type Variant = struct
  val type_: int // datatype : 0 : null, 1: string, 2: array, 3: long, 4: float, 5: function, 6: Symbl
  val mutable valInt: int64
  val mutable valString: string
  val mutable valFloat: float
  val mutable valArr: Variant[]
  
  // used to pass functions around and call them dynamically with any arguments
  val mutable valFn: (Variant[]->Variant) option
  
  val mutable valSymbl: Symbl option

  new (type__) =
      {type_ = type__; valInt=0L; valString=""; valFloat=0.0; valArr=[||]; valFn=None; valSymbl=None;}
end

let makeSymbl (value: Symbl) =
  let mutable res = new Variant(6);
  res.valSymbl <- Some value;
  res

let makeString (value: string) =
  let mutable res = new Variant(1);
  res.valString <- value;
  res

let makeNull =
  new Variant(0)

let makeLong (value: int64) = 
  let mutable res = new Variant(3);
  res.valInt <- value;
  res

let makeFloat (value: float) = 
  let mutable res = new Variant(4);
  res.valFloat <- value;
  res

let makeArr (content: Variant[]) =
  let mutable res = new Variant(2);
  res.valArr <- content;
  res

let makeFn (fn_ : Variant[]->Variant) =
  let mutable res = new Variant(5);
  res.valFn <- Some fn_;
  res

let isArr (var:Variant) = var.type_ = 2
let isNull (var:Variant) = var.type_ = 0

// convert string variant array to native string array
let convStrVariantArrToStrArr (v:Variant): string[] =
  if v.type_ = 2 then // IMPL< check for array >
    let mutable arr:string[] = [||]
    for i in v.valArr do
      if i.type_ = 1 then // IMPL< check for string >
        arr <- Array.append arr [|i.valString|];
    arr
  else
    [||]




// short names of facets/slots
let strGen = "generalization";
let strSpec = "specialization";







type Slot = struct
  val name : string[] // is a array because
                      // Lenat called it "facet" (of concepts) in AM. Facet's can have "sub-facets"
  val mutable value : string->Variant // unitname is passed as arg

  val mutable getCounter : int64 // counts how many get operations were done overall
  val mutable getCounterHistory: int64[] // historic track keeping of get counters

  // TODO LATER< generalize with some PLL like language >  
  // heuristics are "tacked" to slots, as described in Lenat's dissertation physical page 55
  // heuristics have their own concepts like in EURISKO [EURISKO Micro pdf page 7]
  val mutable heuristicNames: string[]

  new (name_, value_) =
    {name=name_;value=value_;getCounter=0L;getCounterHistory=[||];heuristicNames=[||];}
end



// /param name attribute name to be retrieved
// /param subject object/subect for the call
let slotGet (slots: Slot[]) (name: string[]) (subject: string): Variant =
  match Array.tryFindIndex (fun (e:Slot) -> e.name = name) slots with
    | Some idx ->
       slots.[idx].getCounter <- slots.[idx].getCounter + 1L; // bump get counter
       let r2: string->Variant = slots.[idx].value;
       r2 (subject)
    | None   -> (makeNull)

let slotPut (slots: Slot[]) (name: string[]) (subject: string) (value:Variant): bool =
  match Array.tryFindIndex (fun (e:Slot) -> e.name = name) slots with
    | Some idx ->
       slots.[idx].value <- fun e -> value;
       true
    | None   ->
       printfn "[w ] slotPut failed! name=%s" (convPath name)
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




// helper to change the usefulness of a concept by slots
let changeUsefulness (slots: Slot[]) (delta:float) =
  let mutable usefulness = (slotGet slots [|"usefulness"|] "").valFloat;
  usefulness <- usefulness + delta;
  usefulness <- min usefulness 1.0;
  usefulness <- max usefulness 0.0;
  slotPut slots [|"usefulness"|] "" (makeFloat usefulness);





type Concept = struct
  // slots as defined in PLL
  val mutable slots: Slot[]

  new (slots_) = {slots=slots_;}
end

// helper to return the name of a concept
let retConceptName (concept:Concept) =
  // TODO< check for correct type (must be string)
  (slotGet concept.slots [|"name"|] "").valString

// return the value of the slot or a invariant null
let conceptRetSlotOrNull (concept:Concept) (slotAdress:string[]): Variant = 
  if slotHas concept.slots slotAdress then
    slotGet concept.slots slotAdress ""
  else
    makeNull // IMPL< return null as default >


// reason for a task

type Reason = struct
  // TODO< add fields >
  
  val worth: float // 0.0 to 1.0
end

type Task = struct
  val id: int64 // task id - used for logging and referencing from outside system

  val mutable name_hr: string // human readable name, can be empty if generated by machine
  
  val mutable manipulatedConceptName: string // name of the manipulated concept
  val mutable facetName: string // name of the "facet"/item which is touched
  val mutable taskType: string // currently only support "fillin"

  // tasks can have reasons
  // see [Lenat phd dissertation page pdf 25]
  // see [Lenat phd dissertation page pdf 41]
  //
  // * reasons are added by heuristics [Lenat phd dissertation page pdf 163]
  //   * adding of a reason forces the recomputation of the priority of the task [Lenat phd dissertation page pdf 163]
  val mutable reasons: Reason[]


  val mutable priority: float // from 0.0 to 1.0

  new (id_, initialPriority) = {
    id=id_;
    priority=initialPriority;
    name_hr="";
    manipulatedConceptName="";
    facetName="";
    taskType="";
    reasons=[||];}
end







// used for carrying context for heuristic invocation 
type HeuristicInvocationCtx = struct
  val task: Task // task is the current task
  val concept: Concept // concept the evaluated concept of the heuristic   MAY NOT BE HOST CONCEPT!

  // TODO< other fields from AM >
  // TODO< work out arguments, I believe that argument and facet are passed into it >

  // TODO< mutable fields which can be mutated by the actions of the heuristic >

  new (task_, concept_) = {task=task_;concept=concept_;}
end





type Agenda = struct
  // tasks ordered by priority - high priority items appear first
  val mutable tasks: Task[]

  new (null_) = {tasks=[||];}
end

let mutable taskCounter: int64 = 0L; // global task counter - each task has a unique id. This is require ust for logging/reference purposes













// http://www.cs.northwestern.edu/~mek802/papers/not-mine/Lenat_EURISKO.pdf
// GET
// PUT






(* commented because it was just an ealy idea and a early test
// build a concept of an algorithm which can be applied
let buildConceptAlgorithm (name:string): Concept =
  let slots = [|
    new Slot([|"name"|], fun a -> makeString name);
    new Slot([|"isa2"|], fun a -> (makeString "algorithm")); // TODO< make up a isa relationship >
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|"interestingness"|], fun a -> makeLong 500L);

    new Slot([|"is-operator"|], fun a -> (makeString "true")); // algorithm can be called as operator
    |]

  let concept = new Concept(slots);
  concept

buildConceptAlgorithm "sdr-sim"; // provide similarity of sdr's
buildConceptAlgorithm "ga"; // provide genetic algorithm
*)


(*
changeUsefulness conceptTest.slots 50

printfn "usefulness=%i" (slotGet conceptTest.slots [|"usefulness"|] "").valInt

*)






(*
type Heuristic = struct
  // TODO< refactor to only left side >
  val leftSideFirst: HeuristicInvocationCtx->bool 
  
  val leftSideFollowing: (HeuristicInvocationCtx->bool)[]

  val actions: (HeuristicInvocationCtx->unit)[]
end

let checkLeftSide (heuristic:Heuristic) (invocationCtx:HeuristicInvocationCtx) =
  if not (heuristic.leftSideFirst invocationCtx) then
    false
  else
    let rec checkLeftSideRec (rem:(HeuristicInvocationCtx->bool)[]) =
      if (Array.length rem) = 0 then
        true
      elif not (rem.[0] invocationCtx) then
        false
      else
        checkLeftSideRec (Array.sub rem 1 ((Array.length rem)-1)) // call recursivlyy
    (checkLeftSideRec heuristic.leftSideFollowing)

*)

// heuristic which can be used by the system
// is pointed at by heuristics which are saved as concepts just like in EURISKO
// see [Lenat phd dissertation page pdf 42] for the treatment in AM
type Heuristic2 = struct 
  val name: string
  val leftSide: (HeuristicInvocationCtx->bool)[] // preconditions
                                                 // first is special in AM but we abstracted the "specialness" away
  
  //val actions: (HeuristicInvocationCtx->unit)[]

  new (name_, leftSide_) = {name=name_;leftSide=leftSide_;}
end


// decouples the name of the heuristic from the function
// IMPL< necessary because we can't store the function inside the Concept as a item! >
let mutable heuristics2: Heuristic2[] = [||];

let retHeuristicByName (name:string) =
   match Array.tryFindIndex (fun (e:Heuristic2) -> e.name = name) heuristics2 with
    | Some idx -> Some (heuristics2.[idx])
    | None   -> None


let checkLeftSide (heuristic:Heuristic2) (invocationCtx:HeuristicInvocationCtx) =
  let rec checkLeftSideRec (rem:(HeuristicInvocationCtx->bool)[]) =
    if (Array.length rem) = 0 then
      true
    elif not (rem.[0] invocationCtx) then
      false
    else
      checkLeftSideRec (Array.sub rem 1 ((Array.length rem)-1)) // call recursivly
  (checkLeftSideRec heuristic.leftSide)




// global agenda
let mutable agenda: Agenda = new Agenda(());

let mutable taskCnt = 0L; // task counter

// all (global) concepts
let mutable concepts: Concept[] = [||];

let retConceptByName (name:string) =
  match Array.tryFindIndex (fun (e:Concept) -> (retConceptName e) = name) concepts with
    | Some idx -> Some (concepts.[idx])
    | None   -> None


// updates the value of a slot or creates the slot and set the value
// /param name path of the slot
let updateConceptSlot (conceptName:string) (name:string[]) (value:Variant) =
  match Array.tryFindIndex (fun (e:Concept) -> (retConceptName e) = conceptName) concepts with
    | Some idx ->
      let c = (concepts.[idx]);

      if not (slotHas c.slots name) then
        concepts.[idx].slots <- Array.append concepts.[idx].slots [|new Slot(name, fun e -> value)|];
      
      slotPut concepts.[idx].slots name "" value;

      ()

    | None   -> ()

// tries to add a heuristic name to a slot of a concept
// /param conceptName name of the concept
// /param slotName name of the slot, Lenat calls these "facet"
// /param addedHeuristicName added heuristic name
let conceptAppendHeuristicName (conceptName:string) (slotName:string) (addedHeuristicName:string) =
  match Array.tryFindIndex (fun (e:Concept) -> (retConceptName e) = conceptName) concepts with
  | Some conceptIdx ->

    match Array.tryFindIndex (fun (e:Slot) -> e.name = [|slotName|]) concepts.[conceptIdx].slots with
    | Some idx ->
       concepts.[conceptIdx].slots.[idx].heuristicNames <- Array.append concepts.[conceptIdx].slots.[idx].heuristicNames [|addedHeuristicName|];
    | None   ->
      printfn "[w2] couldn't find slot=%s of concept=%s" slotName conceptName
      ()
  | None ->
    printfn "[w2] couldn't find concept=%s" conceptName
    ()


//////////////////////
/// algorithms for calling by the AI

let algorithm_union (args:Variant[]): Variant =
  let mutable resArr:Variant[] = [||];
  
  // TODO< implement fully >
  let isAlreadyInResult (checked:Variant) =
    false

  for iArg in args do
    // IMPL NOTE< arguments must be arrays! >
    for iiArg in iArg.valArr do
      if not (isAlreadyInResult iiArg) then
        resArr <- Array.append resArr [|iiArg|]

  makeArr resArr


//////////////////////
/// fill concepts with hardcoded concepts from [Lenat phd dissertation]
let fillLenatConcepts =
  let mutable slots: Slot[] = [||];
  
  // is important! see [Lenat phd dissertation page pdf 138] for justification
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Equality");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.8); // boosted usefulness
    //new Slot([|strGen|], fun a -> makeString "TODO");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];



  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Anything");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  // see [Lenat phd dissertation page pdf 113]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Any-concept");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Anything");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 113]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Object");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Any-concept");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  // see [Lenat phd dissertation page pdf 113]
  // "Structure" means data-structure here!
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Structure");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Object");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 113]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Ordered");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Structure");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 113]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Unordered");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Structure");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Numbers");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 113]
  // "Activity represents something that can be performed"
  //   [Lenat phd dissertation page pdf 114]
  
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Activity");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Any-concept");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 113]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Operation");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Activity");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Predicate");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Activity");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  
  // see [Lenat phd dissertation page pdf 215]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Atom-obj");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.1);
    new Slot([|strGen|], fun a -> makeString "Object");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 215]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "BooleanTruth-Value");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.1);
    new Slot([|strGen|], fun a -> makeString "Atom-obj");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 113]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Lists");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Ordered");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 113]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Sets");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
    new Slot([|strGen|], fun a -> makeString "Unordered");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  

  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Union");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.1); // 0.1 see [Lenat phd dissertation page pdf 196]

    new Slot([|"domain-range"|], fun a -> makeArr[| makeArr [|makeString "Structures";makeString "Structures";makeString "-->";makeString "Structures"|] |] );
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  // see [Lenat phd dissertation page pdf 198]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Set-Union");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.1); // 0.1 see [Lenat phd dissertation page pdf 198]
    new Slot([|strGen|], fun a -> makeString "Union");

    new Slot([|"domain-range"|], fun a -> makeArr [| makeArr [|makeString "Sets";makeString "Sets";makeString "-->";makeString "Sets"|] |] );
    new Slot([|"algorithms"|], fun a -> makeArr [| makeFn algorithm_union |] );
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  
  let composeDefinitionDeclarativeSlowRightSide = SymblFn ("A", [SymblFn ("B", [SymblName "x"])]);
  let composeDefinitionDeclarativeSlowSymbl = SymblBinary (SymblFn ("C", [SymblName "x"]), "=", composeDefinitionDeclarativeSlowRightSide);
  let composeDefinitionDeclarativeSlow = makeSymbl composeDefinitionDeclarativeSlowSymbl;
  
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Compose");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.1);
    new Slot([|strGen|], fun a -> makeString "Operation");
    
    // definitions
    new Slot([|"definition"|], fun a -> makeArr [|
      // declarative slow 
      makeArr[|
        makeArr[|
          makeString "declarative"; makeString "slow";
          
          // C(x) = A(B(x))
          composeDefinitionDeclarativeSlow
        |] |] |]);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  (*

  // see [Lenat phd dissertation page pdf 94]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Sets-union");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 94]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Sets-delete");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 94]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Sets-insert");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 94]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Sets-equal");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

  // see [Lenat phd dissertation page pdf 99]
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Sets-intersect");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.5);
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
   *)

  // TODO< composition from [Lenat phd dissertation page pdf 115] >
  // TODO< delete and delete-ops from [Lenat phd dissertation page pdf 115] >
  // TODO< difference and difference-ops from [Lenat phd dissertation page pdf 115] >
  // TODO< empty-structure [Lenat phd dissertation page pdf 115] >
  // TODO< first-element [Lenat phd dissertation page pdf 115] >
  // TODO< identity [Lenat phd dissertation page pdf 115] >
  // TODO< insert [Lenat phd dissertation page pdf 115] >
  // TODO< intersect [Lenat phd dissertation page pdf 115] >
  // TODO< last-element [Lenat phd dissertation page pdf 115] >
  // TODO< list-delete [Lenat phd dissertation page pdf 115] >
  // TODO< list-diff [Lenat phd dissertation page pdf 115] >
  // TODO< list-insert [Lenat phd dissertation page pdf 115] >
  // TODO< list-intersect [Lenat phd dissertation page pdf 115] >
  // TODO< list-union [Lenat phd dissertation page pdf 115] >
  
  ()


let fillCustomConcepts =
  let mutable slots: Slot[] = [||];
  
  // concept from which all heuristics specialize
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Heuristic");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.999);
    new Slot([|"usefulnessIsFrozen"|], fun a -> makeLong 1L); // freeze usefulness 
    //new Slot([|strGen|], fun a -> makeString "Any");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  // use to refer and store everything related to the AI itself
  // TODO SCIFI< is a good place to add basic emotion parameters >
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Self");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.999);
    new Slot([|"usefulnessIsFrozen"|], fun a -> makeLong 1L); // freeze usefulness 
    //new Slot([|strGen|], fun a -> makeString "Any");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  slots <- [|
    new Slot([|"name"|], fun a -> makeString "Integer");
    new Slot([|"usefulness"|], fun a -> makeFloat 0.999);
    new Slot([|"usefulnessIsFrozen"|], fun a -> makeLong 1L); // freeze usefulness 
    new Slot([|strGen|], fun a -> makeString "Atom-obj");
  |];
  concepts <- Array.append concepts [|new Concept(slots)|];

// we need a way to store the (typed) bodies used by the heuristics
let heuristicActions :Dictionary<string, HeuristicInvocationCtx->unit> = new Dictionary<string, HeuristicInvocationCtx->unit>()


let fillLenatHeuristics =
  // if there are no examples for concept C filled in so far,
  // THEN 
  // consider the task "Fillin examples of C", 
  // for the following reason: 
  // "No examples of C filled in so far", 
  // whose value is half of Worth(C). 
  // If that value is below argi, then forget it; 
  // otherwise, try to add to to the agenda.
  // [Lenat phd dissertation pdf page 107]
  
  
  let heuristicSuggestTaskLeftSide (invocationCtx:HeuristicInvocationCtx) =
    // return true iff Concept has no examples
    not (slotHas invocationCtx.concept.slots [|"examples"|])

  // heuristic to suggest new tasks which may be plausible at the current time
  
  let heuristicSuggestTasksAction (invocationCtx:HeuristicInvocationCtx) =
    let nameOfConcept = (retConceptName invocationCtx.concept);

    let reason_hr = "no examples of "+nameOfConcept+" filled in so far" // human readable reason

    printfn "[d ] heuristicSuggestTasks() called for concept=%s" nameOfConcept
    // TODO< fully implement this heuristic >
  
  heuristicActions.Add("heuristicSuggestTasksAction", heuristicSuggestTasksAction); // IMPL< register action >

  let heuristicName = ("H_"+"suggestTasks");

  heuristics2 <- Array.append heuristics2
    [|new Heuristic2(heuristicName, [|heuristicSuggestTaskLeftSide|])|]; // IMPL< register >
  
  
  // create and add heuristic concept
  let slots =
    [|
    new Slot([|"name"|], fun a -> makeString heuristicName);
    new Slot([|"usefulness"|], fun a -> (makeFloat 0.75));
    new Slot([|"heuristicActions"|], fun a -> (makeArr [|makeString "heuristicSuggestTasksAction"|])); // IMPL< register action for heuristic >
    new Slot([|strGen|], fun a -> makeString "Heuristic"); // "is a" relationship
    |];
  concepts <- Array.append concepts [|new Concept(slots)|];
  
  
  // add heuristic to "Any-concept" concept
  let anyConceptMaybe: Concept option = (retConceptByName "Any-concept");
  
  match anyConceptMaybe with
  | Some c ->
    
    updateConceptSlot "Any-concept" [|"suggest"|] (makeString ""); // IMPL< ensure the slot exists >    
    conceptAppendHeuristicName "Any-concept" "suggest" heuristicName
    
  | None -> 
    printfn "[w ] couldn't find concept=%s" "Any-concept";
  


  ()

let mutable taskIdCounter = 0L;

let fillDefaultTasks =
  let mutable task = new Task(taskIdCounter, 0.5);
  taskIdCounter <- taskIdCounter + 1L;
  task.taskType <- "fillin";
  task.manipulatedConceptName <- "Any-concept"; // TODO< choose better concept which makes sense >
  task.facetName <- "suggest";

  // TODO< put reasons into task >

  task.name_hr <- "Fill in facet="+task.facetName+" of the Concept="+task.manipulatedConceptName
  
  // TODO< insert by priority >
  agenda.tasks <- Array.append [|task|] agenda.tasks;



// fill concepts with hardcoded concepts!
(fillLenatConcepts) // concepts from AM as defined by Lenat
(fillCustomConcepts) // custom concepts

// fill heuristis with hardcoded heuristics
(fillLenatHeuristics)

// fill start tasks
(fillDefaultTasks)



// -1 to disable debug messages
let mutable debugVerbosity = 10;

// best is to keep it that high to enable all warnings
let mutable warningVerbosity = 100;

let debug (verbosity:int) (msg:string) =
  let levelAsString = match verbosity with
  | 0 -> " "
  | _ -> (string)verbosity
  
  if verbosity <= debugVerbosity then
    printfn "[d%s] %s" levelAsString msg

let warning (verbosity:int) (msg:string) =
  let levelAsString = match verbosity with
  | 0 -> " "
  | _ -> (string)verbosity
  
  if verbosity <= warningVerbosity then
    printfn "[w%s] %s" levelAsString msg


// selects a task and processes it
let selectTaskAndProcess =
  // loop consists out of   https://www.quora.com/Has-Douglas-Lenats-EURISKO-research-ever-been-reproduced
  // * Find something to do (pull a high-value task off an agenda)
  //   * Go do it
  //   * Study what you did

  // select task with highest prirority (which is the first task because it is sorted)
  let currentTask = agenda.tasks.[0];

  // work on current task
  // IMPL< we need to iterate over all concepts >
  // MAYBE OPTIMIZATION< efficiency may get improved by caching it like described in [Lenat phd dissertation pdf page around 39] - but why should we do it when we are working with just a few tasks??? >
  for iConcept in concepts do
    debug 0 (String.Format("iterate over concept={0} for search for matching heuristics", retConceptName iConcept));

    let heuristicInvocationCtx = new HeuristicInvocationCtx(currentTask, iConcept);
    
    // IMPL< we need to iterate over all "facets"(slots) to find the heuristics (and apply them)
    for iItem in iConcept.slots do
      debug 7 (String.Format("   has item name={0}", iItem.name.[0]));

      let iHeuristicConceptNames = iItem.heuristicNames; // IMPL< we need to retrieve the names of the heuristics >
      
      // fetch heuristics as described in [Lenat phd dissertation] [AM CASE pdf page 5]
      for iHeuristicConceptName in iHeuristicConceptNames do
        // retrieve heuristic concept

        
        printfn "[d ] found iHeuristicConceptName=%s of concept=%s" iHeuristicConceptName (retConceptName iConcept);
        
        let heuristicConceptOpt: Concept option = (retConceptByName iHeuristicConceptName)
        let mutable heuristicName = iHeuristicConceptName;
        
        let heuristicMaybe: Heuristic2 option = (retHeuristicByName heuristicName);

        match heuristicMaybe with
        | Some heuristic -> 
          // iterate over applied concepts
          // TODO< keep track of resource quota's >
          for iAppliedConcept in concepts do
            let invocationCtx = new HeuristicInvocationCtx(currentTask, iAppliedConcept);
            let heuristicFires = (checkLeftSide heuristic invocationCtx)

            if heuristicFires then
              printfn "[d5] heuristicConceptName=%s heuristic=%s FIRING!" iHeuristicConceptName heuristicName;
              
              let heuristicActionsNamesOpt: string[] option =
                match heuristicConceptOpt with
                | Some heuristicConcept ->
                  Some (conceptRetSlotOrNull heuristicConcept [|"heuristicActions"|] |> convStrVariantArrToStrArr);
                | None ->
                  None

              match heuristicActionsNamesOpt with
              | Some heuristicActionsNames ->
                printfn "[d5] found heuristicActions of heuristic name=%s" heuristicName;

                // execute body of heuristic
                for iHeuristicActionName in heuristicActionsNames do
                  let foundHeuristicAction, heuristicAction = heuristicActions.TryGetValue iHeuristicActionName;

                  if foundHeuristicAction then
                    
                    printfn "[d6]   invoke heuristicAction name=%s for concept=%s" iHeuristicActionName (retConceptName iAppliedConcept);

                    (heuristicAction invocationCtx);
                  else
                    printfn "[w6]   could not find heuristic action name=%s" iHeuristicActionName;

              | None ->
                printfn "[w ] couldn't retrieve heuristicActions of heuristic name=%s" heuristicName;

          ()

        | None ->
          printfn "[w ] heuristicConcept=%s pointed at heuristic=%s which was not found!" iHeuristicConceptName heuristicName;

        
        ()

  // we need to remove the task
  // see  [Lenat phd dissertation pdf page 39]
  agenda.tasks <- Array.sub agenda.tasks 1 ((Array.length agenda.tasks) - 1);

  // insert new tasks
  // TODO< >

// main loop
let mutable cycleCnt = 0L;
let mutable forceTermination = false;
while cycleCnt < 10L && not forceTermination do
  printfn "cycle#=%i" cycleCnt
  printfn "  stats  abstractCycle=%i" (-1L) // TODO< implement abstract cycles for measuring the computational resources >
  printfn "  stats  tasks#=%i" (Array.length agenda.tasks)

  if (Array.length agenda.tasks) = 0 then
    forceTermination <- true;

  if (Array.length agenda.tasks) > 0 then
    (selectTaskAndProcess)

  cycleCnt <- cycleCnt + 1L;


















// Notes about [Lenat phd dissertation]
// 5.2.4 Views
//   * details views and contains an example of a walk of a specialization of a datastructure
//   * contains example on how views update facets


// datastructure and control structure maintainance
// * loop prevention mechanism [Lenat phd dissertation pdf page 96]
// * entry policy for agenda [Lenat phd dissertation pdf page 107]
// * reevaluating priorities of tasks [Lenat phd dissertation pdf page 107]




















/////////////////
/// helpers for heuristics

// helper structure for
// domain and range of a function/predicate
// see [Lenat phd dissertation page pdf 55] for example with a algorithm
type DomainRange = {domain: Variant[]; range: Variant}

// returns the domain/range's of a concept
// see [Lenat phd dissertation page pdf 55] for a detailed discussion of the algorithm
let conceptRetDomainRange (concept:Concept): DomainRange[] =
  let idxOfProj (arr:Variant[]): int = // IMPL< index of "-->" >
    let mutable idx = 0;
    let mutable return_: bool = false;
    while idx < (Array.length arr) && not return_ do
      if arr.[idx].valString = "-->" then
        return_ <- true;
      else
        idx <- idx + 1;
    idx
  
  
  let mutable res: DomainRange[] = [||];
  // IMPL< see [Lenat phd dissertation page pdf 55] for a detailed discussion of the algorithm >
  
  let domRange: Variant = conceptRetSlotOrNull concept [|"domain-range"|]
  if isNull domRange then
    warning 5 "domRange was null because it was not found!";

  if isArr domRange then
    for iDomRange in domRange.valArr do

      if isArr iDomRange then // IMPL< must be array because we store domain/range as array as [|DA DB "-->" Range0|]]>
        let idxOfProj2 = (idxOfProj iDomRange.valArr);

        let domain = Array.sub iDomRange.valArr 0 (idxOfProj2);
        let range = iDomRange.valArr.[idxOfProj2+1];

        res <- Array.append res [|{domain = domain; range = range}|];

  res



// heuristic as described in Lenat's thesis at pdf page 55
// IMPL< D stand for "documented" - because Lenat wrote a detailed description of it >
//
// TODO< add randomness to heuristicBodyD55 >
// TODO< add quota to heuristicBodyD55 >
// TODO< make up small example to (unit) test it!!! >
let heuristicBodyD55 =
  // IMPL< see [Lenat phd dissertation page pdf 55] for a detailed discussion of the algorithm >
  // IMPL< comments are numbered and named after the descriptions in Lenat's thesis >
  
  // TODO< assign real index to it >
  let FIdx: int = -1; // index of the referenced concept by F
  

  
  // IMPL< 1. find the domain of F and call it D >
  let D: DomainRange[] = conceptRetDomainRange concepts.[FIdx];

  // IMPL< 2. find example of D and call them E >
  let E: Variant = conceptRetSlotOrNull concepts.[FIdx] [|"examples"|];
  if isNull E then
    debug 10 "abort heuristicBodyD55() because no examples were found!"
  else
    // IMPL< 3. find an algorithm to compute F and call it A >
    // IMPL<   we do this by randomly choosing a entry from the "algorithm" faces
    let A2: Variant = conceptRetSlotOrNull concepts.[FIdx] [|"algorithms"|];
    if isNull A2 then
      warning 0 "algorithms is null!";
    
    
    let mutable A: Variant = makeNull;
    if not (isNull A2) then
      // TODO< choose random algorithm >
      A <- A2.valArr.[0];
    

    printfn "HERE"

    // TODO< check for quota and update quota >
    // IMPL< loop 4 >
    while true do
      // IMPL< 4a choose any member >
      let E1 = E.valArr.[0].valArr; // HACK TODO< we just choose the first one >
      
      // IMPL< 4b run A on E1 and call the result X >
      let X = match A.valFn with
      | Some fn ->
        debug 10 "run algorithm"
        let res = (fn E1);
        debug 10 "    algorithm was run"
        res
      | None ->
        warning 0 "A was not a valid function!"
        (makeNull)
      
      
      // IMPL< 4c check whether <E1,X> satifies the definition of F >
      let flag = false; // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO;
      // IMPL< 4d if so, then add <E1 --> X> to the examples facet of F >
      
      // adds example to slot or creates slot
      let addToSlot (slotName:string) added = 
        let mutable examples: Variant = (makeNull);
        if slotHas concepts.[FIdx].slots [|slotName|] then
          examples <- slotGet concepts.[FIdx].slots [|slotName|] "";
        
        examples.valArr <- Array.append examples.valArr added;
        slotPut concepts.[FIdx].slots [|slotName|] "" examples;
      
      if flag then
        // IMPL< add <E1 --> X> to the examples of F >
        let mutable added = Array.copy E1;
        added <- Array.append added [|(makeString "-->"); X|];
        addToSlot "examples" added
        ()

      
      // IMPL< 4e if not, then add <E1 --> X> to the non-examples facet of F >
      if not flag then
        // IMPL< add <E1 --> X> to the non-examples of F >
        let mutable added = Array.copy E1;
        added <- Array.append added [|(makeString "-->"); X|];
        addToSlot "non-examples" added
        ()






// can behave as union or append of list
let fuse (isSet:bool) (a:Variant[]) (b:Variant[]): Variant[] =
  let mutable res = Array.copy a;
  
  let isInRes (v:Variant): bool =
    let rec eq (a:Variant) (b:Variant) =
      if a.type_ <> b.type_ then
        false
      else
        match a.type_ with
        | 0 -> true // null is always equal
        | 1 -> a.valString = b.valString
        | 3 -> a.valInt = b.valInt
        | 4 -> a.valFloat = b.valFloat
        | 5 -> false // can't compare functions
        | 2 ->
          // array
          if (Array.length a.valArr) = (Array.length b.valArr) then
            let mutable idx = 0;
            let mutable res = true;
            while idx < (Array.length a.valArr) do
              res <- res && (eq (a.valArr.[idx]) (b.valArr.[idx]));
              idx <- idx + 1;
            res
          else
            false
    
    if isSet then
      Array.exists (eq v) res
    else
      false // IMPL< just append if it has to behave like a array - append >
  
  for iB in b do
    if not (isInRes iB) then
      res <- Array.append res [|iB|];
  
  res
