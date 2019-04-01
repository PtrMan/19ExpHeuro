// used for dev debugging and experimentation

module Dev

open System

open BaseStructures
open Heuro
open HInterpreter1

let runDev0 () =
  agenda.tasks <- [||]; // IMPL< flush tasks >
  // IMPL< ... to insert test task >
  let insertTestTasks () = 
    let mutable task = new Task(taskIdCounter, 0.5);
    taskIdCounter <- taskIdCounter + 1L;
    task.taskType <- "fillin";
    task.manipulatedConceptName <- SymblName "Compose"; // TODO< choose better concept which makes sense >
    task.facetName <- "suggest";

    // TODO< put reasons into task >

    task.name_hr <- "Fill in facet="+task.facetName+" of the Concept="+(convSymblToStrRec task.manipulatedConceptName)
    
    // TODO< insert by priority >
    agenda.tasks <- Array.append [|task|] agenda.tasks;
  (insertTestTasks ());



  // bump interestingness so the heuristic processes the concept
  let conceptnameOfBumpedInterestingness = SymblName "Compose";
  updateConceptSlot conceptnameOfBumpedInterestingness [|"interestingness"|] (makeFloat 0.999);

  // let Heuro do it's magic
  (selectTaskAndProcess ())

  info 0 "terminated gracefully"



  agenda.tasks <- [||];
  concepts <- [||]; // reset all concepts for the next test

// program which tests the HInterpreter1
let devTest () =
  // IMPL< flush >
  agenda.tasks <- [||];
  concepts <- [||];

  // IMPL< add heuristic which is manipulated >
  // IMPL< heuristicActions are not real actions but actions to simply see if the array is manipulated correctly
  conceptsAdd {name=SymblName "TestHeuristic"; usefulness=0.1} [("h1Actions",
    VariantArr [|
      VariantArr [|VariantString "0"; |]
      VariantArr [|VariantString "1"; |]
      VariantArr [|VariantString "2"; |]
      VariantArr [|VariantString "3"; |]
    |]) ]

    

  // run interpreter to manipulate concept
  heuristicAlgo1Interpreter {manipulatedHeuristic=concepts.[0]; repr=["remove"; "0"; "action"]}

  info 0 "terminated gracefully"

  ()

  // program which tests some functionality of a heuristic which is described by an concept
let devTestInterpretedHeuristic () =
  // IMPL< flush >
  agenda.tasks <- [||];
  concepts <- [||];

  // add concept which stores the heuristic (under test)
  conceptsAdd {name=SymblName "TestHeuristic"; usefulness=0.1} [("h1Actions",
    VariantArr [|
      VariantArr [|VariantString "remove"; VariantString "0"; VariantString  "action" |]
      VariantArr [|VariantString "remove"; VariantString "0"; VariantString  "action" |]
    |]) ]

  
  // add concept which invokes the heuristic
  conceptsAdd {name=SymblName "TestHostConcept"; usefulness=0.1} [
    ("a", VariantNull) // IMPL< slot for testing >
    ]
  
  conceptAppendHeuristicName (SymblName "TestHostConcept") "a" (SymblName "TestHeuristic") |> ignore
  

  // add task
  let insertTestTasks () = 
    let mutable task = new Task(taskIdCounter, 0.5);
    taskIdCounter <- taskIdCounter + 1L;
    task.taskType <- "exec";
    task.manipulatedConceptName <- SymblName "TestHostConcept"; // TODO< choose better concept which makes sense >
    task.facetName <- "a";

    // TODO< put reasons into task >

    task.name_hr <- (String.Format ("Exec {0}.{1}", (convSymblToStrRec task.manipulatedConceptName), task.facetName))
    
    // TODO< insert by priority >
    agenda.tasks <- Array.append [|task|] agenda.tasks;
  (insertTestTasks ());

  // process task
  selectTaskAndProcess ()

  info 0 "terminated gracefully"

  ()


