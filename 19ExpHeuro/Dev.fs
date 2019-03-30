// used for dev debugging and experimentation

module Dev

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
  conceptsAdd {name=SymblName "TestHeuristic"; usefulness=0.1} [("heuristicActions",
    VariantArr [|
      VariantArr [|VariantString "0"; |]
      VariantArr [|VariantString "1"; |]
      VariantArr [|VariantString "2"; |]
      VariantArr [|VariantString "3"; |]
    |]) ]

    

  // run interpreter to manipulate concept
  heuristicAlgo1Interpreter concepts.[0] ["remove"; "0"; "action";]

  info 0 "terminated gracefully"

  ()