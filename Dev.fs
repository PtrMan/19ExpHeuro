// used for dev debugging and experimentation


open Heuro

agenda.tasks <- [||]; // IMPL< flush tasks >
// IMPL< ... to insert test task >
let insertTestTasks = 
  let mutable task = new Task(taskIdCounter, 0.5);
  taskIdCounter <- taskIdCounter + 1L;
  task.taskType <- "fillin";
  task.manipulatedConceptName <- "Compose"; // TODO< choose better concept which makes sense >
  task.facetName <- "suggest";

  // TODO< put reasons into task >

  task.name_hr <- "Fill in facet="+task.facetName+" of the Concept="+task.manipulatedConceptName
  
  // TODO< insert by priority >
  agenda.tasks <- Array.append [|task|] agenda.tasks;
(insertTestTasks);



// bump interestingness so the heuristic processes the concept
let conceptnameOfBumpedInterestingness = "Compose";
updateConceptSlot conceptnameOfBumpedInterestingness [|"interestingness"|] (makeFloat 0.999);

// let Heuro do it's magic
(selectTaskAndProcess ())

printfn "[i ] terminated gracefully";
