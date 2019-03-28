// Heuro as a application which can be run

open Heuro

(fillDefaultTasks ())

// main loop
let mutable cycleCnt = 0L;
let mutable forceTermination = false;
while cycleCnt < 10L && not forceTermination do
  printfn "[i ] cycle#=%i" cycleCnt
  printfn "[i ]  stats  abstractCycle=%i" (-1L) // TODO< implement abstract cycles for measuring the computational resources >
  printfn "[i ]  stats  tasks#=%i" (Array.length agenda.tasks)

  if (Array.length agenda.tasks) = 0 then
    forceTermination <- true;

  if (Array.length agenda.tasks) > 0 then
    (selectTaskAndProcess ())

  cycleCnt <- cycleCnt + 1L;

printfn "[i ] terminated gracefully";
