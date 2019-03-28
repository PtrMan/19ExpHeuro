// Heuro as a application which can be run

open Heuro



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
