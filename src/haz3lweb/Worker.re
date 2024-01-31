/**
   Web worker thread.
 */;
//open Haz3lweb;
//open ProgramEvaluator.WorkerImpl;

//let () = () |> init |> register;

print_endline("Worker.re: Registering");
SimpleWorker.EvalWorker.register();
