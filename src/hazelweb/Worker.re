/**
   Web worker thread.
 */
open Lwt.Infix;

open ProgramEvaluator;
open ProgramEvaluator.Sync;

/**
   [respond response] sends [response] to the main thread.
 */
let respond = (res: program_result): unit => {
  print_endline("posted response");
  Js_of_ocaml.Worker.post_message(res);
};

/**
   [on_request t request] computes the result of [request] and calls [respond].
 */
let on_request = (t: t, req: Program.t): unit => {
  print_endline("received request");
  /* FIXME: Catch exceptions. */
  let (_, res) = req |> get_result(t);
  res >|= respond |> ignore;
};

let () = {
  let t = init();
  Js_of_ocaml.Worker.set_onmessage(on_request(t));
};
