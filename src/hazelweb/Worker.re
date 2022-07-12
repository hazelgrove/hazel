/**
   Web worker thread.
 */
open Async_kernel;

open ProgramEvaluator.Sync;

/**
   [respond response] sends [response] to the main thread.
 */
let respond = (res: ProgramResult.t): unit =>
  Js_of_ocaml.Worker.post_message(res);

/**
   [on_request t request] computes the result of [request] and calls [respond].
 */
let on_request = (t: t, req: Program.t): unit =>
  Deferred.upon(req |> get_result(t) |> snd, respond);

let () = {
  let t = init();
  Js_of_ocaml.Worker.set_onmessage(on_request(t));
};
