/**
   Web worker thread.
 */
open Async_kernel;

open ProgramEvaluator;
open ProgramEvaluator.Make(ProgramEvaluator.Sync);

/**
   [respond result] sends the response containing [result] to the main thread.
 */
let respond = (result: ProgramResult.t) => {
  let res: response = {result: result};
  Js_of_ocaml.Worker.post_message(res);
};

/**
  [on_request t request] computes the result of [request] and sends the
  response.
 */
let on_request = (t: t, {program}: request): unit =>
  Deferred.upon(program |> get_result(t), respond);

let () = {
  let t = init();
  Js_of_ocaml.Worker.set_onmessage(on_request(t));
};
