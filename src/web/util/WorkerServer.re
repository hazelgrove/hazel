open Util;
module Js = Js_of_ocaml.Js;

[@deriving (sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    /* Projected statics data used by the incremental driver to look up
     * per-id sub-elaborations and co-ctxs. We ship this slice instead of
     * the full StaticsBase.Map.t because the full map transitively contains
     * LivelitCtx entries that embed OCaml closures, which the structured-
     * clone algorithm postMessage uses rejects. Pass the empty slice to
     * opt out of incremental reuse. */
    eval_info_map: Language.EvalInfo.t,
    /* Previous run's incremental map; pass IncrEval.empty on first run. */
    prev: Language.IncrEval.t,
  };
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));
};

module Response = {
  [@deriving (show, sexp, yojson)]
  type value =
    Result.t(
      (Language.Exp.t, Language.EvaluatorState.t),
      Language.ProgramResult.error,
    );
  [@deriving (show, sexp, yojson)]
  type t = list((string, value));

  let (sexp_of_t, t_of_sexp) =
    Util.StructureShareSexp.structure_share_in(sexp_of_t, t_of_sexp);
};

module ClientMessage = {
  [@deriving (show, sexp, yojson)]
  type evaluate = {
    request_id: int,
    batch: Request.t,
  };

  [@deriving (show, sexp, yojson)]
  type t =
    | Evaluate(evaluate);
};

module ServerMessage = {
  [@deriving (show, sexp, yojson)]
  type result = {
    request_id: int,
    response: Response.t,
  };

  [@deriving (show, sexp, yojson)]
  type t =
    | Ack(int)
    | Result(result);
};

let work = (req_value: Request.value): Response.value => {
  let Request.{expr, eval_info_map, prev} = req_value;
  switch (
    Language.Evaluator.evaluate(
      ~prev,
      ~info_map=eval_info_map,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception (Language.EvaluatorError.Exception(reason)) =>
    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exception exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
  | (result, state) =>
    /* Clear transient data before sending to avoid serializing massive
     * amounts of unnecessary data (e.g., app_args can be 100MB+). */
    Ok((result, Language.EvaluatorState.clear_transient(state)))
  };
};

type started_work =
  | Started(Language.Evaluator.yielding_evaluation)
  | Finished(Response.value);

type running = {
  request_id: int,
  key,
  remaining: Request.t,
  completed: Response.t,
  evaluation: Language.Evaluator.yielding_evaluation,
};

type runtime =
  | Idle
  | Running(running);

let slice_step_budget = 5000;
let latest_request: ref(option(ClientMessage.evaluate)) = ref(None);
let runtime = ref(Idle);
let pump_scheduled = ref(false);

let error_response = exn =>
  switch (exn) {
  | Language.EvaluatorError.Exception(reason) =>
    print_endline("EvaluatorError:" ++ Language.EvaluatorError.show(reason));
    Error(Language.ProgramResult.EvaulatorError(reason));
  | exn =>
    print_endline("EXN:" ++ Printexc.to_string(exn));
    Error(Language.ProgramResult.UnknownException(Printexc.to_string(exn)));
  };

let finish_success = ((result, state)): Response.value =>
  Ok((result, Language.EvaluatorState.clear_transient(state)));

let start_work = (req_value: Request.value): started_work => {
  let Request.{expr, eval_info_map, prev} = req_value;
  switch (
    Language.Evaluator.start_yielding_evaluation(
      ~prev,
      ~info_map=eval_info_map,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception exn => Finished(error_response(exn))
  | evaluation => Started(evaluation)
  };
};

let is_latest = request_id =>
  switch (latest_request^) {
  | Some({request_id: latest_request_id, _}) =>
    request_id == latest_request_id
  | None => false
  };

let post_result = (request_id, completed) =>
  if (is_latest(request_id)) {
    Js_of_ocaml.Worker.post_message(
      ServerMessage.Result({
        request_id,
        response: List.rev(completed),
      }),
    );
  };

let schedule = callback => {
  ignore(
    Js.Unsafe.meth_call(
      Js.Unsafe.global,
      "setTimeout",
      [|
        Js.Unsafe.inject(Js.wrap_callback(callback)),
        Js.Unsafe.inject(0.),
      |],
    ),
  );
};

let rec start_next = (request_id, completed, remaining) =>
  switch (remaining) {
  | [] =>
    runtime := Idle;
    post_result(request_id, completed);
  | [(key, req_value), ...remaining] =>
    switch (start_work(req_value)) {
    | Finished(response) =>
      start_next(request_id, [(key, response), ...completed], remaining)
    | Started(evaluation) =>
      runtime :=
        Running({
          request_id,
          key,
          remaining,
          completed,
          evaluation,
        });
      schedule_pump();
    }
  }
and start_latest = () =>
  switch (latest_request^) {
  | None => runtime := Idle
  | Some({request_id, batch}) => start_next(request_id, [], batch)
  }
and finish_running = (running, response) =>
  start_next(
    running.request_id,
    [(running.key, response), ...running.completed],
    running.remaining,
  )
and pump = () => {
  pump_scheduled := false;
  switch (runtime^) {
  | Idle => start_latest()
  | Running(running) when !is_latest(running.request_id) => start_latest()
  | Running(running) =>
    switch (
      Language.Evaluator.run_yielding_slice(
        ~step_budget=slice_step_budget,
        running.evaluation,
      )
    ) {
    | exception exn => finish_running(running, error_response(exn))
    | EvaluationCompleted(value) =>
      finish_running(running, finish_success(value))
    | EvaluationStepLimitExceeded =>
      finish_running(
        running,
        Error(
          Language.ProgramResult.UnknownException("Step limit exceeded"),
        ),
      )
    | EvaluationYielded(evaluation) =>
      runtime :=
        Running({
          ...running,
          evaluation,
        });
      schedule_pump();
    }
  };
}
and schedule_pump = () =>
  if (! pump_scheduled^) {
    pump_scheduled := true;
    schedule(pump);
  };

let on_request = (msg: ClientMessage.t): unit => {
  let ClientMessage.Evaluate(request) = msg;
  latest_request := Some(request);
  Js_of_ocaml.Worker.post_message(ServerMessage.Ack(request.request_id));
  schedule_pump();
};

let start = () => Js_of_ocaml.Worker.set_onmessage(on_request);
