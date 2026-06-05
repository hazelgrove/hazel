open Util;
module Js = Js_of_ocaml.Js;

[@deriving (show, sexp, yojson)]
type key = string;

module Request = {
  [@deriving (show, sexp, yojson)]
  type value = {
    expr: Language.Exp.t,
    eval_info_map: Language.EvalInfo.t,
    prev: Language.EvaluatorState.incr_eval,
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
  type ack = {
    request_id: int,
    initial: list((key, Language.IncrEval.t(Language.EvaluatorState.t))),
  };

  [@deriving (show, sexp, yojson)]
  type stream = {
    request_id: int,
    key,
    update: Language.IncrEval.t(Language.EvaluatorState.t),
  };

  [@deriving (show, sexp, yojson)]
  type result = {
    request_id: int,
    response: Response.t,
  };

  [@deriving (show, sexp, yojson)]
  type t =
    | Ack(ack)
    | Stream(stream)
    | Result(result);
};

let evaluate_sync = (req_value: Request.value): Response.value => {
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
  | (result, state) => Ok((result, state))
  };
};

type evaluation_start =
  | Yielding(Language.Evaluator.yielding_evaluation)
  | CompletedImmediately(Response.value);

type running = {
  request_id: int,
  key,
  remaining: Request.t,
  completed: Response.t,
  evaluation: Language.Evaluator.yielding_evaluation,
};

type runtime =
  | Idle
  | Starting
  | Running(running);

type model = {
  latest_request: option(ClientMessage.evaluate),
  runtime,
  slice_already_scheduled: bool,
};

let slice_step_budget = 5000;
let initial_model = {
  latest_request: None,
  runtime: Idle,
  slice_already_scheduled: false,
};

/* Worker execution model:
 * - `on_request` records only the newest batch and immediately ACKs with
 *   predicted reusable entries for UI tinting.
 * - The worker evaluates one batch item at a time in small async slices.
 * - After each yielded slice, completed cache entries are streamed to the UI.
 * - If a newer request arrives, the next scheduled slice abandons the stale
 *   batch and begins the latest one. */

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
  Ok((result, state));

let predict_reuse_for_request = ((key, req_value): (key, Request.value)) => {
  let Request.{expr, eval_info_map, prev} = req_value;
  let stream =
    switch (
      Language.ReusePass.reuse_pass(
        ~prev,
        ~info_map=eval_info_map,
        ~env=Language.Builtins.env_init,
        expr,
      )
    ) {
    | exception _ => Language.IncrEval.empty
    | stream => stream
    };
  (key, stream);
};

let start_evaluation = (req_value: Request.value): evaluation_start => {
  let Request.{expr, eval_info_map, prev} = req_value;
  switch (
    Language.Evaluator.start_yielding_evaluation(
      ~prev,
      ~info_map=eval_info_map,
      ~env=Language.Builtins.env_init,
      expr,
    )
  ) {
  | exception exn => CompletedImmediately(error_response(exn))
  | evaluation => Yielding(evaluation)
  };
};

let is_latest = (model, request_id) =>
  switch (model.latest_request) {
  | Some({request_id: latest_request_id, _}) =>
    request_id == latest_request_id
  | None => false
  };

let post_batch_result = (model, request_id, completed) =>
  if (is_latest(model, request_id)) {
    Js_of_ocaml.Worker.post_message(
      ServerMessage.Result({
        request_id,
        response: List.rev(completed),
      }),
    );
  };

let post_stream_update =
    (
      model,
      request_id,
      key,
      update: Language.IncrEval.t(Language.EvaluatorState.t),
    ) =>
  if (is_latest(model, request_id) && !Id.Map.is_empty(update.entries)) {
    Js_of_ocaml.Worker.post_message(
      ServerMessage.Stream({
        request_id,
        key,
        update,
      }),
    );
  };

let flush_stream_update = (model, request_id, key, evaluation) => {
  let update = Language.Evaluator.drain_streaming_outbox(evaluation);
  post_stream_update(model, request_id, key, update);
};

let post_ack = request =>
  Js_of_ocaml.Worker.post_message(
    ServerMessage.Ack({
      request_id: request.ClientMessage.request_id,
      initial: List.map(predict_reuse_for_request, request.batch),
    }),
  );

let schedule_async = callback => {
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

let rec evaluate_next_batch_item = (model, request_id, completed, remaining) =>
  switch (remaining) {
  | [] =>
    let model = {
      ...model,
      runtime: Idle,
    };
    post_batch_result(model, request_id, completed);
    model;
  | [(key, req_value), ...remaining] =>
    switch (start_evaluation(req_value)) {
    | CompletedImmediately(response) =>
      evaluate_next_batch_item(
        model,
        request_id,
        [(key, response), ...completed],
        remaining,
      )
    | Yielding(evaluation) =>
      let model = {
        ...model,
        runtime:
          Running({
            request_id,
            key,
            remaining,
            completed,
            evaluation,
          }),
      };
      model;
    }
  }
and begin_latest_batch = model =>
  switch (model.latest_request) {
  | None => {
      ...model,
      runtime: Idle,
    }
  | Some({request_id, batch}) =>
    evaluate_next_batch_item(model, request_id, [], batch)
  }
and finish_current_item = (model, running, response) =>
  evaluate_next_batch_item(
    model,
    running.request_id,
    [(running.key, response), ...running.completed],
    running.remaining,
  )
and run_scheduled_slice = model => {
  let model = {
    ...model,
    slice_already_scheduled: false,
  };
  switch (model.runtime) {
  | Idle => model
  | Starting => begin_latest_batch(model)
  | Running(running) when !is_latest(model, running.request_id) =>
    begin_latest_batch(model)
  | Running(running) =>
    switch (
      Language.Evaluator.run_yielding_slice(
        ~step_budget=slice_step_budget,
        running.evaluation,
      )
    ) {
    | exception exn =>
      finish_current_item(model, running, error_response(exn))
    | EvaluationCompleted(value) =>
      flush_stream_update(
        model,
        running.request_id,
        running.key,
        running.evaluation,
      );
      finish_current_item(model, running, finish_success(value));
    | EvaluationYielded(evaluation) =>
      flush_stream_update(model, running.request_id, running.key, evaluation);
      let model = {
        ...model,
        runtime:
          Running({
            ...running,
            evaluation,
          }),
      };
      model;
    }
  };
};

let install_message_handler = () => {
  let model = ref(initial_model);

  let rec commit = next_model => {
    let should_schedule_slice =
      switch (next_model.runtime) {
      | Idle => false
      | Starting
      | Running(_) => !next_model.slice_already_scheduled
      };
    model :=
      should_schedule_slice
        ? {
          ...next_model,
          slice_already_scheduled: true,
        }
        : next_model;
    if (should_schedule_slice) {
      schedule_async(() => commit(run_scheduled_slice(model^)));
    };
  };

  let on_request = (msg: ClientMessage.t): unit => {
    let ClientMessage.Evaluate(request) = msg;
    post_ack(request);
    commit({
      ...model^,
      latest_request: Some(request),
      runtime: Starting,
    });
  };

  Js_of_ocaml.Worker.set_onmessage(on_request);
};
