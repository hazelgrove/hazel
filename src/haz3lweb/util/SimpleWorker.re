open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type key = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type request = (key, DHExp.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type eval_result =
  | EvaluationOk(ProgramResult.t)
  | EvaluationFail(ProgramEvaluatorError.t);
[@deriving (show({with_path: false}), sexp, yojson)]
type response = (key, eval_result);

module Request = {
  [@deriving (sexp, yojson)]
  type t = request;
  type u = string;

  let serialize = program => program |> sexp_of_t |> Sexplib.Sexp.to_string;
  let deserialize = sexp => sexp |> Sexplib.Sexp.of_string |> t_of_sexp;
};

module Response = {
  [@deriving (sexp, yojson)]
  type t = response;
  type u = string;

  let serialize = r => Sexplib.(r |> sexp_of_t |> Sexp.to_string);
  let deserialize = sexp => Sexplib.(sexp |> Sexp.of_string |> t_of_sexp);
};

module EvalWorker = {
  let on_request = ((key, d): request) => {
    /* NOTE(andrew): I'm not sure how to properly route settings
       through the abstractions here; this should be done if this is renabled */
    print_endline("EvalWorker: Recieved request, evaluating program");
    let r = d |> Interface.evaluate(~settings=CoreSettings.on);
    let res =
      switch (r) {
      | r => EvaluationOk(r)
      | exception (Interface.EvalError(error)) =>
        EvaluationFail(Program_EvalError(error))
      | exception Interface.DoesNotElaborate =>
        EvaluationFail(Program_DoesNotElaborate)
      };
    (key, res);
  };
  let on_request = (req: Request.u) => {
    req
    |> Request.deserialize
    |> on_request
    |> Response.serialize
    |> Js_of_ocaml.Worker.post_message;
  };
  let register = () => {
    print_endline("EvalWorker: Setting onmessage");
    Js_of_ocaml.Worker.set_onmessage(on_request);
  };
};

/*
 1. Main.on_startup calls:
    1.1 State.init() to initialize the worker
    1.2 and then State.evaluator_subscribe
        with a callback to processes messages from the
         worker by scheduling an action to update the UI
 2. Update.evaluate_and_schedule calls:
     2.1 State.evaluator_next(state, key, d)
         and schedules an action to set UI to resultpending
  */
