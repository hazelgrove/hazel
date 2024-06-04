open Sexplib.Std;

// TODO[Matt]: combine into one module

module Result = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | BoxedValue(DHExp.t)
    | Indet(DHExp.t);

  let unbox =
    fun
    | BoxedValue(d)
    | Indet(d) => d;

  let fast_equal = (r1, r2) =>
    switch (r1, r2) {
    | (BoxedValue(d1), BoxedValue(d2))
    | (Indet(d1), Indet(d2)) => DHExp.fast_equal(d1, d2)
    | _ => false
    };
};

/**
  The result of a program evaluation. Includes the {!type:EvaluatorResult.t},
  the {!type:EvaluatorState}, and the tracked hole instance information
  ({!type:HoleInstanceInfo.t}). Constructed by {!val:Program.get_result}.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type inner = {
  result: Result.t,
  state: EvaluatorState.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Timeout
  | EvaulatorError(EvaluatorError.t)
  | UnknownException(string);

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | Off(Elaborator.Elaboration.t)
  | ResultOk('a)
  | ResultFail(error)
  | ResultPending;

let get_dhexp = (r: inner) => Result.unbox(r.result);
let get_state = (r: inner) => r.state;

let map = (f: 'a => 'b, r: t('a)) =>
  switch (r) {
  | Off(elab) => Off(elab)
  | ResultOk(a) => ResultOk(f(a))
  | ResultFail(e) => ResultFail(e)
  | ResultPending => ResultPending
  };
