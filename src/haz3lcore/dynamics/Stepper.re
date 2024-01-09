open Sexplib.Std;
open EvaluatorStep;
open Transition;

type step_with_previous = {
  step,
  previous: option(step),
  hidden: list(step),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type current =
  | StepperOK(DHExp.t)
  | StepperError(ProgramEvaluatorError.t) // Must have at least one in previous
  | StepTimeout // Must have at least one in previous
  | StepPending(DHExp.t, option(EvalObj.t)); // StepPending(_,Some(_)) cannot be saved

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* Might be different to first expression in previous because
     steps are taken automatically (this may no longer be true - Matt) */
  elab: DHExp.t,
  previous: list(step),
  current,
  next: list(EvalObj.t),
};

let get_elab = ({elab, _}: t) => elab;

let get_next_steps = s => s.next;

let current_expr = (s: t) =>
  switch (s.current, s.previous) {
  | (StepperOK(d), _)
  | (StepPending(d, _), _) => d
  | (StepperError(_) | StepTimeout, [x, ..._]) => x.d
  | (StepperError(_) | StepTimeout, []) => s.elab
  };

let step_pending = (eo: EvalObj.t, {elab, previous, current, next}: t) =>
  switch (current) {
  | StepperOK(d) => {
      elab,
      previous,
      current: StepPending(d, Some(eo)),
      next,
    }
  | StepperError(_)
  | StepTimeout => {
      elab,
      previous: List.tl(previous),
      current: StepPending(List.hd(previous).d, Some(eo)),
      next,
    }
  | StepPending(d, _) => {
      elab,
      previous,
      current: StepPending(d, Some(eo)),
      next,
    }
  };

let init = (elab: DHExp.t) => {
  {
    elab,
    previous: [],
    current: StepPending(elab, None),
    next: decompose(elab),
  };
};

let update_result =
    (
      (
        d: DHExp.t,
        next_eval_objs: list(EvalObj.t),
        skipped_steps: list(step),
      ),
      s: t,
    ) => {
  previous: skipped_steps @ s.previous,
  current: StepperOK(d),
  next: next_eval_objs,
  elab: s.elab,
};

let rec evaluate_pending = (~settings, s: t) => {
  switch (s.current) {
  | StepperOK(_)
  | StepperError(_)
  | StepTimeout => s
  | StepPending(d, Some(eo)) =>
    let d' = compose(eo.ctx, eo.apply());
    {
      elab: s.elab,
      previous: [
        {d, d_loc: eo.undo, ctx: eo.ctx, knd: eo.knd},
        ...s.previous,
      ],
      current: StepPending(d', None),
      next: decompose(d'),
    }
    |> evaluate_pending(~settings);
  | StepPending(d, None) =>
    let next_step =
      List.find_opt(
        (eo: EvalObj.t) => should_hide_step(~settings, eo.knd),
        s.next,
      );
    switch (next_step) {
    | Some(eo) =>
      {
        elab: s.elab,
        previous: s.previous,
        current: StepPending(d, Some(eo)),
        next: s.next,
      }
      |> evaluate_pending(~settings)
    | None => {
        elab: s.elab,
        previous: s.previous,
        current: StepperOK(d),
        next: s.next,
      }
    };
  };
};

let timeout =
  fun
  | {elab, previous, current: StepPending(d, Some(eo)), next} => {
      elab,
      previous: [{d, d_loc: eo.undo, ctx: eo.ctx, knd: eo.knd}, ...previous],
      current: StepTimeout,
      next,
    }
  | {
      current:
        StepperError(_) | StepTimeout | StepperOK(_) | StepPending(_, None),
      _,
    } as s => s;

// let rec step_forward = (~settings, e: EvalObj.t, s: t) => {
//   let current = compose(e.ctx, e.apply());
//   skip_steps(
//     ~settings,
//     {
//       current,
//       previous: [{d: s.current, step: e}, ...s.previous],
//       next: decompose(current),
//     },
//   );
// }
// and skip_steps = (~settings, s) => {
//   switch (
//     List.find_opt(
//       (x: EvalObj.t) => should_hide_step(~settings, x.knd),
//       s.next,
//     )
//   ) {
//   | None => s
//   | Some(e) => step_forward(~settings, e, s)
//   };
// };

let rec undo_point =
        (~settings): (list(step) => option((step, list(step)))) =>
  fun
  | [] => None
  | [x, ...xs] when should_hide_step(~settings, x.knd) =>
    undo_point(~settings, xs)
  | [x, ...xs] => Some((x, xs));

let step_backward = (~settings, s: t) =>
  switch (undo_point(~settings, s.previous)) {
  | None => failwith("cannot step backwards")
  | Some((x, xs)) => {
      current: StepperOK(x.d),
      next: decompose(x.d),
      previous: xs,
      elab: s.elab,
    }
  };

let get_justification: step_kind => string =
  fun
  | LetBind => "substitution"
  | Sequence => "sequence"
  | FixUnwrap => "unroll fixpoint"
  | UpdateTest => "update test"
  | FunAp => "apply function"
  | Builtin(s) => "evaluate " ++ s
  | BinIntOp(Plus | Minus | Times | Power | Divide)
  | BinFloatOp(Plus | Minus | Times | Power | Divide) => "arithmetic"
  | BinIntOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual)
  | BinFloatOp(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual) => "comparison"
  | BinIntOp(Equals | NotEquals)
  | BinFloatOp(Equals | NotEquals)
  | BinStringOp(Equals) => "check equality"
  | BinStringOp(Concat) => "string manipulation"
  | BinBoolOp(_) => "boolean logic"
  | ListCons => "list manipulation"
  | ListConcat => "list manipulation"
  | CaseApply => "case selection"
  | CaseNext => "case discarding"
  | Projection => "projection" // TODO(Matt): We don't want to show projection to the user
  | InvalidStep => "error"
  | VarLookup => "variable lookup"
  | CastAp
  | Cast => "cast calculus"
  | CompleteFilter => "unidentified step"
  | CompleteClosure => "unidentified step"
  | FunClosure => "unidentified step"
  | Skip => "skipped steps";

let get_history = (~settings, stepper) => {
  let rec get_history':
    list(step) => (list(step), list(step_with_previous)) =
    fun
    | [] => ([], [])
    | [step, ...steps] => {
        let (hidden, ss) = get_history'(steps);
        if (should_hide_step(~settings, step.knd)) {
          ([step, ...hidden], ss);
        } else {
          (
            [],
            [
              {
                step,
                previous:
                  Option.map(
                    (x: step_with_previous) => x.step,
                    List.nth_opt(ss, 0),
                  ),
                hidden,
              },
              ...ss,
            ],
          );
        };
      };
  stepper.previous |> get_history';
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = {
  elab: DHExp.t,
  previous: list(step),
  current,
};

// Remove EvalObj.t objects from stepper to prevent problems when loading
let to_persistent: t => persistent =
  fun
  | {elab, previous, current: StepPending(d, Some(_)), _} => {
      elab,
      previous,
      current: StepPending(d, None),
    }
  | {elab, previous, current, _} => {elab, previous, current};

let from_persistent: persistent => t =
  ({elab, previous, current}) => {
    let s = {elab, previous, current, next: []};
    {elab, previous, current, next: decompose(current_expr(s))};
  };
