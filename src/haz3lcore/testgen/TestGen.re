open Util;
open Language;

/* Test input generation orchestration (web-free, solver-agnostic).
 *
 * Given the statics of a boolean expression, TestGen:
 *   1. collects the expression's free variables and their base types from
 *      the co-context (these are the "inputs" to generate),
 *   2. assembles a self-contained SMT-LIB2 script asserting the predicate,
 *   3. parses the textual model a solver returns into assignments.
 *
 * Steps 1-2 are the "predicate-local" semantics: the boolean expression is
 * itself the constraint, and a satisfying assignment of its free variables
 * is a test input that makes it true. (Reach-point symbolic execution over
 * an enclosing program is future work; see the plan.)
 *
 * The solver itself is a swappable backend that consumes the script string
 * and produces solver output text: natively the system `z3` binary, in the
 * browser/node the `z3-solver` WASM package. Both feed their output back
 * through parse_model so model parsing is shared and unit-testable. */

[@deriving (show({with_path: false}), sexp, yojson)]
type assignment = {
  name: string,
  value: string,
};

/* The result of a solve. Persisted in the projector model, hence the
 * derivers. */
[@deriving (show({with_path: false}), sexp, yojson)]
type outcome =
  | Sat(list(assignment))
  | Unsat
  | Unknown
  | Error(string);

/* ===================== free variables & sorts ===================== */

let sort_of_cls = (cls: Atom.cls): string =>
  switch (cls) {
  | Int
  | SInt
  | Nat => "Int"
  | Float => "Real"
  | Bool => "Bool"
  | String => "String"
  };

/* Resolve a type to its base class (Int/Float/Bool/String/...), looking
 * through aliases/parens. None for arrows, products, unknowns, etc. */
let cls_of_typ = (~ctx: Ctx.t, ty: Typ.t): option(Atom.cls) =>
  Typ.is_ana_atom(Typ.weak_head_normalize(ctx, ty));

type free_var = {
  name: string,
  cls: Atom.cls,
};

/* Free variables of the expression are its inputs. The co-context maps each
 * locally-free variable to its use sites, each carrying an expected type; we
 * take the first use that resolves to a base type. Returns the resolved
 * inputs plus the names of any whose type we couldn't reduce to a base type
 * (which we can't generate, and so report). */
let free_vars = (info: Statics.Info.exp): (list(free_var), list(string)) =>
  info.co_ctx
  |> VarMap.to_list
  |> List.fold_left(
       ((resolved, unresolved), (name, entries)) => {
         let cls =
           List.find_map(
             (e: CoCtx.entry) => cls_of_typ(~ctx=info.ctx, e.expected_ty),
             entries,
           );
         switch (cls) {
         | Some(cls) => (
             resolved
             @ [
               {
                 name,
                 cls,
               },
             ],
             unresolved,
           )
         | None => (resolved, unresolved @ [name])
         };
       },
       ([], []),
     );

/* Whether test input generation applies to an expression: it must be a
 * boolean (the predicate to satisfy). Used to gate the context-menu entry. */
let applicable = (info: Statics.Info.exp): bool =>
  cls_of_typ(~ctx=info.ctx, info.ty) == Some(Bool);

/* ===================== SMT-LIB2 assembly ===================== */

let declare = (fv: free_var): string =>
  Printf.sprintf("(declare-const %s %s)", fv.name, sort_of_cls(fv.cls));

/* Nat is encoded as a non-negative Int. */
let extra_constraint = (fv: free_var): option(string) =>
  switch (fv.cls) {
  | Nat => Some(Printf.sprintf("(assert (>= %s 0))", fv.name))
  | _ => None
  };

/* Build a complete SMT-LIB2 script that is satisfiable exactly when the
 * predicate can be made true, with a model assigning the inputs. Returns an
 * error message instead when the expression isn't a generatable predicate. */
let build = (info: Statics.Info.exp): result(string, string) =>
  switch (cls_of_typ(~ctx=info.ctx, info.ty)) {
  | Some(Bool) =>
    let (resolved, unresolved) = free_vars(info);
    if (unresolved != []) {
      Error(
        "Cannot generate inputs: input(s) without a base (Int/Float/Bool/String) type: "
        ++ String.concat(", ", unresolved),
      );
    } else {
      switch (ConstraintGen.smt_of_exp(info.user_term)) {
      | exception (ConstraintGen.Unsupported(msg)) =>
        Error("Unsupported construct: " ++ msg)
      | predicate =>
        let lines =
          ["(set-logic ALL)", "(set-option :produce-models true)"]
          @ List.map(declare, resolved)
          @ List.filter_map(extra_constraint, resolved)
          @ ["(assert " ++ predicate ++ ")", "(check-sat)", "(get-model)"];
        Ok(String.concat("\n", lines));
      };
    };
  | _ => Error("Test input generation needs a boolean expression")
  };

/* ===================== model parsing ===================== */

/* Render a model value s-expression for display. z3 prints negatives as
 * `(- 3)`; flatten that common case to `-3`, otherwise fall back to the
 * raw s-expression text. */
let rec render_value = (sexp: Sexplib.Sexp.t): string =>
  switch (sexp) {
  | Atom(s) => s
  | List([Atom("-"), v]) => "-" ++ render_value(v)
  | other => Sexplib.Sexp.to_string(other)
  };

let assignments_of_model = (model: Sexplib.Sexp.t): list(assignment) => {
  let entries =
    switch (model) {
    /* z3 may or may not wrap definitions in `(model ...)`. */
    | List([Atom("model"), ...defs]) => defs
    | List(defs) => defs
    | Atom(_) => []
    };
  List.filter_map(
    (def: Sexplib.Sexp.t) =>
      switch (def) {
      | List([Atom("define-fun"), Atom(name), List([]), _sort, value]) =>
        Some({
          name,
          value: render_value(value),
        })
      | _ => None
      },
    entries,
  );
};

let is_error_line = (s: string): bool =>
  String.length(s) >= 7 && String.sub(s, 0, 7) == "(error ";

/* Parse raw solver output into an outcome. z3 prints sat/unsat/unknown on its
 * own line, optionally followed by a model. Defensively, it may also emit
 * `(error "...")` lines (e.g. from a misused/stale context); we scan past
 * those to find the status, and report them only if no status appears.
 * Shared by every backend. */
let parse_model = (output: string): outcome => {
  let lines = String.split_on_char('\n', output);
  let rec scan = (errs: list(string), lines: list(string)) =>
    switch (lines) {
    | [] => (`None, List.rev(errs), [])
    | [line, ...rest] =>
      switch (String.trim(line)) {
      | "sat" => (`Sat, List.rev(errs), rest)
      | "unsat" => (`Unsat, List.rev(errs), rest)
      | "unknown" => (`Unknown, List.rev(errs), rest)
      | t => scan(is_error_line(t) ? [t, ...errs] : errs, rest)
      }
    };
  let (status, errs, after) = scan([], lines);
  switch (status) {
  | `Unsat => Unsat
  | `Unknown => Unknown
  | `Sat =>
    let model_text = after |> String.concat("\n") |> String.trim;
    if (model_text == "") {
      Sat([]);
    } else {
      switch (Sexplib.Sexp.of_string(model_text)) {
      | exception _ => Sat([]) /* satisfiable, but model text unparseable */
      | model => Sat(assignments_of_model(model))
      };
    };
  | `None =>
    errs == []
      ? Error("unexpected solver output: " ++ String.trim(output))
      : Error(String.concat("; ", errs))
  };
};
