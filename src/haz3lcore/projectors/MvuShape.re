open Util;
open Language;

/* Shared shape-detection for the MVU/HTML runtime (HazelDOM, CmdRunner,
   SubManager, HTMLProj, tests): evaluator-wrapper stripping,
   constructor/primitive extraction, app-shape detection, and the HTML
   constructor name set (derived from BuiltinsADT.HTML.t). */

// Labeled-tuple field (name=value). KeyEvent/MouseEvent payloads are
// labeled products (BuiltinsADT.Event), so handlers can project e.key etc.
let field = (name: string, v: DHExp.t): DHExp.t =>
  IdTagged.FreshGrammar.Exp.(tup_label(label(name), v));

// Strip evaluator wrappers (Asc, Closure, Parens) from outermost level
let rec strip_wrappers = (d: DHExp.t): DHExp.t =>
  switch (d.term) {
  | Asc(inner, _)
  | Closure(_, inner)
  | Parens(inner) => strip_wrappers(inner)
  | _ => d
  };

// Extract constructor name and body, stripping wrappers from the body too.
// Nullary constructors get an empty tuple as placeholder body.
let of_constructor = (d: DHExp.t): option((string, DHExp.t)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Ap(Forward, fn, body) =>
    let fn = strip_wrappers(fn);
    switch (fn.term) {
    | Constructor(name, _) => Some((name, strip_wrappers(body)))
    | _ => None
    };
  | Constructor(name, _) =>
    Some((
      name,
      {
        ...d,
        term: Tuple([]),
      },
    ))
  | _ => None
  };
};

// Like of_constructor, but leaves the body untouched so handler bodies keep
// their Closure wrappers (the Cmd/Sub interpreters apply them later).
let rec of_constructor_raw = (d: DHExp.t): option((string, DHExp.t)) =>
  switch (d.term) {
  | Asc(inner, _)
  | Closure(_, inner)
  | Parens(inner) => of_constructor_raw(inner)
  | Ap(Forward, fn, body) =>
    switch (fn.term) {
    | Constructor(name, _) => Some((name, body))
    | Asc({term: Constructor(name, _), _}, _) => Some((name, body))
    | Closure(_, {term: Constructor(name, _), _}) => Some((name, body))
    | _ => None
    }
  | Constructor(name, _) =>
    Some((
      name,
      {
        ...d,
        term: Tuple([]),
      },
    ))
  | _ => None
  };

// === Primitive extractors (strip wrappers, then match) ===

let of_string = (d: DHExp.t): option(string) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Atom(String(s)) => Some(s)
  | _ => None
  };
};

let of_bool = (d: DHExp.t): option(bool) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Atom(Bool(b)) => Some(b)
  | _ => None
  };
};

let of_int = (d: DHExp.t): option(int) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Atom(Int(n)) => Bigint.to_int(n)
  | _ => None
  };
};

let of_float = (d: DHExp.t): option(float) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Atom(Float(f)) => Some(f)
  | _ => None
  };
};

let of_list = (d: DHExp.t): option(list(DHExp.t)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | ListLit(items) => Some(items)
  | _ => None
  };
};

let of_tuple = (d: DHExp.t): option(list(DHExp.t)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Tuple(items) => Some(items)
  | _ => None
  };
};

let of_string_list = (d: DHExp.t): option(list(string)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | ListLit(items) =>
    let strings = List.filter_map(of_string, items);
    if (List.length(strings) == List.length(items)) {
      Some(strings);
    } else {
      None;
    };
  | _ => None
  };
};

let of_pair = (d: DHExp.t): option((string, string)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Tuple([k, v]) =>
    let k = strip_wrappers(k);
    let v = strip_wrappers(v);
    switch (k.term, v.term) {
    | (Atom(String(k)), Atom(String(v))) => Some((k, v))
    | _ => None
    };
  | _ => None
  };
};

let of_string_bool_pair = (d: DHExp.t): option((string, bool)) => {
  let d = strip_wrappers(d);
  switch (d.term) {
  | Tuple([k, v]) =>
    let k = strip_wrappers(k);
    let v = strip_wrappers(v);
    switch (k.term, v.term) {
    | (Atom(String(k)), Atom(Bool(v))) => Some((k, v))
    | _ => None
    };
  | _ => None
  };
};

// === Direct evaluation ===

// Evaluate directly (skip elaboration/statics). MVU runtime values contain
// already-evaluated Closures which re-elaboration would choke on.
let evaluate = exp => fst(Evaluator.evaluate(~env=Builtins.env_init, exp));

// Error boundary: wrap evaluate to catch exceptions
let safe_evaluate = (exp: DHExp.t): result(DHExp.t, string) =>
  try(Ok(evaluate(exp))) {
  | exn => Error(Printexc.to_string(exn))
  };

// === App-shape detection ===

// Check if an expression is a function (possibly wrapped in a Closure)
let is_function = (exp: DHExp.t): bool =>
  switch (exp.term) {
  | Fun(_)
  | FixF(_)
  | Closure(_, {term: Fun(_), _})
  | Closure(_, {term: FixF(_), _}) => true
  | _ => false
  };

// App shape: the Elm-style 4-tuple (init_model, update, view, subs)
type app_kind =
  | ElmApp(DHExp.t, DHExp.t, DHExp.t, DHExp.t);

// A labeled tuple element `name = value`, if it is one. The value is left
// alone: update/view are Closures, and is_function looks through those.
let of_field = (d: DHExp.t): option((string, DHExp.t)) =>
  switch (strip_wrappers(d).term) {
  | TupLabel({term: Label(name), _}, v) => Some((name, v))
  | _ => None
  };

// The app fields, when written as a labeled tuple. All four must be
// labeled (order is then irrelevant); a partially labeled tuple falls
// through to the positional reading.
let of_labeled_app = (elements: list(DHExp.t)): option(app_kind) => {
  let fields = List.filter_map(of_field, elements);
  let all_labeled = List.length(elements) == 4 && List.length(fields) == 4;
  let get = name => all_labeled ? List.assoc_opt(name, fields) : None;
  switch (get("init"), get("update"), get("view"), get("subs")) {
  | (Some(init_model), Some(update_fn), Some(view_fn), Some(subs_fn))
      when is_function(update_fn) && is_function(view_fn) =>
    Some(ElmApp(init_model, update_fn, view_fn, subs_fn))
  | _ => None
  };
};

let of_positional_app = (elements: list(DHExp.t)): option(app_kind) =>
  switch (elements) {
  | [init_model, update_fn, view_fn, subs_fn]
      when is_function(update_fn) && is_function(view_fn) =>
    Some(ElmApp(init_model, update_fn, view_fn, subs_fn))
  | _ => None
  };

let detect_app_kind = (exp: DHExp.t): option(app_kind) => {
  switch (exp.term) {
  | Tuple(elements)
  | Parens({term: Tuple(elements), _}) =>
    switch (of_labeled_app(elements)) {
    | Some(app) => Some(app)
    | None => of_positional_app(elements)
    }
  | _ => None
  };
};

let looks_like_mvu_app = (exp: DHExp.t): bool =>
  Option.is_some(detect_app_kind(exp));

// === HTML constructor names ===

// Collect variant names from a (possibly Rec-wrapped) Sum type
let variant_names = (ty: Language.Typ.t): list(string) => {
  let of_sum = variants =>
    List.filter_map(
      fun
      | ConstructorMap.Variant(name, _, _) => Some(name)
      | ConstructorMap.BadEntry(_) => None,
      variants,
    );
  switch (ty.term) {
  | Rec(_, {term: Sum(variants), _}) => of_sum(variants)
  | Sum(variants) => of_sum(variants)
  | _ => []
  };
};

// All valid HTML element constructor names, derived from the type
// definition in BuiltinsADT so the set can never drift from it.
let html_constructor_names: list(string) = variant_names(BuiltinsADT.HTML.t);

let is_html_constructor = (name: string): bool =>
  List.mem(name, html_constructor_names);

// Is this value HTML (an application of an HTML constructor, or a nullary
// one like Br)?
let is_html = (d: DHExp.t): bool =>
  switch (of_constructor(d)) {
  | Some((name, _)) => is_html_constructor(name)
  | None => false
  };

/* Is this an HTML constructor judged by the TYPE statics gave it, rather
 * than by its name? Auto-display in the evaluation output is gated on this
 * so a user ADT that happens to define its own `Div` is not drawn as HTML.
 *
 * Constructor annotations for unshadowed builtin aliases are compacted to
 * `Var("HTML")` (ConstructorStaticsHelpers.compact_builtin_recs) — a
 * shadowing user type keeps its expanded Rec, so the Var form IS the
 * discrimination. The expanded form is accepted too, for callers that
 * normalize before asking. */
let is_html_typed_ctr = (e: DHExp.t): bool => {
  let rec result_typ = (ty: Typ.t): Typ.t =>
    switch (Typ.term_of(ty)) {
    | Parens(ty)
    | Arrow(_, ty) => result_typ(ty)
    | _ => ty
    };
  switch (strip_wrappers(e).term) {
  | Constructor(name, Some(Some(ty))) when is_html_constructor(name) =>
    switch (Typ.term_of(result_typ(ty))) {
    | Var("HTML") => true
    | _ => Typ.fast_equal(result_typ(ty), BuiltinsADT.HTML.t)
    }
  | _ => false
  };
};

/* The same test for a whole term: `Div(...)` or a nullary `Br`. */
let is_html_typed = (e: DHExp.t): bool =>
  switch (strip_wrappers(e).term) {
  | Ap(_, fn, _) => is_html_typed_ctr(fn)
  | Constructor(_) => is_html_typed_ctr(e)
  | _ => false
  };

// === Checkpoints ===
//
// Rejects terms carrying a captured environment. Functions are fine:
// Evaluator.evaluate substitutes environments away (see its INVARIANT), so
// a Fun/TypFun/FixF/BuiltinFun in a value is closed syntax and sexps like
// any other term. Environments do serialize, but a closure over the
// builtins env is ~200KB of checkpoint and by the invariant should never
// reach here at all.
//
// Does NOT prove closedness — only `evaluate` guarantees that. A model
// arriving some other way with an open term would restore to unbound
// variables, and restore_model's render check is not an airtight backstop
// (a view can embed a function without applying it).

let is_checkpointable = (d: DHExp.t): bool => {
  let ok = ref(true);
  let f_exp = (continue, e: DHExp.t) => {
    switch (e.term) {
    | Closure(_)
    | FixF(_, _, Some(_)) => ok := false
    | _ => ()
    };
    continue(e);
  };
  let _ = Exp.map_term(~f_exp, d);
  ok^;
};

let serialize_model = (d: DHExp.t): option(string) =>
  is_checkpointable(d)
    ? Some(d |> DHExp.sexp_of_t |> Sexplib.Sexp.to_string) : None;

let deserialize_model = (s: string): option(DHExp.t) =>
  try(Some(s |> Sexplib.Sexp.of_string |> DHExp.t_of_sexp)) {
  | _ => None
  };

// Restore a checkpointed model for an app whose current view is `view_fn`.
// The checkpoint is used only if it still deserializes AND the current view
// renders it as HTML; a checkpoint left over from an incompatible earlier
// version of the program is discarded rather than breaking the app. (The
// html check matters because Hazel is gradual: applying a view to a model
// of the wrong shape usually yields an indeterminate value, not an error.)
// Returns the model paired with its rendered html.
let restore_model =
    (~view_fn: DHExp.t, checkpoint: string): option((DHExp.t, DHExp.t)) =>
  switch (deserialize_model(checkpoint)) {
  | None => None
  | Some(model) =>
    switch (
      safe_evaluate(IdTagged.FreshGrammar.Exp.ap(Forward, view_fn, model))
    ) {
    | Ok(html) when is_html(html) => Some((model, html))
    | Ok(_)
    | Error(_) => None
    }
  };
