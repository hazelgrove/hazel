open Util;
open Language;

/* Shared shape-detection for the MVU/HTML runtime (HazelDOM, CmdRunner,
   SubManager, HTMLProj, AppViewPanel, tests): evaluator-wrapper stripping,
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

let detect_app_kind = (exp: DHExp.t): option(app_kind) => {
  switch (exp.term) {
  | Tuple([init_model, update_fn, view_fn, subs_fn])
  | Parens({term: Tuple([init_model, update_fn, view_fn, subs_fn]), _})
      when is_function(update_fn) && is_function(view_fn) =>
    Some(ElmApp(init_model, update_fn, view_fn, subs_fn))
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

// === Checkpoints ===
//
// An app model can be persisted in the projector's (serialized) model only
// if it is a plain value: functions and closures carry environments that
// don't survive a round trip, so a model holding one is simply not
// checkpointed.

let is_closure_free = (d: DHExp.t): bool => {
  let ok = ref(true);
  let f_exp = (continue, e: DHExp.t) => {
    switch (e.term) {
    | Fun(_)
    | TypFun(_)
    | FixF(_)
    | Closure(_)
    | BuiltinFun(_) => ok := false
    | _ => ()
    };
    continue(e);
  };
  let _ = Exp.map_term(~f_exp, d);
  ok^;
};

let serialize_model = (d: DHExp.t): option(string) =>
  is_closure_free(d)
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
