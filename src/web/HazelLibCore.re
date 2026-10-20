/* Hazel as a JavaScript library (a draft), its working part (HazelLib.re
   puts it on the page as window.Hazel): analyze and run a Hazel program
   from any page, its values as plain JS values, and a function in the
   result as a JS function, so a page can analyze a program once and call
   it many times.

   First use: Messed Up Dress Up's Scripting panel, whose rules for a room
   are Hazel functions from a game event to the commands to run. Built as
   www/hazel-lib.js, beside the app's hazel.js, so every branch build serves
   it: <script src=".../hazel-lib.js"> defines window.Hazel.

     Hazel.analyze(text)   -> { errors: [string] }
     Hazel.run(text)       -> { errors: [string], value }
     Hazel.version         -> string

   A program's errors do not stop it running (Hazel evaluates around its
   holes and marks), so `value` is there either way; a page decides what
   errors it accepts.

   Values, both ways: Int, Float and the other numbers <-> number; Bool <->
   boolean; String <-> string; a list <-> an array; () <-> null; a labeled
   tuple <-> an object, its labels the keys; another tuple -> an array; a
   constructor <-> {tag: "Name"}, applied <-> {tag: "Name", value}; a
   function -> a JS function, whose arguments (one, or a tuple of several)
   go back to Hazel. Anything else comes back as {hazel: <its printing>}. */
open Js_of_ocaml;
open Util;
open Haz3lcore;
open Language;

let version = "0.1 (draft, js-library)";

let ctx = () => Builtins.ctx_init(Some(Operators.default_mode));

/* The program's term, or None if it does not parse */
let term_of = (text: string): option(Exp.t) =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      text,
    )
  ) {
  | None => None
  | Some(seg) => Some(MakeTerm.go(seg).term)
  };

let analyze_term = (term: Exp.t): (list(string), Exp.t) => {
  let (map, elab) = Statics.mk(CoreSettings.on, ctx(), term);
  (ErrorPrint.all(map), elab);
};

let evaluate = (e: Exp.t): Exp.t =>
  fst(Evaluator.evaluate(~env=Builtins.env_init, e));

let js_string = (s: string): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.string(s));

let js_array = (xs: list(Js.Unsafe.any)): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.array(Array.of_list(xs)));

let js_object = (fields: list((string, Js.Unsafe.any))): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.Unsafe.obj(Array.of_list(fields)));

let number_of_bigint = (b: Bigint.t): Js.Unsafe.any =>
  Js.Unsafe.inject(
    Js.number_of_float(float_of_string(Bigint.to_string(b))),
  );

/* A Hazel value as a JS value. */
let rec to_js = (v: Exp.t): Js.Unsafe.any =>
  switch (v.term) {
  | Atom(Int(b))
  | Atom(Nat(b)) => number_of_bigint(b)
  | Atom(SInt(i)) => Js.Unsafe.inject(Js.number_of_float(float_of_int(i)))
  | Atom(Float(f)) => Js.Unsafe.inject(Js.number_of_float(f))
  | Atom(Bool(b)) => Js.Unsafe.inject(Js.bool(b))
  | Atom(String(s)) => js_string(s)
  | ListLit(xs) => js_array(List.map(to_js, xs))
  | Tuple([]) => Js.Unsafe.inject(Js.null)
  | Tuple(xs) =>
    let labeled =
      List.filter_map(
        (x: Exp.t) =>
          switch (x.term) {
          | TupLabel({term: Label(l), _}, v) => Some((l, to_js(v)))
          | _ => None
          },
        xs,
      );
    List.length(labeled) == List.length(xs)
      ? js_object(labeled) : js_array(List.map(to_js, xs));
  /* a labeled tuple of one field */
  | TupLabel({term: Label(l), _}, x) => js_object([(l, to_js(x))])
  | Constructor(name, _) => js_object([("tag", js_string(name))])
  | Ap(_, {term: Constructor(name, _), _}, arg) =>
    js_object([("tag", js_string(name)), ("value", to_js(arg))])
  | Parens(e) => to_js(e)
  | Fun(_)
  | FixF(_)
  | Closure(_)
  | BuiltinFun(_) => function_of(v)
  | _ => js_object([("hazel", js_string(DHExp.show(v)))])
  }
/* A Hazel function as a JS function: its arguments, one or a tuple of
   several, go to Hazel, and its result comes back as a JS value. */
and function_of = (f: Exp.t): Js.Unsafe.any =>
  Js.Unsafe.inject(
    Js.Unsafe.callback_with_arguments(args => {
      let args =
        Array.to_list(Js.to_array(Js.Unsafe.coerce(args)))
        |> List.map(of_js);
      let arg =
        switch (args) {
        | [] => Exp.fresh(Tuple([]))
        | [a] => a
        | many => Exp.fresh(Tuple(many))
        };
      to_js(evaluate(Exp.fresh(Ap(Operators.Forward, f, arg))));
    }),
  )
/* A JS value as a Hazel value. */
and of_js = (j: Js.Unsafe.any): Exp.t => {
  let kind = Js.to_string(Js.typeof(Js.Unsafe.coerce(j)));
  switch (kind) {
  | "number" =>
    let f: float = Js.float_of_number(Js.Unsafe.coerce(j));
    Float.is_integer(f) && Float.abs(f) < 1e15
      ? Exp.fresh(Atom(Int(Bigint.of_int(int_of_float(f)))))
      : Exp.fresh(Atom(Float(f)));
  | "boolean" => Exp.fresh(Atom(Bool(Js.to_bool(Js.Unsafe.coerce(j)))))
  | "string" => Exp.fresh(Atom(String(Js.to_string(Js.Unsafe.coerce(j)))))
  | "undefined" => Exp.fresh(Tuple([]))
  | "object" when Js.Unsafe.equals(j, Js.null) => Exp.fresh(Tuple([]))
  | _
      when
        Js.to_bool(
          Js.Unsafe.fun_call(Js.Unsafe.js_expr("Array.isArray"), [|j|]),
        ) =>
    Exp.fresh(
      ListLit(
        Array.to_list(Js.to_array(Js.Unsafe.coerce(j))) |> List.map(of_js),
      ),
    )
  | _ =>
    let tag = Js.Unsafe.get(j, "tag");
    if (Js.to_string(Js.typeof(Js.Unsafe.coerce(tag))) == "string") {
      let c =
        Exp.fresh(Constructor(Js.to_string(Js.Unsafe.coerce(tag)), None));
      let value = Js.Unsafe.get(j, "value");
      Js.to_string(Js.typeof(Js.Unsafe.coerce(value))) == "undefined"
        ? c : Exp.fresh(Ap(Operators.Forward, c, of_js(value)));
    } else {
      let keys =
        Array.to_list(Js.to_array(Js.object_keys(Js.Unsafe.coerce(j))))
        |> List.map(Js.to_string);
      Exp.fresh(
        Tuple(
          List.map(
            k =>
              Exp.fresh(
                TupLabel(Exp.fresh(Label(k)), of_js(Js.Unsafe.get(j, k))),
              ),
            keys,
          ),
        ),
      );
    };
  };
};

let errors_js = (errs: list(string)): Js.Unsafe.any =>
  js_array(List.map(js_string, errs));

let analyze = (text: string): Js.Unsafe.any =>
  switch (term_of(text)) {
  | None =>
    js_object([("errors", errors_js(["the program does not parse"]))])
  | Some(term) =>
    let (errs, _) = analyze_term(term);
    js_object([("errors", errors_js(errs))]);
  };

let run = (text: string): Js.Unsafe.any =>
  switch (term_of(text)) {
  | None =>
    js_object([
      ("errors", errors_js(["the program does not parse"])),
      ("value", Js.Unsafe.inject(Js.undefined)),
    ])
  | Some(term) =>
    let (errs, elab) = analyze_term(term);
    js_object([
      ("errors", errors_js(errs)),
      ("value", to_js(evaluate(elab))),
    ]);
  };
