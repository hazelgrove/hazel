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
     Hazel.build           -> the commit it was built from ("local" off
                              GitHub), as embed mode's ready message has it

   A program of named modules (ModuleProgram), each module's body its own
   text, as a page with an editor per module holds it:

     Hazel.analyzeProgram(spec) -> { ok, errors, holes, programHoles,
                                     incomplete, programIncomplete, ms }
     Hazel.runProgram(spec)     -> the same, and value

     spec = { preamble: "type Cmd = ... in",  (definitions every module
                                                sees, ending in `in`)
              modules: [{ name: "Common", text: "let flip2 = ..." }, ...],
              tail: "(trap_room = TrapRoom.on_event, rooms = 1)" }

   Each module sees the preamble and the modules before it, and the tail
   sees them all. errors: [{ module, message, id }], module the module's
   name, or null for the preamble and the tail (but an error on a use of
   a module's member there, TrapRoom.on_event, is that module's). holes:
   { Common: 0, TrapRoom: 1 }, each module's holes; programHoles, the
   preamble's and the tail's. incomplete and programIncomplete: the same
   for brackets and keywords left open, or closed with no opener. ok: none
   of these. ms: the time it took. Only the parts whose text changed
   since the last call are read again.

   A program's errors do not stop it running (Hazel evaluates around its
   holes and marks), so `value` is there either way; a page decides what
   errors it accepts.

   Values, both ways: Int, Float and the other numbers <-> number; Bool <->
   boolean; String <-> string; a list <-> an array; () <-> null; a labeled
   tuple <-> an object, its labels the keys; another tuple <-> an array; a
   constructor <-> {tag: "Name"}, applied <-> {tag: "Name", value}; a
   function -> a JS function, whose arguments (one, or a tuple of several)
   go back to Hazel. Anything else comes back as {hazel: <its printing>}.
   A JS value going into a function is read by the type the function
   declares for its parameter, where it has one: an array is a tuple where
   a tuple is expected and a list where a list is, a constructor's value by
   its own type in the sum, a number as the number type expected (of_js_at).
   With no type to go by, an array is a list and a whole number an Int. */
open Js_of_ocaml;
open Util;
open Haz3lcore;
open Language;

let version = "0.1 (draft, js-library)";

let build = BuildStamp.commit;

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

let js_is_array = (j: Js.Unsafe.any): bool =>
  Js.to_bool(
    Js.Unsafe.fun_call(Js.Unsafe.js_expr("Array.isArray"), [|j|]),
  );

let js_elements = (j: Js.Unsafe.any): list(Js.Unsafe.any) =>
  Array.to_list(Js.to_array(Js.Unsafe.coerce(j)));

let js_kind = (j: Js.Unsafe.any): string =>
  Js.to_string(Js.typeof(Js.Unsafe.coerce(j)));

/* (^ticket(736)) The type a function declares for its parameter, as its
   value has it after elaboration (a type's name already its definition):
   fun s : S -> ..., fun (m, t) : (Int, Bool) -> ..., or none. */
let rec pat_type = (p: Pat.t): option(Typ.t) =>
  switch (p.term) {
  | Asc(_, t) => Some(t)
  | Parens(p) => pat_type(p)
  | _ => None
  };

let rec param_type = (f: Exp.t): option(Typ.t) =>
  switch (f.term) {
  | Fun(p, _, t, _) =>
    switch (pat_type(p), t) {
    | (Some(t), _) => Some(t)
    | (None, Some(t)) => arrow_domain(t)
    | (None, None) => None
    }
  | Closure(_, e)
  | Parens(e) => param_type(e)
  /* (let f : A -> B = fun x -> ...: the type is the binding's, not the
     parameter's) */
  | FixF(p, e, _) =>
    switch (param_type(e)) {
    | Some(t) => Some(t)
    | None => Option.bind(pat_type(p), arrow_domain)
    }
  | _ => None
  }
and arrow_domain = (t: Typ.t): option(Typ.t) =>
  switch (Typ.term_of(t)) {
  | Parens(t) => arrow_domain(t)
  | Arrow(a, _) => Some(a)
  | _ => None
  };

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
and function_of = (f: Exp.t): Js.Unsafe.any => {
  let param = param_type(f);
  Js.Unsafe.inject(
    Js.Unsafe.callback_with_arguments(args =>
      to_js(
        evaluate(
          Exp.fresh(Ap(Operators.Forward, f, arg_of_js(param, args))),
        ),
      )
    ),
  );
}
/* (^ticket(736)) A JS call's arguments as one Hazel argument, read by the
   type the function declares, if any: one argument by the whole type,
   several by the tuple's components */
and arg_of_js = (param: option(Typ.t), args: Js.Unsafe.any_js_array): Exp.t => {
  let args = Array.to_list(Js.to_array(args));
  switch (args, param) {
  | ([], _) => Exp.fresh(Tuple([]))
  | ([a], Some(t)) => of_js_at(~recs=[], t, a)
  | ([a], None) => of_js(a)
  | (many, Some(t)) =>
    switch (prod_components(t)) {
    | Some(ts) when List.length(ts) == List.length(many) =>
      Exp.fresh(
        Tuple(List.map2((t, x) => of_js_at(~recs=[], t, x), ts, many)),
      )
    | _ => Exp.fresh(Tuple(List.map(of_js, many)))
    }
  | (many, None) => Exp.fresh(Tuple(List.map(of_js, many)))
  };
}
/* a product type's components, under its parentheses */
and prod_components = (t: Typ.t): option(list(Typ.t)) =>
  switch (Typ.term_of(t)) {
  | Parens(t) => prod_components(t)
  | Prod(ts) => Some(ts)
  | _ => None
  }
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
      /* A constructor carries its type once elaborated, and a JS one has
         none to give, so it gets the unknown type: ?, or ? -> ? when
         applied. Not None, the unelaborated form: an ascription to a sum
         type (a parameter `e : Event`) can only be pushed through a
         constructor with a type to check against the sum, so around an
         untyped one it stays, and a case on the value is stuck. Applied
         (? -> ?), the ascription checks its tag and its payload against
         the sum. With no value (?), it is only consistent with the sum,
         so a tag the sum does not have is not caught there: a case then
         finds no rule for it (or a wildcard's). */
      let name = Js.to_string(Js.Unsafe.coerce(tag));
      let value = Js.Unsafe.get(j, "value");
      js_kind(value) == "undefined"
        ? constructor_of(name, None)
        : constructor_of(name, Some(of_js(value)));
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
}

/* (^ticket(736)) A JS value as a Hazel value of type ty. A JS array cannot
   say whether it is a tuple or a list ([3, 4] is either), and a list met
   with a product type is stuck: {tag: "Two", value: [3, true]} never
   matched Two(m, t). So the type decides, all the way down: an array is a
   tuple for a product of its length, a list for a list type; an object a
   labeled tuple in the type's order, by its labels; a constructor's value
   by its own type in the sum; a number as the number type asks (a whole
   number for a Float is a Float). Where the type gives nothing to go by
   (?, a type variable, a recursive type's own name), of_js as before. */
and of_js_at =
    (~recs: list((string, Typ.t)), ty: Typ.t, j: Js.Unsafe.any): Exp.t => {
  /* (a recursive type's own name, Cmd in Flip(Int, Cmd), is read as the
     whole recursive type again: recs, the names bound around here) */
  let at = (t: Typ.t, x: Js.Unsafe.any): Exp.t => of_js_at(~recs, t, x);
  let kind = js_kind(j);
  let number = () => Js.float_of_number(Js.Unsafe.coerce(j));
  let whole = () => kind == "number" && Float.is_integer(number());
  switch (Typ.term_of(ty)) {
  | Parens(t) => at(t, j)
  | Rec(tp, t) =>
    switch (tp.term) {
    | Var(name) => of_js_at(~recs=[(name, ty), ...recs], t, j)
    | _ => at(t, j)
    }
  | Var(name) when List.mem_assoc(name, recs) =>
    at(List.assoc(name, recs), j)
  | Atom(Float) when kind == "number" => Exp.fresh(Atom(Float(number())))
  | Atom(Int) when whole() =>
    Exp.fresh(
      Atom(Int(Bigint.of_string(Printf.sprintf("%.0f", number())))),
    )
  | Atom(Nat) when whole() && number() >= 0. =>
    Exp.fresh(
      Atom(Nat(Bigint.of_string(Printf.sprintf("%.0f", number())))),
    )
  | Atom(SInt) when whole() =>
    Exp.fresh(Atom(SInt(int_of_float(number()))))
  | List(t) when js_is_array(j) =>
    Exp.fresh(ListLit(List.map(x => at(t, x), js_elements(j))))
  | Prod(ts)
      when js_is_array(j) && List.length(js_elements(j)) == List.length(ts) =>
    Exp.fresh(Tuple(List.map2(at, ts, js_elements(j))))
  | Prod(ts) when kind == "object" && !Js.Unsafe.equals(j, Js.null) =>
    /* a labeled tuple, every field there: in the type's order */
    let fields =
      List.map(
        (t: Typ.t) =>
          switch (Typ.term_of(t)) {
          | TupLabel(l, t) =>
            switch (Typ.term_of(l)) {
            | Label(name)
                when js_kind(Js.Unsafe.get(j, name)) != "undefined" =>
              Some(
                Exp.fresh(
                  TupLabel(
                    Exp.fresh(Label(name)),
                    at(t, Js.Unsafe.get(j, name)),
                  ),
                ),
              )
            | _ => None
            }
          | _ => None
          },
        ts,
      );
    List.for_all(Option.is_some, fields)
      ? Exp.fresh(Tuple(List.filter_map(x => x, fields))) : of_js(j);
  | Sum(variants)
      when
        kind == "object"
        && !Js.Unsafe.equals(j, Js.null)
        && js_kind(Js.Unsafe.get(j, "tag")) == "string" =>
    let name = Js.to_string(Js.Unsafe.coerce(Js.Unsafe.get(j, "tag")));
    let value = Js.Unsafe.get(j, "value");
    let arg =
      List.find_map(
        v =>
          switch (v) {
          | ConstructorMap.Variant(c, _, Some(t)) when c == name => Some(t)
          | _ => None
          },
        variants,
      );
    switch (arg) {
    | Some(t) when js_kind(value) != "undefined" =>
      constructor_of(name, Some(at(t, value)))
    | _ => of_js(j)
    };
  | _ => of_js(j)
  };
}
/* A constructor from JS, applied to its value or not. A constructor carries
   its type once elaborated, and a JS one has none to give, so it gets the
   unknown type: ?, or ? -> ? when applied (see of_js). */
and constructor_of = (name: string, value: option(Exp.t)): Exp.t => {
  let unknown = Typ.temp(Unknown(Internal));
  switch (value) {
  | None => Exp.fresh(Constructor(name, Some(Some(unknown))))
  | Some(v) =>
    Exp.fresh(
      Ap(
        Operators.Forward,
        Exp.fresh(
          Constructor(
            name,
            Some(Some(Typ.temp(Arrow(unknown, unknown)))),
          ),
        ),
        v,
      ),
    )
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

/* A program of named modules, from a page's spec */
type program_spec = {
  preamble: string,
  modules: list((string, string)),
  tail: string,
};

let js_is_object = (j: Js.Unsafe.any): bool =>
  Js.to_string(Js.typeof(Js.Unsafe.coerce(j))) == "object"
  && !Js.Unsafe.equals(j, Js.null);

/* j[key] when j is an object and that is a string, else default */
let js_field_string = (j: Js.Unsafe.any, key: string, ~default: string) =>
  if (js_is_object(j)) {
    let v = Js.Unsafe.get(j, key);
    Js.to_string(Js.typeof(Js.Unsafe.coerce(v))) == "string"
      ? Js.to_string(Js.Unsafe.coerce(v)) : default;
  } else {
    default;
  };

/* What is missing is empty: no preamble, no modules, and () for the
   tail */
let program_spec_of_js = (j: Js.Unsafe.any): program_spec => {
  let modules =
    js_is_object(j)
      ? Js.Unsafe.get(j, "modules") : Js.Unsafe.inject(Js.undefined);
  let modules =
    Js.to_bool(
      Js.Unsafe.fun_call(Js.Unsafe.js_expr("Array.isArray"), [|modules|]),
    )
      ? Array.to_list(Js.to_array(Js.Unsafe.coerce(modules)))
        |> List.map(m =>
             (
               js_field_string(m, "name", ~default=""),
               js_field_string(m, "text", ~default=""),
             )
           )
      : [];
  {
    preamble: js_field_string(j, "preamble", ~default=""),
    modules,
    tail: js_field_string(j, "tail", ~default="()"),
  };
};

type program_analysis = {
  errors: list(ModuleProgram.error),
  holes: list((string, int)),
  program_holes: int,
  incomplete: list((string, int)),
  program_incomplete: int,
  elab: Exp.t,
};

/* The parts the last program was read from, so a page that calls again
   as one module is edited has only that module read again */
let parts_read = ModuleProgram.cache();

let analyze_program_spec = (spec: program_spec): program_analysis => {
  let program =
    ModuleProgram.of_texts_cached(
      parts_read,
      ~preamble=spec.preamble,
      ~modules=spec.modules,
      ~tail=spec.tail,
    );
  let (map, elab) = Statics.mk(CoreSettings.on, ctx(), program.term);
  let (holes, program_holes) = ModuleProgram.holes(program);
  let (incomplete, program_incomplete) = ModuleProgram.incomplete(program);
  {
    errors: ModuleProgram.errors(program, map),
    holes,
    program_holes,
    incomplete,
    program_incomplete,
    elab,
  };
};

/* milliseconds, for timing. A method call (Util.JsUtil.precise_timestamp),
   which js_of_ocaml keeps where it is written. The inlined JS expression
   this was before was taken for pure and moved to where its value is used,
   after the call, so the build's `ms` was the difference of two clock
   readings in a row: 0, or a little below. */
let now = (): float => Util.JsUtil.precise_timestamp();

let counts_js = (counts: list((string, int))): Js.Unsafe.any =>
  js_object(
    List.map(
      ((name, n)) =>
        (name, Js.Unsafe.inject(Js.number_of_float(float_of_int(n)))),
      counts,
    ),
  );

let int_js = (n: int): Js.Unsafe.any =>
  Js.Unsafe.inject(Js.number_of_float(float_of_int(n)));

/* A program's errors, holes and incomplete tiles, as analyzeProgram gives
   them (and embed mode's status message): { ok, errors, holes,
   programHoles, incomplete, programIncomplete } */
let analysis_fields =
    (
      ~errors: list(ModuleProgram.error),
      ~holes: list((string, int)),
      ~program_holes: int,
      ~incomplete: list((string, int)),
      ~program_incomplete: int,
    )
    : list((string, Js.Unsafe.any)) => {
  let none = List.for_all(((_, n)) => n == 0);
  let ok =
    errors == []
    && program_holes == 0
    && none(holes)
    && program_incomplete == 0
    && none(incomplete);
  [
    ("ok", Js.Unsafe.inject(Js.bool(ok))),
    (
      "errors",
      js_array(
        List.map(
          (e: ModuleProgram.error) =>
            js_object([
              (
                "module",
                switch (e.module_) {
                | Some(name) => js_string(name)
                | None => Js.Unsafe.inject(Js.null)
                },
              ),
              ("message", js_string(e.message)),
              ("id", js_string(Id.to_string(e.id))),
            ]),
          errors,
        ),
      ),
    ),
    ("holes", counts_js(holes)),
    ("programHoles", int_js(program_holes)),
    ("incomplete", counts_js(incomplete)),
    ("programIncomplete", int_js(program_incomplete)),
  ];
};

let program_fields =
    (a: program_analysis, ~ms: float): list((string, Js.Unsafe.any)) =>
  analysis_fields(
    ~errors=a.errors,
    ~holes=a.holes,
    ~program_holes=a.program_holes,
    ~incomplete=a.incomplete,
    ~program_incomplete=a.program_incomplete,
  )
  @ [("ms", Js.Unsafe.inject(Js.number_of_float(ms)))];

let analyze_program = (spec: Js.Unsafe.any): Js.Unsafe.any => {
  let t0 = now();
  let a = analyze_program_spec(program_spec_of_js(spec));
  js_object(program_fields(a, ~ms=now() -. t0));
};

/* Run whatever the errors and holes, as run does: ok says whether to
   trust the value */
let run_program = (spec: Js.Unsafe.any): Js.Unsafe.any => {
  let t0 = now();
  let a = analyze_program_spec(program_spec_of_js(spec));
  let value = to_js(evaluate(a.elab));
  js_object(program_fields(a, ~ms=now() -. t0) @ [("value", value)]);
};

/* ---- hazel-jit: a program compiled to JavaScript (Matthew, 2026-11-23:
   "so that hazel code can be parsed and jitted from the npm package API as
   well")

     Hazel.compile(text)         -> { errors, compiled, js, runtime, reason,
                                      ms }
     Hazel.compileProgram(spec)  -> analyzeProgram's fields, and the same
     Hazel.jit(text)             -> { errors, compiled, reason, value, ms }
     Hazel.jitProgram(spec)      -> analyzeProgram's fields, and the same

   compile gives hazel-jit's JavaScript for the program: js, the body of a
   function of one argument, $R, and runtime, the source of the object to
   pass as $R. new Function("$R", js)(eval("(" + runtime + ")")) is then
   the program's value in hazel-jit's shapes (HazelJit.re says them: a list
   is null or {h, t}, a constructor {c, v}, a tuple an array with its
   labels as keys too). A page can keep the two strings in a file and skip
   Hazel altogether, as hazeljs-examples' compiled.js files do.

   jit compiles the program and runs it compiled, and gives its value in
   this library's shapes, as run does. A function the compiled code made
   stays compiled: calling it reads its arguments as a function from run
   does, by the type it declares, and runs the compiled code. A call the
   compiled code does not finish (an invalid operation, something
   indeterminate, a form it does not compile) is run again by the
   interpreter, whose answer it is.

   compiled: whether the value runs compiled; when not, reason says why
   (the first form hazel-jit does not compile: a hole, a test, ...) and
   jit's value is the interpreter's, as run gives it. */

let js_keys = (v: Js.Unsafe.any): list(string) =>
  Js.to_array(
    Js.Unsafe.meth_call(
      Js.Unsafe.get(Js.Unsafe.global, "Object"),
      "keys",
      [|v|],
    ),
  )
  |> Array.to_list
  |> List.map(k => Js.to_string(k));

let is_index = (k: string): bool =>
  k != "" && String.for_all(c => c >= '0' && c <= '9', k);

let rec arrow_codomain = (t: Typ.t): option(Typ.t) =>
  switch (Typ.term_of(t)) {
  | Parens(t) => arrow_codomain(t)
  | Arrow(_, b) => Some(b)
  | _ => None
  };

/* the type a function declares for its result, if any */
let result_type = (f: Exp.t): option(Typ.t) =>
  switch (f.term) {
  | Fun(_, _, Some(t), _) => arrow_codomain(t)
  | _ => None
  };

let compile_elab = (elab: Exp.t): result(HazelJit.compiled, string) =>
  switch (HazelJit.compile_full(~env=Builtins.env_init, elab)) {
  | c => Ok(c)
  | exception (HazelJit.Unsupported(why)) => Error(why)
  };

/* A compiled value as this library's JS value */
let rec of_jit =
        (ts: HazelJitValue.types, ty: option(Typ.t), v: Js.Unsafe.any)
        : Js.Unsafe.any =>
  if (HazelJitValue.typeof(v) == "function" && HazelJitValue.has(v, "$k")) {
    jit_function(ts, v);
  } else if (HazelJitValue.is_array(v) && holds_function(v)) {
    jit_tuple(ts, ty, v);
  } else {
    to_js(HazelJitValue.of_js(ts, ty, v));
  }
and holds_function = (v: Js.Unsafe.any): bool => {
  let n = Js.Unsafe.get(v, "length");
  List.exists(
    i => HazelJitValue.typeof(Js.Unsafe.get(v, i)) == "function",
    List.init(n, i => i),
  );
}
/* a tuple holding a function: each component by its own type, the labels
   by the expected product type, or else by the array's keys (a key whose
   value is the very value at an index is that index's label) */
and jit_tuple =
    (ts: HazelJitValue.types, ty: option(Typ.t), v: Js.Unsafe.any)
    : Js.Unsafe.any => {
  let n: int = Js.Unsafe.get(v, "length");
  let at = i => Js.Unsafe.get(v, i);
  let fields: list((option(string), option(Typ.t))) =
    switch (
      Option.map(
        (t: Typ.t) => t.term,
        Option.bind(ty, HazelJitValue.shape(ts, 0)),
      )
    ) {
    | Some(Prod(tys)) when List.length(tys) == n =>
      List.map(
        (t: Typ.t) =>
          switch (Typ.match_tup_label(t)) {
          | Some((l, t)) => (Some(l), Some(t))
          | None => (None, Some(t))
          },
        tys,
      )
    | _ =>
      let keys = List.filter(k => !is_index(k), js_keys(v));
      let used = ref([]);
      List.init(n, i =>
        switch (
          List.find_opt(
            k =>
              !List.mem(k, used^)
              && Js.Unsafe.equals(Js.Unsafe.get(v, k), at(i)),
            keys,
          )
        ) {
        | Some(k) =>
          used := [k, ...used^];
          (Some(HazelJit.label_of_key(k)), None);
        | None => (None, None)
        }
      );
    };
  let values = List.mapi((i, (_, t)) => of_jit(ts, t, at(i)), fields);
  List.for_all(((l, _)) => l != None, fields)
    ? js_object(
        List.map2(
          ((l, _), v) => (Option.value(l, ~default=""), v),
          fields,
          values,
        ),
      )
    : js_array(values);
}
/* a function the compiled code made, as a JS function that runs it */
and jit_function = (ts: HazelJitValue.types, f: Js.Unsafe.any): Js.Unsafe.any => {
  let k: int =
    int_of_float(
      Js.float_of_number(Js.Unsafe.coerce(Js.Unsafe.get(f, "$k"))),
    );
  let term = Option.map(fst, Hashtbl.find_opt(ts.HazelJitValue.funs, k));
  let param = Option.bind(term, param_type);
  let result = Option.bind(term, result_type);
  Js.Unsafe.inject(
    Js.Unsafe.callback_with_arguments(args => {
      let arg = arg_of_js(param, args);
      switch (
        of_jit(
          ts,
          result,
          Js.Unsafe.fun_call(f, [|HazelJitValue.to_js(arg)|]),
        )
      ) {
      | v => v
      | exception _ =>
        to_js(
          evaluate(
            Exp.fresh(
              Ap(Operators.Forward, HazelJitValue.of_js(ts, None, f), arg),
            ),
          ),
        )
      };
    }),
  );
};

let compiled_fields =
    (c: result(HazelJit.compiled, string)): list((string, Js.Unsafe.any)) =>
  switch (c) {
  | Ok(c) => [
      ("compiled", Js.Unsafe.inject(Js.bool(true))),
      ("js", js_string(c.HazelJit.src)),
      ("runtime", js_string(HazelJit.Runtime.source)),
    ]
  | Error(why) => [
      ("compiled", Js.Unsafe.inject(Js.bool(false))),
      ("reason", js_string(why)),
    ]
  };

/* the value of a program compiled, or the interpreter's with why not */
let jit_value =
    (elab: Exp.t, c: result(HazelJit.compiled, string))
    : list((string, Js.Unsafe.any)) => {
  let interpreted = why => [
    ("compiled", Js.Unsafe.inject(Js.bool(false))),
    ("reason", js_string(why)),
    ("value", to_js(evaluate(elab))),
  ];
  switch (c) {
  | Error(why) => interpreted(why)
  | Ok(c) =>
    switch (
      of_jit(
        HazelJitValue.types_of(c),
        None,
        HazelJitValue.instantiate(c.HazelJit.src),
      )
    ) {
    | value => [
        ("compiled", Js.Unsafe.inject(Js.bool(true))),
        ("value", value),
      ]
    | exception e => interpreted(HazelJitValue.message(e))
    }
  };
};

let ms_field = (t0: float): (string, Js.Unsafe.any) => (
  "ms",
  Js.Unsafe.inject(Js.number_of_float(now() -. t0)),
);

let not_parsed = (~value: bool, t0: float): Js.Unsafe.any =>
  js_object(
    [
      ("errors", errors_js(["the program does not parse"])),
      ("compiled", Js.Unsafe.inject(Js.bool(false))),
      ("reason", js_string("the program does not parse")),
    ]
    @ (value ? [("value", Js.Unsafe.inject(Js.undefined))] : [])
    @ [ms_field(t0)],
  );

let compile = (text: string): Js.Unsafe.any => {
  let t0 = now();
  switch (term_of(text)) {
  | None => not_parsed(~value=false, t0)
  | Some(term) =>
    let (errs, elab) = analyze_term(term);
    js_object(
      [("errors", errors_js(errs))]
      @ compiled_fields(compile_elab(elab))
      @ [ms_field(t0)],
    );
  };
};

let jit = (text: string): Js.Unsafe.any => {
  let t0 = now();
  switch (term_of(text)) {
  | None => not_parsed(~value=true, t0)
  | Some(term) =>
    let (errs, elab) = analyze_term(term);
    js_object(
      [("errors", errors_js(errs))]
      @ jit_value(elab, compile_elab(elab))
      @ [ms_field(t0)],
    );
  };
};

let compile_program = (spec: Js.Unsafe.any): Js.Unsafe.any => {
  let t0 = now();
  let a = analyze_program_spec(program_spec_of_js(spec));
  let c = compile_elab(a.elab);
  js_object(program_fields(a, ~ms=now() -. t0) @ compiled_fields(c));
};

let jit_program = (spec: Js.Unsafe.any): Js.Unsafe.any => {
  let t0 = now();
  let a = analyze_program_spec(program_spec_of_js(spec));
  let fields = jit_value(a.elab, compile_elab(a.elab));
  js_object(program_fields(a, ~ms=now() -. t0) @ fields);
};
