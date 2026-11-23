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

/* (^report(54)) a program's type, as the statics give it: what its value is read back at, so a compiled function in it
   has its parameter's and its result's types for the converters */
let program_type = (map: Statics.Map.t, term: Exp.t): option(Typ.t) =>
  Statics.Map.lookup_exp(Exp.rep_id(term), map)
  |> Option.map(~f=Info.exp_ty);

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
    | None => Option.bind(pat_type(p), ~f=arrow_domain)
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
  | ListLit(xs) => js_array(List.map(xs, ~f=to_js))
  | Tuple([]) => Js.Unsafe.inject(Js.null)
  | Tuple(xs) =>
    let labeled =
      List.filter_map(xs, ~f=(x: Exp.t) =>
        switch (x.term) {
        | TupLabel({term: Label(l), _}, v) => Some((l, to_js(v)))
        | _ => None
        }
      );
    List.length(labeled) == List.length(xs)
      ? js_object(labeled) : js_array(List.map(xs, ~f=to_js));
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
  let args = Array.to_list(Js.to_array(Js.Unsafe.coerce(args)));
  switch (args, param) {
  | ([], _) => Exp.fresh(Tuple([]))
  | ([a], Some(t)) => of_js_at(~recs=[], t, a)
  | ([a], None) => of_js(a)
  | (many, Some(t)) =>
    switch (prod_components(t)) {
    | Some(ts) when List.length(ts) == List.length(many) =>
      Exp.fresh(
        Tuple(
          List.map2_exn(ts, many, ~f=(t, x) => of_js_at(~recs=[], t, x)),
        ),
      )
    | _ => Exp.fresh(Tuple(List.map(many, ~f=of_js)))
    }
  | (many, None) => Exp.fresh(Tuple(List.map(many, ~f=of_js)))
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
    Float.is_integer(f) && Float.(abs(f) < 1e15)
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
        Array.to_list(Js.to_array(Js.Unsafe.coerce(j)))
        |> List.map(~f=of_js),
      ),
    )
  | _ =>
    let tag = Js.Unsafe.get(j, "tag");
    if (String.equal(
          Js.to_string(Js.typeof(Js.Unsafe.coerce(tag))),
          "string",
        )) {
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
      String.equal(js_kind(value), "undefined")
        ? constructor_of(name, None)
        : constructor_of(name, Some(of_js(value)));
    } else {
      let keys =
        Array.to_list(Js.to_array(Js.object_keys(Js.Unsafe.coerce(j))))
        |> List.map(~f=Js.to_string);
      Exp.fresh(
        Tuple(
          List.map(keys, ~f=k =>
            Exp.fresh(
              TupLabel(Exp.fresh(Label(k)), of_js(Js.Unsafe.get(j, k))),
            )
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
  let whole = () =>
    String.equal(kind, "number") && Float.is_integer(number());
  switch (Typ.term_of(ty)) {
  | Parens(t) => at(t, j)
  | Rec(tp, t) =>
    switch (tp.term) {
    | Var(name) => of_js_at(~recs=[(name, ty), ...recs], t, j)
    | _ => at(t, j)
    }
  | Var(name) when List.Assoc.mem(recs, ~equal=String.equal, name) =>
    at(List.Assoc.find_exn(recs, ~equal=String.equal, name), j)
  | Atom(Float) when String.equal(kind, "number") =>
    Exp.fresh(Atom(Float(number())))
  | Atom(Int) when whole() =>
    Exp.fresh(
      Atom(Int(Bigint.of_string(Printf.sprintf("%.0f", number())))),
    )
  | Atom(Nat) when whole() && Float.(number() >= 0.) =>
    Exp.fresh(
      Atom(Nat(Bigint.of_string(Printf.sprintf("%.0f", number())))),
    )
  | Atom(SInt) when whole() =>
    Exp.fresh(Atom(SInt(int_of_float(number()))))
  | List(t) when js_is_array(j) =>
    Exp.fresh(ListLit(List.map(js_elements(j), ~f=x => at(t, x))))
  | Prod(ts)
      when js_is_array(j) && List.length(js_elements(j)) == List.length(ts) =>
    Exp.fresh(Tuple(List.map2_exn(ts, js_elements(j), ~f=at)))
  | Prod(ts)
      when String.equal(kind, "object") && !Js.Unsafe.equals(j, Js.null) =>
    /* a labeled tuple, every field there: in the type's order */
    let fields =
      List.map(ts, ~f=(t: Typ.t) =>
        switch (Typ.term_of(t)) {
        | TupLabel(l, t) =>
          switch (Typ.term_of(l)) {
          | Label(name)
              when
                !String.equal(js_kind(Js.Unsafe.get(j, name)), "undefined") =>
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
        }
      );
    List.for_all(fields, ~f=Option.is_some)
      ? Exp.fresh(Tuple(List.filter_map(fields, ~f=x => x))) : of_js(j);
  | Sum(variants)
      when
        String.equal(kind, "object")
        && !Js.Unsafe.equals(j, Js.null)
        && String.equal(js_kind(Js.Unsafe.get(j, "tag")), "string") =>
    let name = Js.to_string(Js.Unsafe.coerce(Js.Unsafe.get(j, "tag")));
    let value = Js.Unsafe.get(j, "value");
    let arg =
      List.find_map(variants, ~f=v =>
        switch (v) {
        | ConstructorMap.Variant(c, _, Some(t)) when String.equal(c, name) =>
          Some(t)
        | _ => None
        }
      );
    switch (arg) {
    | Some(t) when !String.equal(js_kind(value), "undefined") =>
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
  js_array(List.map(errs, ~f=js_string));

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
  String.equal(Js.to_string(Js.typeof(Js.Unsafe.coerce(j))), "object")
  && !Js.Unsafe.equals(j, Js.null);

/* j[key] when j is an object and that is a string, else default */
let js_field_string = (j: Js.Unsafe.any, key: string, ~default: string) =>
  if (js_is_object(j)) {
    let v = Js.Unsafe.get(j, key);
    String.equal(Js.to_string(Js.typeof(Js.Unsafe.coerce(v))), "string")
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
        |> List.map(~f=m =>
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
  /* the program's type, from the statics */
  ty: option(Typ.t),
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
    ty: program_type(map, program.term),
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
    List.map(counts, ~f=((name, n)) =>
      (name, Js.Unsafe.inject(Js.number_of_float(float_of_int(n))))
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
  let none = List.for_all(~f=((_, n)) => n == 0);
  let ok =
    List.is_empty(errors)
    && program_holes == 0
    && none(holes)
    && program_incomplete == 0
    && none(incomplete);
  [
    ("ok", Js.Unsafe.inject(Js.bool(ok))),
    (
      "errors",
      js_array(
        List.map(errors, ~f=(e: ModuleProgram.error) =>
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
          ])
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
  |> List.map(~f=k => Js.to_string(k));

let is_index = (k: string): bool =>
  !String.is_empty(k) && String.for_all(k, ~f=Char.is_digit);

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

/* (^report(54): converters by declared type) A compiled function's arguments and answer converted straight between a
   page's values and compiled code's shapes, by the function's declared types, in JavaScript made once for the
   function: no Hazel value made on the way (the old way, through Hazel's values, cost most of the jit's time on 34 of
   report 063's 45 operations, and about seventy times cells for a large list). The shapes are the old way's, exactly:
   a page's number, boolean and string are compiled code's; a list an array one way and cells the other; a labeled
   tuple an object one way and an array carrying its labels the other; an unlabeled tuple an array both ways; a
   constructor {tag, value} one way and {c, v} the other; the empty tuple null and []. Where a type gives nothing to go
   by (?, a function type, a type variable, a tuple both labeled and not), there are no converters, and every call goes
   the old way; where a value does not fit its type at run time (a number that is not whole for an Int, a tag the sum
   does not have), or the compiled code stops (an invalid operation, something indeterminate), that call goes the old
   way, which falls back to the interpreter as before. */
module Direct = {
  exception No;
  type st = {
    ts: HazelJitValue.types,
    mutable defs: list(string),
    mutable named: list((string, string)),
    mutable n: int,
  };
  let fresh = (st: st, base: string): string => {
    st.n = st.n + 1;
    base ++ string_of_int(st.n);
  };
  let q = HazelJit.js_string;
  /* a named converter, made once: recursive types and type aliases call it by name, so it is registered before its
     body is made */
  let named = (st: st, key: string, x: string, body: string => string): string => {
    switch (List.Assoc.find(st.named, ~equal=String.equal, key)) {
    | Some(f) => f ++ "(" ++ x ++ ")"
    | None =>
      let f = fresh(st, "c");
      st.named = [(key, f), ...st.named];
      let b = body("v");
      st.defs = [
        "function " ++ f ++ "(v) { return " ++ b ++ "; }",
        ...st.defs,
      ];
      f ++ "(" ++ x ++ ")";
    };
  };
  let labeled = (ts: list(Typ.t)): option(list((string, Typ.t))) => {
    let ls =
      List.map(ts, ~f=(t: Typ.t) =>
        switch (Typ.term_of(t)) {
        | TupLabel(l, t) =>
          switch (Typ.term_of(l)) {
          | Label(name) => Some((name, t))
          | _ => None
          }
        | _ => None
        }
      );
    if (List.for_all(ls, ~f=Option.is_some)) {
      Some(List.filter_opt(ls));
    } else if (List.for_all(ls, ~f=Option.is_none)) {
      None;
    } else {
      raise(No);
    };
  };
  let variants =
      (vs: ConstructorMap.t(Typ.t)): list((string, option(Typ.t))) =>
    List.map(vs, ~f=v =>
      switch (v) {
      | ConstructorMap.Variant(c, _, t) => (c, t)
      | ConstructorMap.BadEntry(_) => raise(No)
      }
    );
  /* a page's value x (a JS expression) as compiled code's, of type t; recs: the recursive types' names in scope, each
     with its converter's key */
  let rec inn =
          (st: st, recs: list((string, string)), t: Typ.t, x: string)
          : string =>
    switch (Typ.term_of(t)) {
    | Parens(t) => inn(st, recs, t, x)
    | Atom(Int) => "$I(" ++ x ++ ")"
    | Atom(Nat) => "$N(" ++ x ++ ")"
    | Atom(SInt) => "$S(" ++ x ++ ")"
    | Atom(Float) => "$F(" ++ x ++ ")"
    | Atom(Bool) => "$B(" ++ x ++ ")"
    | Atom(String) => "$T(" ++ x ++ ")"
    | List(t) =>
      let e = fresh(st, "e");
      "$Lin(" ++ x ++ ", " ++ e ++ " => " ++ inn(st, recs, t, e) ++ ")";
    | Prod([]) => "$Uin(" ++ x ++ ")"
    | Prod(ts) =>
      let v = fresh(st, "p");
      switch (labeled(ts)) {
      | Some(fields) =>
        let vals =
          List.map(fields, ~f=((l, t)) =>
            inn(st, recs, t, v ++ "[" ++ q(l) ++ "]")
          );
        let keys =
          List.mapi(fields, ~f=(i, (l, _)) =>
            "a["
            ++ q(HazelJit.label_key(l))
            ++ "] = a["
            ++ string_of_int(i)
            ++ "]; "
          );
        "("
        ++ v
        ++ " => { if ("
        ++ v
        ++ " === null || typeof "
        ++ v
        ++ " !== \"object\" || Array.isArray("
        ++ v
        ++ ")) fail(); const a = ["
        ++ String.concat(~sep=", ", vals)
        ++ "]; "
        ++ String.concat(keys)
        ++ "return a; })("
        ++ x
        ++ ")";
      | None =>
        let n = List.length(ts);
        let vals =
          List.mapi(ts, ~f=(i, t) =>
            inn(st, recs, t, v ++ "[" ++ string_of_int(i) ++ "]")
          );
        "("
        ++ v
        ++ " => Array.isArray("
        ++ v
        ++ ") && "
        ++ v
        ++ ".length === "
        ++ string_of_int(n)
        ++ " ? ["
        ++ String.concat(~sep=", ", vals)
        ++ "] : fail())("
        ++ x
        ++ ")";
      };
    | Sum(vs) =>
      let v = fresh(st, "s");
      let arms =
        List.map(variants(vs), ~f=((c, t)) =>
          "case "
          ++ q(c)
          ++ ": return "
          ++ (
            switch (t) {
            | None => "{c: " ++ q(c) ++ "}"
            | Some(t) =>
              "{c: "
              ++ q(c)
              ++ ", v: "
              ++ inn(st, recs, t, v ++ ".value")
              ++ "}"
            }
          )
          ++ "; "
        );
      "("
      ++ v
      ++ " => { if ("
      ++ v
      ++ " === null || typeof "
      ++ v
      ++ " !== \"object\") fail(); switch ("
      ++ v
      ++ ".tag) { "
      ++ String.concat(arms)
      ++ "default: fail(); } })("
      ++ x
      ++ ")";
    | Rec(tp, body) =>
      switch (tp.term) {
      | Var(name) =>
        named(
          st,
          "in rec " ++ name ++ " " ++ string_of_int(List.length(recs)),
          x,
          v =>
          inn(
            st,
            [
              (
                name,
                "in rec " ++ name ++ " " ++ string_of_int(List.length(recs)),
              ),
              ...recs,
            ],
            body,
            v,
          )
        )
      | _ => raise(No)
      }
    | Var(name) =>
      switch (List.Assoc.find(recs, ~equal=String.equal, name)) {
      | Some(key) => named(st, key, x, _ => raise(No))
      | None =>
        switch (Stdlib.Hashtbl.find_opt(st.ts.aliases, name)) {
        | Some(t) =>
          named(st, "in alias " ++ name, x, v => inn(st, recs, t, v))
        | None => raise(No)
        }
      }
    | _ => raise(No)
    };
  /* compiled code's value y as a page's, of type t */
  let rec out =
          (st: st, recs: list((string, string)), t: Typ.t, y: string)
          : string =>
    switch (Typ.term_of(t)) {
    | Parens(t) => out(st, recs, t, y)
    | Atom(Int | Nat) => "$oI(" ++ y ++ ")"
    | Atom(SInt | Float) => "$F(" ++ y ++ ")"
    | Atom(Bool) => "$B(" ++ y ++ ")"
    | Atom(String) => "$T(" ++ y ++ ")"
    | List(t) =>
      let e = fresh(st, "e");
      "$Lout(" ++ y ++ ", " ++ e ++ " => " ++ out(st, recs, t, e) ++ ")";
    | Prod([]) => "$Uout(" ++ y ++ ")"
    | Prod(ts) =>
      let v = fresh(st, "p");
      let n = string_of_int(List.length(ts));
      switch (labeled(ts)) {
      | Some(fields) =>
        let fs =
          List.mapi(fields, ~f=(i, (l, t)) =>
            q(l)
            ++ ": "
            ++ out(st, recs, t, v ++ "[" ++ string_of_int(i) ++ "]")
          );
        "("
        ++ v
        ++ " => Array.isArray("
        ++ v
        ++ ") && "
        ++ v
        ++ ".length === "
        ++ n
        ++ " ? {"
        ++ String.concat(~sep=", ", fs)
        ++ "} : fail())("
        ++ y
        ++ ")";
      | None =>
        let vals =
          List.mapi(ts, ~f=(i, t) =>
            out(st, recs, t, v ++ "[" ++ string_of_int(i) ++ "]")
          );
        "("
        ++ v
        ++ " => Array.isArray("
        ++ v
        ++ ") && "
        ++ v
        ++ ".length === "
        ++ n
        ++ " ? ["
        ++ String.concat(~sep=", ", vals)
        ++ "] : fail())("
        ++ y
        ++ ")";
      };
    | Sum(vs) =>
      let v = fresh(st, "s");
      let arms =
        List.map(variants(vs), ~f=((c, t)) =>
          "case "
          ++ q(c)
          ++ ": return "
          ++ (
            switch (t) {
            | None => "{tag: " ++ q(c) ++ "}"
            | Some(t) =>
              "{tag: "
              ++ q(c)
              ++ ", value: "
              ++ out(st, recs, t, v ++ ".v")
              ++ "}"
            }
          )
          ++ "; "
        );
      "("
      ++ v
      ++ " => { if ("
      ++ v
      ++ " === null || typeof "
      ++ v
      ++ " !== \"object\" || typeof "
      ++ v
      ++ ".c !== \"string\") fail(); switch ("
      ++ v
      ++ ".c) { "
      ++ String.concat(arms)
      ++ "default: fail(); } })("
      ++ y
      ++ ")";
    | Rec(tp, body) =>
      switch (tp.term) {
      | Var(name) =>
        named(
          st,
          "out rec " ++ name ++ " " ++ string_of_int(List.length(recs)),
          y,
          v =>
          out(
            st,
            [
              (
                name,
                "out rec " ++ name ++ " " ++ string_of_int(List.length(recs)),
              ),
              ...recs,
            ],
            body,
            v,
          )
        )
      | _ => raise(No)
      }
    | Var(name) =>
      switch (List.Assoc.find(recs, ~equal=String.equal, name)) {
      | Some(key) => named(st, key, y, _ => raise(No))
      | None =>
        switch (Stdlib.Hashtbl.find_opt(st.ts.aliases, name)) {
        | Some(t) =>
          named(st, "out alias " ++ name, y, v => out(st, recs, t, v))
        | None => raise(No)
        }
      }
    | _ => raise(No)
    };
  let helpers = {|
  const FAIL = {}; const fail = () => { throw FAIL; };
  const $I = v => Number.isSafeInteger(v) ? v : fail();
  const $N = v => Number.isSafeInteger(v) && v >= 0 ? v : fail();
  const $S = v => typeof v === "number" && (v | 0) === v ? v : fail();
  const $F = v => typeof v === "number" ? v : fail();
  const $B = v => typeof v === "boolean" ? v : fail();
  const $T = v => typeof v === "string" ? v : fail();
  const $oI = v => typeof v === "number" ? v : typeof v === "bigint" ? Number(v) : fail();
  const $Uin = v => v == null || (Array.isArray(v) && v.length === 0) ? [] : fail();
  const $Uout = v => Array.isArray(v) && v.length === 0 ? null : fail();
  const $Lin = (v, f) => { if (!Array.isArray(v)) fail(); let r = null; for (let i = v.length - 1; i >= 0; i--) r = { h: f(v[i]), t: r }; return r; };
  const $Lout = (v, f) => { const a = []; for (let p = v; p !== null; p = p.t) { if (typeof p !== "object" || !("h" in p)) fail(); a.push(f(p.h)); } return a; };
|};
  /* the factory: given the compiled function and the old way (slow), the function a page calls; None when the types
     give nothing to go by */
  let source =
      (ts: HazelJitValue.types, param: option(Typ.t), result: option(Typ.t))
      : option(string) =>
    switch (param, result) {
    | (Some(param), Some(result)) =>
      let st = {
        ts,
        defs: [],
        named: [],
        n: 0,
      };
      switch (
        {
          let one = inn(st, [], param, "args[0]");
          let many =
            switch (Typ.term_of(param)) {
            | Prod(ts)
                when List.length(ts) > 1 && Option.is_none(labeled(ts)) =>
              "args.length === "
              ++ string_of_int(List.length(ts))
              ++ " ? ["
              ++ String.concat(
                   ~sep=", ",
                   List.mapi(ts, ~f=(i, t) =>
                     inn(st, [], t, "args[" ++ string_of_int(i) ++ "]")
                   ),
                 )
              ++ "] : fail()"
            | _ => "fail()"
            };
          let back = out(st, [], result, "r");
          (one, many, back);
        }
      ) {
      | exception No => None
      | (one, many, back) =>
        Some(
          "(function (f, slow) { \"use strict\";"
          ++ helpers
          ++ String.concat(~sep="\n", List.rev(st.defs))
          ++ "\n  const IN = args => args.length === 0 ? [] : args.length === 1 ? "
          ++ one
          ++ " : "
          ++ many
          ++ ";"
          ++ "\n  const OUT = r => "
          ++ back
          ++ ";"
          ++ "\n  return function (...args) { let a, r;"
          ++ "\n    try { a = IN(args); } catch (e) { if (e === FAIL) return slow(...args); throw e; }"
          ++ "\n    try { r = f(a); } catch (e) { return slow(...args); }"
          ++ "\n    try { return OUT(r); } catch (e) { if (e === FAIL) return slow(...args); throw e; } }; })",
        )
      };
    | _ => None
    };
};

/* A compiled value as this library's JS value */
let rec of_jit =
        (ts: HazelJitValue.types, ty: option(Typ.t), v: Js.Unsafe.any)
        : Js.Unsafe.any =>
  if (String.equal(HazelJitValue.typeof(v), "function")
      && HazelJitValue.has(v, "$k")) {
    jit_function(~ty?, ts, v);
  } else if (HazelJitValue.is_array(v) && holds_function(v)) {
    jit_tuple(ts, ty, v);
  } else {
    to_js(HazelJitValue.of_js(ts, ty, v));
  }
and holds_function = (v: Js.Unsafe.any): bool => {
  let n = Js.Unsafe.get(v, "length");
  List.exists(List.init(n, ~f=i => i), ~f=i =>
    String.equal(HazelJitValue.typeof(Js.Unsafe.get(v, i)), "function")
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
        Option.bind(ty, ~f=HazelJitValue.shape(ts, 0)), ~f=(t: Typ.t) =>
        t.term
      )
    ) {
    | Some(Prod(tys)) when List.length(tys) == n =>
      List.map(tys, ~f=(t: Typ.t) =>
        switch (Typ.match_tup_label(t)) {
        | Some((l, t)) => (Some(l), Some(t))
        | None => (None, Some(t))
        }
      )
    | _ =>
      let keys = List.filter(js_keys(v), ~f=k => !is_index(k));
      let used = ref([]);
      /* in index order, since each match uses its key up (Core's List.init
         calls f from the last index down) */
      List.map(List.range(0, n), ~f=i =>
        switch (
          List.find(keys, ~f=k =>
            !List.mem(used^, k, ~equal=String.equal)
            && Js.Unsafe.equals(Js.Unsafe.get(v, k), at(i))
          )
        ) {
        | Some(k) =>
          used := [k, ...used^];
          (Some(HazelJit.label_of_key(k)), None);
        | None => (None, None)
        }
      );
    };
  let values = List.mapi(fields, ~f=(i, (_, t)) => of_jit(ts, t, at(i)));
  List.for_all(fields, ~f=((l, _)) => Option.is_some(l))
    ? js_object(
        List.map2_exn(fields, values, ~f=((l, _), v) =>
          (Option.value(l, ~default=""), v)
        ),
      )
    : js_array(values);
}
/* a function the compiled code made, as a JS function that runs it */
and jit_function =
    (~ty: option(Typ.t)=?, ts: HazelJitValue.types, f: Js.Unsafe.any)
    : Js.Unsafe.any => {
  let k: int =
    int_of_float(
      Js.float_of_number(Js.Unsafe.coerce(Js.Unsafe.get(f, "$k"))),
    );
  let term =
    Option.map(Stdlib.Hashtbl.find_opt(ts.HazelJitValue.funs, k), ~f=fst);
  /* (^report(54)) the types it is read at, an arrow (seen through aliases), else its own term's: dev's elaborated Fun
     keeps only its parameter's type, and a recursive function's declared type is its binding's, not its term's */
  let declared =
    switch (
      Option.bind(ty, ~f=HazelJitValue.shape(ts, 0))
      |> Option.map(~f=(t: Typ.t) => Typ.term_of(t))
    ) {
    | Some(Arrow(a, b)) => Some((a, b))
    | _ => None
    };
  let param =
    switch (declared) {
    | Some((a, _)) => Some(a)
    | None => Option.bind(term, ~f=param_type)
    };
  let result =
    switch (declared) {
    | Some((_, b)) => Some(b)
    | None => Option.bind(term, ~f=result_type)
    };
  /* the old way: through Hazel's values, and to the interpreter if the compiled code stops */
  let slow =
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
      | exception stopped =>
        switch (HazelJitValue.of_js(ts, None, f)) {
        | g => to_js(evaluate(Exp.fresh(Ap(Operators.Forward, g, arg))))
        | exception not_made =>
          failwith(
            "hazel-jit: the compiled call stopped ("
            ++ HazelJitValue.message(stopped)
            ++ "), and the interpreter could not be given its function ("
            ++ HazelJitValue.message(not_made)
            ++ ")",
          )
        }
      };
    });
  /* (^report(54)) the converters by type, where the types say enough; the old way for any call they cannot take */
  switch (Direct.source(ts, param, result)) {
  | Some(src) =>
    Js.Unsafe.fun_call(
      Js.Unsafe.eval_string(src),
      [|f, Js.Unsafe.inject(slow)|],
    )
  | None => Js.Unsafe.inject(slow)
  };
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
    (
      ~ty: option(Typ.t)=?,
      elab: Exp.t,
      c: result(HazelJit.compiled, string),
    )
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
        ty,
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
    let (map, elab) = Statics.mk(CoreSettings.on, ctx(), term);
    js_object(
      [("errors", errors_js(ErrorPrint.all(map)))]
      @ jit_value(~ty=?program_type(map, term), elab, compile_elab(elab))
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
  let fields = jit_value(~ty=?a.ty, a.elab, compile_elab(a.elab));
  js_object(program_fields(a, ~ms=now() -. t0) @ fields);
};
