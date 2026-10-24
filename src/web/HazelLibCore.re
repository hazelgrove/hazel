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
   tuple <-> an object, its labels the keys; another tuple -> an array; a
   constructor <-> {tag: "Name"}, applied <-> {tag: "Name", value}; a
   function -> a JS function, whose arguments (one, or a tuple of several)
   go back to Hazel. Anything else comes back as {hazel: <its printing>}. */
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
      let unknown = Typ.temp(Unknown(Internal));
      let value = Js.Unsafe.get(j, "value");
      Js.to_string(Js.typeof(Js.Unsafe.coerce(value))) == "undefined"
        ? Exp.fresh(Constructor(name, Some(Some(unknown))))
        : Exp.fresh(
            Ap(
              Operators.Forward,
              Exp.fresh(
                Constructor(
                  name,
                  Some(Some(Typ.temp(Arrow(unknown, unknown)))),
                ),
              ),
              of_js(value),
            ),
          );
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
  /* the program read, and its statics, for an outline (^ticket(587)) */
  program: ModuleProgram.t,
  map: Statics.Map.t,
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
    program,
    map,
  };
};

/* milliseconds, for timing */
let now = (): float =>
  Js.float_of_number(
    Js.Unsafe.coerce(
      Js.Unsafe.js_expr(
        "(typeof performance === 'undefined' ? Date : performance).now()",
      ),
    ),
  );

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

/* (^ticket(587), Matthew 2026-10-23: "maybe we need to re-architect the JS
   library to expose more of modular editors?") A program's outline, from
   Hazel rather than from matching its text: the types its start declares,
   and each module's types and members in order, each member with the type
   Hazel infers for it, each type with its kind (a sum and how many cases, a
   product and how many fields, or another type) and its text. Read from the
   same spec as analyzeProgram:
     outlineProgram(spec) -> { ok, types: [type], modules: [{ name, types:
       [type], members: [{ name, kind, type }] }] }
   a type: { name, kind ("sum" | "product" | "alias"), cases, text }; a
   member's kind: "function", "value" or "module"; its type the inferred
   one's text, or null where Hazel has none. */
let rec outline_pat_name = (p: Pat.t): option(string) =>
  switch (p.term) {
  | Var(x) => Some(x)
  | Parens(p)
  | Asc(p, _) => outline_pat_name(p)
  | _ => None
  };

let rec outline_typ_head = (t: Typ.t): Typ.t =>
  switch (t.term) {
  | Parens(t) => outline_typ_head(t)
  | _ => t
  };

let rec outline_exp_head = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e)
  | Asc(e, _) => outline_exp_head(e)
  | _ => e
  };

let outline_type = (tp: TPat.t, t: Typ.t): option(Js.Unsafe.any) =>
  switch (tp.term) {
  | Var(name) =>
    let (kind, cases) =
      switch (outline_typ_head(t).term) {
      | Sum(variants) => ("sum", List.length(variants))
      | Prod(fields) => ("product", List.length(fields))
      | _ => ("alias", 0)
      };
    Some(
      js_object([
        ("name", js_string(name)),
        ("kind", js_string(kind)),
        ("cases", int_js(cases)),
        ("text", js_string(Typ.pretty_print(t))),
      ]),
    );
  | _ => None
  };

/* A type as the program writes it: each sum or product the context names
   folded back to its name (Hazel infers them unfolded: rec Cmd -> + Done +
   ..., where the program says Cmd), and a function's tuple of arguments in
   one pair of brackets */
/* (the program's own: Hazel's built-in aliases, Option among them, would
   fold a None + Some(?) of the program's into a bare Option) */
let outline_builtin_types: Lazy.t(list(string)) =
  lazy(
    List.filter_map(
      (entry: Ctx.entry) =>
        switch (entry) {
        | TVarEntry({name, _}) => Some(name)
        | _ => None
        },
      ctx().entries,
    )
  );

let outline_aliases = (ctx: Ctx.t): list((string, Typ.t)) =>
  List.filter_map(
    (entry: Ctx.entry) =>
      switch (entry) {
      | TVarEntry({name, kind: Singleton(ty), _})
          when !List.mem(name, Lazy.force(outline_builtin_types)) =>
        switch (outline_typ_head(ty).term) {
        | Sum(_)
        | Prod(_)
        | Rec(_) => Some((name, ty))
        | _ => None
        }
      | _ => None
      },
    ctx.entries,
  );

let outline_typ_text = (ctx: Ctx.t, t: Typ.t): string => {
  let aliases = outline_aliases(ctx);
  let folded =
    Typ.map_term(
      ~f_typ=
        (continue, t: Typ.t) =>
          switch (List.find_opt(((_, d)) => Typ.fast_equal(t, d), aliases)) {
          | Some((name, _)) => Typ.temp(Var(name))
          | None =>
            switch (t.term) {
            /* a recursive sum, inferred: rec Cmd -> ..., bound by the
               alias's own name */
            | Rec({term: Var(n), _}, _)
                when List.exists(((name, _)) => name == n, aliases) =>
              Typ.temp(Var(n))
            | _ => continue(t)
            }
          },
      t,
    );
  switch (folded.term) {
  | Arrow(a, b) =>
    let a_text =
      switch (outline_typ_head(a).term) {
      | Arrow(_) => "(" ++ Typ.pretty_print(a) ++ ")"
      | _ => Typ.pretty_print(a)
      };
    a_text ++ " -> " ++ Typ.pretty_print(b);
  | _ => Typ.pretty_print(folded)
  };
};

let outline_member =
    (map: Statics.Map.t, ~kind: string, p: Pat.t): option(Js.Unsafe.any) =>
  switch (outline_pat_name(p)) {
  | Some(name) =>
    let ty =
      switch (Statics.Map.lookup(Pat.rep_id(p), map)) {
      | Some(InfoPat({ty, ctx, _})) => js_string(outline_typ_text(ctx, ty))
      | _ => Js.Unsafe.inject(Js.null)
      };
    Some(
      js_object([
        ("name", js_string(name)),
        ("kind", js_string(kind)),
        ("type", ty),
      ]),
    );
  | None => None
  };

let outline_module = (map: Statics.Map.t, name: string, def: Exp.t) => {
  let items =
    switch (outline_exp_head(def).term) {
    | Module(items) => items
    | _ => []
    };
  let types =
    List.filter_map(
      (item: Mod.t) =>
        switch (item.term) {
        | ModType(tp, t) => outline_type(tp, t)
        | _ => None
        },
      items,
    );
  let members =
    List.filter_map(
      (item: Mod.t) =>
        switch (item.term) {
        | ModLet(p, e) =>
          let kind =
            switch (outline_exp_head(e).term) {
            | Fun(_) => "function"
            | _ => "value"
            };
          outline_member(map, ~kind, p);
        | ModuleMod(mp, _) =>
          switch (ModuleProgram.name_of_module_pat(mp)) {
          | Some(n) =>
            Some(
              js_object([
                ("name", js_string(n)),
                ("kind", js_string("module")),
                ("type", Js.Unsafe.inject(Js.null)),
              ]),
            )
          | None => None
          }
        | _ => None
        },
      items,
    );
  js_object([
    ("name", js_string(name)),
    ("types", js_array(types)),
    ("members", js_array(members)),
  ]);
};

let outline_program = (spec: Js.Unsafe.any): Js.Unsafe.any => {
  let t0 = now();
  let a = analyze_program_spec(program_spec_of_js(spec));
  /* along the program's chain of definitions: the start's types, then each
     module, as ModuleProgram.of_term walks it */
  let rec chain = (e: Exp.t, types, modules) =>
    switch (e.term) {
    | TyAlias(tp, t, rest) =>
      chain(
        rest,
        switch (outline_type(tp, t)) {
        | Some(j) => [j, ...types]
        | None => types
        },
        modules,
      )
    | Let(_, _, rest)
    | Theorem(_, _, rest)
    | Use(_, rest)
    | Filter(_, rest)
    | Seq(_, rest) => chain(rest, types, modules)
    | ModuleExp(mp, def, rest) =>
      chain(
        rest,
        types,
        switch (ModuleProgram.name_of_module_pat(mp)) {
        | Some(name) => [outline_module(a.map, name, def), ...modules]
        | None => modules
        },
      )
    | _ => (List.rev(types), List.rev(modules))
    };
  let (types, modules) = chain(a.program.term, [], []);
  js_object(
    program_fields(a, ~ms=now() -. t0)
    @ [("types", js_array(types)), ("modules", js_array(modules))],
  );
};
