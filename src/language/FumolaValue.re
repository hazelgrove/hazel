open Grammar;

/* Translating a Fumola result into a Hazel value.
 *
 * The Fumola runtime answers with JSON that preserves structure:
 *
 *   {"tag": "Int",    "value": "3"}
 *   {"tag": "Tuple",  "value": [<node>, ...]}
 *   {"tag": "Record", "value": {"<field>": <node>, ...}}
 *   {"tag": "Variant","value": {"name": "<name>", "value": <node> | null}}
 *
 * so that a Fumola tuple can be rebuilt here as a Hazel tuple, rather than as
 * a wrapper a Hazel program would have to take apart. This lives outside
 * Livelit.Fumola so that it can be tested directly: the livelit itself can
 * only be exercised in a browser with the wasm runtime present.
 *
 * A Fumola result has no single static type -- 1 + 2 is an Int, while
 * (get(1), get(2)) is a pair -- so the livelit's expansion type is Unknown and
 * the shape is whatever the program produced.
 */

/* Translation is type-directed: the expected type is pushed down through the
   structure as it is rebuilt. This is what lets a Fumola variant become a
   constructor of the sum type the program actually asked for -- Fumola tags
   live in one flat namespace and carry no home type, so the expected type is
   the only place that information can come from.

   Where the expected type says nothing useful (Unknown, or a shape that does
   not match what came back), translation falls back to resolving names
   against the ambient context, and failing that leaves a constructor
   unannotated for Hazel to mark. */

let unknown = () => Typ.fresh(Unknown(Internal));

/* The expected types of a tuple's elements, given the type expected of the
   tuple. Arity has to agree; otherwise the expectation tells us nothing. */
let element_anas =
    (~tools: LivelitCtx.type_tools, ana: TermBase.Typ.t, arity: int)
    : list(TermBase.Typ.t) =>
  switch (tools.normalize(ana).term) {
  | Prod(tys) when List.length(tys) == arity => tys
  | _ => List.init(arity, _ => unknown())
  };

/* The expected type of a record field, by label. */
let field_ana =
    (~tools: LivelitCtx.type_tools, ana: TermBase.Typ.t, name: string)
    : TermBase.Typ.t => {
  let labelled = (ty: TermBase.Typ.t) =>
    switch (ty.term) {
    | TupLabel({term: Label(l), _}, ty) when l == name => Some(ty)
    | _ => None
    };
  switch (tools.normalize(ana).term) {
  | Prod(tys) =>
    switch (List.filter_map(labelled, tys)) {
    | [ty, ..._] => ty
    | [] => unknown()
    }
  | _ => unknown()
  };
};

/* A constructor, annotated with its type where that can be determined.
   Returns the constructor expression and the type expected of its payload. */
let constructor =
    (~tools: LivelitCtx.type_tools, ~ana: TermBase.Typ.t, name: string)
    : (TermBase.Exp.t, TermBase.Typ.t) =>
  switch (tools.resolve_ctr(~ana, name)) {
  | Some(ty) =>
    /* A constructor carrying a payload resolves to an arrow from the payload
       type, so the domain is what the payload is expected to be. */
    let payload_ana =
      switch (tools.normalize(ty).term) {
      | Arrow(dom, _) => dom
      | _ => unknown()
      };
    (DHExp.fresh(Constructor(name, Some(Some(ty)))), payload_ana);
  | None =>
    /* Unresolvable: leave it unannotated. Hazel marks it as a free
       constructor, which is the honest outcome -- the name does not belong to
       any sum type in scope. */
    (DHExp.fresh(Constructor(name, None)), unknown())
  };

/* The text of a Fumola symbol.

   Symbols are structured -- `x is an identifier, 1 is a number, and they
   compose as `a(`b) and `a.`b -- so this renders the whole structure, without
   the backticks Fumola writes them with:

     `x                    -> "x"
     1                     -> "1"
     `adapton(`settings)   -> "adapton(settings)"

   Backticks are dropped throughout rather than kept on the leaves, so one
   convention holds at every depth.

   This is the one way to get a string out of a livelit without writing a
   quote: Hazel string literals admit no escapes, so a program in a livelit
   cannot contain a double quote at all. Naming a symbol produces the text instead. */
let rec symbol_text = (json: Yojson.Safe.t): result(string, string) => {
  let sub = (name, obj) =>
    switch (List.assoc_opt(name, obj)) {
    | Some(v) => symbol_text(v)
    | None => Error("Fumola symbol is missing field `" ++ name ++ "`")
    };
  switch (json) {
  | `Assoc(obj) =>
    switch (List.assoc_opt("tag", obj), List.assoc_opt("value", obj)) {
    | (Some(`String("Name")), Some(`String(name))) => Ok(name)
    | (Some(`String("Num")), Some(`String(n))) => Ok(n)
    | (Some(`String("Call")), _) =>
      switch (sub("fun", obj), sub("arg", obj)) {
      | (Error(e), _)
      | (_, Error(e)) => Error(e)
      | (Ok(f), Ok(a)) => Ok(f ++ "(" ++ a ++ ")")
      }
    | (Some(`String("Dot")), _) =>
      switch (sub("left", obj), sub("right", obj)) {
      | (Error(e), _)
      | (_, Error(e)) => Error(e)
      | (Ok(l), Ok(r)) => Ok(l ++ "." ++ r)
      }
    | (Some(`String(tag)), _) =>
      Error("Fumola symbol form `" ++ tag ++ "` has no text yet")
    | _ => Error("Fumola symbol has no tag")
    }
  | _ => Error("Fumola symbol is not an object")
  };
};

/* The program that reads the cell a pointer names.
 *
 * peek rather than get: reading a cell to translate it should not change the
 * runtime being translated, and get records a dependency in the adapton
 * graph. Translation runs on every statics pass, so a dependency-recording
 * read would grow the graph as a side effect of merely looking.
 *
 * The `!` unwraps peek's option, assuming the cell is defined. It is, in the
 * case that matters -- the pointer came from a value the runtime just
 * produced -- and a pointer to a cell that has since gone away reads as an
 * error rather than silently as None. */
let reading = (source: string): string => "peek(" ++ source ++ ")!";

/* The Hazel type of a Fumola result.
 *
 * A pointer's type is the type of what it points at, so this dereferences --
 * running get(<name>) in the same instance -- and keeps going for as long as
 * it finds further pointers. That is what lets a pointer be translated with
 * an honest ascription instead of a guess: the type comes from the runtime
 * rather than from anything Hazel could have known.
 *
 * [seen] holds the pointers already followed on this path, so a cell holding
 * a pointer back to itself terminates rather than dereferencing forever.
 * Anything unresolvable becomes Unknown, which is always a sound ascription.
 */
let rec typ_of_json =
        (
          ~eval: string => Yojson.Safe.t,
          ~seen: list(string),
          json: Yojson.Safe.t,
        )
        : TermBase.Typ.t => {
  let tagged = (obj, name) => List.assoc_opt(name, obj);
  switch (json) {
  | `Assoc(obj) =>
    let value = Option.value(tagged(obj, "value"), ~default=`Null);
    switch (tagged(obj, "tag")) {
    | Some(`String("Int")) => Typ.fresh(Atom(Int))
    | Some(`String("Bool")) => Typ.fresh(Atom(Bool))
    | Some(`String("String")) => Typ.fresh(Atom(String))
    /* A symbol becomes its text, so its type is String. */
    | Some(`String("Symbol")) => Typ.fresh(Atom(String))
    | Some(`String("Unit")) => Typ.fresh(Prod([]))
    | Some(`String("Tuple")) =>
      switch (value) {
      | `List(items) =>
        Typ.fresh(Prod(List.map(typ_of_json(~eval, ~seen), items)))
      | _ => unknown()
      }
    | Some(`String("Record")) =>
      switch (value) {
      | `Assoc(fields) =>
        Typ.fresh(
          Prod(
            List.map(
              ((name, v)) =>
                Typ.fresh(
                  TupLabel(
                    Typ.fresh(Label(name)),
                    typ_of_json(~eval, ~seen, v),
                  ),
                ),
              fields,
            ),
          ),
        )
      | _ => unknown()
      }
    /* Hazel's own Option, rather than a synthesized one-variant sum: a
       synthesized type would not be the type Hazel programs actually use, so
       it would typecheck here and fail to interoperate anywhere else. Its
       payload is Unknown, which is all Hazel can express. */
    | Some(`String("Null"))
    | Some(`String("Option")) => BuiltinsADT.Option.t
    /* A variant's home type cannot be recovered from the value: Fumola tags
       live in one flat namespace and name no type. */
    | Some(`String("Variant")) => unknown()
    /* A pointer's type is the type of what it points at. */
    | Some(`String("AdaptonPointer")) =>
      switch (value) {
      | `Assoc(fields) =>
        switch (List.assoc_opt("source", fields)) {
        | Some(`String(source)) when !List.mem(source, seen) =>
          switch (eval(reading(source))) {
          | `Assoc(result) as pointed =>
            switch (List.assoc_opt("ok", result)) {
            | Some(`Bool(true)) =>
              typ_of_json(~eval, ~seen=[source, ...seen], pointed)
            /* A cell that cannot be read tells us nothing about its type. */
            | _ => unknown()
            }
          | _ => unknown()
          }
        /* Already followed on this path: a cycle. */
        | _ => unknown()
        }
      | _ => unknown()
      }
    | _ => unknown()
    };
  | _ => unknown()
  };
};

/* Build the Hazel value denoted by one {tag, value} node. */
let rec exp_of_json =
        (
          ~instance_id: int,
          ~eval: string => Yojson.Safe.t,
          ~ana: TermBase.Typ.t,
          ~tools: LivelitCtx.type_tools,
          json: Yojson.Safe.t,
        )
        : result(TermBase.Exp.t, string) => {
  let field = (name, obj) =>
    switch (List.assoc_opt(name, obj)) {
    | Some(v) => Ok(v)
    | None => Error("Fumola result is missing field `" ++ name ++ "`")
    };
  switch (json) {
  | `Assoc(obj) =>
    switch (field("tag", obj)) {
    | Error(e) => Error(e)
    | Ok(`String(tag)) =>
      switch (field("value", obj)) {
      | Error(e) => Error(e)
      | Ok(value) =>
        exp_of_tagged(~instance_id, ~eval, ~ana, ~tools, tag, value)
      }
    | Ok(_) => Error("Fumola result has a non-string tag")
    }
  | _ => Error("Fumola result is not an object")
  };
}

and exp_of_tagged =
    (
      ~instance_id: int,
      ~eval: string => Yojson.Safe.t,
      ~ana: TermBase.Typ.t,
      ~tools: LivelitCtx.type_tools,
      tag: string,
      value: Yojson.Safe.t,
    )
    : result(TermBase.Exp.t, string) =>
  switch (tag, value) {
  | ("Int", `String(n)) =>
    switch (Bigint.of_string_opt(n)) {
    | Some(n) => Ok(DHExp.fresh(Atom(Int(n))))
    | None => Error("Fumola returned an unreadable integer: " ++ n)
    }
  | ("Bool", `Bool(b)) => Ok(DHExp.fresh(Atom(Bool(b))))
  | ("String", `String(str)) => Ok(DHExp.fresh(Atom(String(str))))
  /* Fumola's unit is Hazel's empty tuple. */
  | ("Unit", _) => Ok(DHExp.fresh(Tuple([])))
  /* A pointer becomes the livelit that reads it: the same Fumola instance,
     running get(<the name it points at>).

     So a livelit that returns a pointer returns a simpler livelit. The term
     is inert where it lands -- statics splices an expansion in without
     traversing it, so a livelit inside one is never expanded, and widgets
     come from projectors over editor syntax, which an expansion is not. What
     it gives you is a faithful reading of what a pointer is: not a value, but
     the expression that would fetch one. Copied into a program it becomes a
     real livelit. */
  | ("AdaptonPointer", `Assoc(fields)) =>
    switch (List.assoc_opt("source", fields)) {
    | Some(`String(source)) =>
      /* The ascription comes from following the pointer: the runtime is asked
         what the cell holds, and the type of that answer is the type of this
         reference. Without it the generated livelit would be asking for an
         annotation nobody can write, since it is code the user never typed. */
      let pointed = eval(reading(source));
      let typ = typ_of_json(~eval, ~seen=[source], pointed);
      Ok(
        DHExp.fresh(
          Asc(
            DHExp.fresh(
              Ap(
                Forward,
                DHExp.fresh(LivelitName("fumola")),
                DHExp.fresh(
                  Tuple([
                    DHExp.fresh(Atom(Int(Bigint.of_int(instance_id)))),
                    DHExp.fresh(Atom(String(reading(source)))),
                  ]),
                ),
              ),
            ),
            typ,
          ),
        ),
      );
    | _ => Error("Fumola pointer has no source text")
    }
  /* A symbol becomes its text. Hazel has no symbol of its own, and the text
     is the part a Hazel program can act on. */
  | ("Symbol", symbol) =>
    switch (symbol_text(symbol)) {
    | Error(e) => Error(e)
    | Ok(text) => Ok(DHExp.fresh(Atom(String(text))))
    }
  | ("Tuple", `List(items)) =>
    let anas = element_anas(~tools, ana, List.length(items));
    let translated =
      List.map2(
        (ana, item) => exp_of_json(~instance_id, ~eval, ~ana, ~tools, item),
        anas,
        items,
      );
    switch (all(translated)) {
    | Error(e) => Error(e)
    | Ok(items) => Ok(DHExp.fresh(Tuple(items)))
    };
  /* A Hazel record is a tuple of labelled elements. */
  | ("Record", `Assoc(fields)) =>
    let element = ((name, value)) => {
      let ana = field_ana(~tools, ana, name);
      switch (exp_of_json(~instance_id, ~eval, ~ana, ~tools, value)) {
      | Error(e) => Error(e)
      | Ok(value) =>
        Ok(DHExp.fresh(TupLabel(DHExp.fresh(Label(name)), value)))
      };
    };
    switch (all(List.map(element, fields))) {
    | Error(e) => Error(e)
    | Ok(elements) => Ok(DHExp.fresh(Tuple(elements)))
    };
  /* Fumola's option is Hazel's: `null` is None, `?(x)` is Some(x). These are
     resolved like any other constructor rather than hard-coded, so they pick
     up whichever Option-shaped type is expected here. */
  | ("Null", _) => Ok(fst(constructor(~tools, ~ana, "None")))
  | ("Option", payload) =>
    let (some, payload_ana) = constructor(~tools, ~ana, "Some");
    switch (
      exp_of_json(~instance_id, ~eval, ~ana=payload_ana, ~tools, payload)
    ) {
    | Error(e) => Error(e)
    | Ok(payload) => Ok(DHExp.fresh(Ap(Forward, some, payload)))
    };
  | ("Variant", `Assoc(fields)) =>
    let name =
      switch (List.assoc_opt("name", fields)) {
      | Some(`String(name)) => Ok(name)
      | _ => Error("Fumola variant has no name")
      };
    switch (name) {
    | Error(e) => Error(e)
    | Ok(name) =>
      let (ctr, payload_ana) = constructor(~tools, ~ana, name);
      switch (List.assoc_opt("value", fields)) {
      | None
      | Some(`Null) => Ok(ctr)
      | Some(payload) =>
        switch (
          exp_of_json(~instance_id, ~eval, ~ana=payload_ana, ~tools, payload)
        ) {
        | Error(e) => Error(e)
        | Ok(payload) => Ok(DHExp.fresh(Ap(Forward, ctr, payload)))
        }
      };
    };
  | (tag, _) =>
    Error(
      "Fumola returned a " ++ tag ++ ", which has no Hazel translation yet",
    )
  }

and all = (results: list(result('a, string))): result(list('a), string) =>
  List.fold_right(
    (r, acc) =>
      switch (r, acc) {
      | (Error(e), _) => Error(e)
      | (_, Error(e)) => Error(e)
      | (Ok(x), Ok(xs)) => Ok([x, ...xs])
      },
    results,
    Ok([]),
  );

/* A short rendering of a result, for the widget itself. Derived from the
   same JSON as the expansion so the two cannot disagree. */
let rec describe = (json: Yojson.Safe.t): string =>
  switch (json) {
  | `Assoc(obj) =>
    let tag =
      switch (List.assoc_opt("tag", obj)) {
      | Some(`String(tag)) => tag
      | _ => ""
      };
    let value =
      switch (List.assoc_opt("value", obj)) {
      | Some(v) => v
      | None => `Null
      };
    switch (tag, value) {
    | ("Int", `String(n)) => n
    | ("Bool", `Bool(b)) => b ? "true" : "false"
    | ("String", `String(str)) => "\"" ++ str ++ "\""
    | ("Unit", _) => "()"
    | ("AdaptonPointer", `Assoc(fields)) =>
      switch (List.assoc_opt("source", fields)) {
      | Some(`String(source)) => reading(source)
      | _ => "<pointer>"
      }
    | ("Symbol", symbol) =>
      switch (symbol_text(symbol)) {
      | Ok(text) => "\"" ++ text ++ "\""
      | Error(_) => "<symbol>"
      }
    | ("Null", _) => "None"
    | ("Option", payload) => "Some(" ++ describe(payload) ++ ")"
    | ("Tuple", `List(items)) =>
      "(" ++ String.concat(", ", List.map(describe, items)) ++ ")"
    | ("Record", `Assoc(fields)) =>
      "{"
      ++ String.concat(
           ", ",
           List.map(((k, v)) => k ++ " = " ++ describe(v), fields),
         )
      ++ "}"
    | ("Variant", `Assoc(fields)) =>
      let name =
        switch (List.assoc_opt("name", fields)) {
        | Some(`String(name)) => name
        | _ => "?"
        };
      switch (List.assoc_opt("value", fields)) {
      | None
      | Some(`Null) => "#" ++ name
      | Some(payload) => "#" ++ name ++ "(" ++ describe(payload) ++ ")"
      };
    | (tag, _) => "<" ++ tag ++ ">"
    };
  | _ => "?"
  };
