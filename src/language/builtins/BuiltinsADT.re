open BuiltinsUtil;
module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;

let sum_type = (variants: list((string, option(Typ.t)))): Typ.t =>
  variants
  |> List.map(((name, typ_opt)) =>
       ConstructorMap.Variant(
         name,
         ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ()),
         typ_opt,
       )
     )
  |> sum;

let meta_type: Typ.t = sum_type([("$e", None), ("$v", None)]);

module Ord = {
  let t: Typ.t = sum_type([("Lt", None), ("Eq", None), ("Gt", None)]);
  /* `Ord` (the surface name). Used in builtin function signatures
     so the cursor inspector displays `(?, ?) -> Ord` rather than
     `(?, ?) -> + Lt + Eq + Gt`. Not parameterized, so the named
     form is just `Var("Ord")`. */
  let named: Typ.t = (Var("Ord"): Typ.term) |> Typ.fresh;

  open IdTagged.FreshGrammar;
  let lt = Exp.constructor("Lt", Some(Some(t)));
  let eq = Exp.constructor("Eq", Some(Some(t)));
  let gt = Exp.constructor("Gt", Some(Some(t)));
  let lt_pat = Pat.constructor("Lt", Some(Some(t)));
  let eq_pat = Pat.constructor("Eq", Some(Some(t)));
  let gt_pat = Pat.constructor("Gt", Some(Some(t)));
};

/* `Option` and `Either` are parameterized in the prelude:

     Option : (Type) -> Type   ≡   typfun a -> + None + Some(a)
     Either : (Type, Type) -> Type
                                 ≡   typfun a, b -> + L(a) + R(b)

   The user's ctx holds them as `TypFun`-bodied alias entries with
   `(Type, …) -> Type` kind, and the constructors get polymorphic
   schemas (`Some : poly a -> a -> Option(a)`, etc.) via
   `Ctx.add_ctrs_with_params`.

   The prelude functions (`option_map`, `option_bind`, etc.) stay
   *gradually typed* rather than polymorphic — their signatures use
   the applied form `Option(?)` / `Either(?, ?)` (which normalizes
   to a `Sum` whose payloads are `?`), and the constructor
   annotations baked into their `imp` Exps follow suit. */
let var_typ = (name: string): Typ.t => Var(name) |> Typ.fresh;
let var_tpat = (name: string): TPat.t => Var(name) |> TPat.fresh;
let unknown_internal = Unknown(Internal) |> Typ.fresh;

module Option = {
  let params: list(TPat.t) = [var_tpat("a")];
  /* `+ None + Some(a)` — the alias body before the `TypFun`
     wrapping. Used to derive both the stored alias type and the
     constructor map. */
  let body: Typ.t =
    sum_type([("None", None), ("Some", Some(var_typ("a")))]);
  /* The full parameterized alias type stored in the ctx:
     `TypFun(a, body)` (kind `(Type) -> Type`). */
  let t: Typ.t = TypFun(List.hd(params), body) |> Typ.fresh;
  /* `Option(?)` — gradually-typed instance. Used in builtin
     function signatures (so the cursor inspector displays
     `(Option(?), ? -> ?) -> Option(?)` rather than the noisy
     `(+ None + Some(?), ? -> ?) -> + None + Some(?)`). The
     parameterized alias itself (`t` above) is what's registered in
     the user's typing ctx; the runtime annotations on the BUILTIN
     constructors are monomorphic-with-`?` because the BUILTIN
     functions are gradually typed. */
  let applied: Typ.t =
    TypParamAp(var_typ("Option"), unknown_internal) |> Typ.fresh;
  /* Normalized form `Sum[None, Some(?)]` used as constructor
     annotations baked into BUILTIN `imp` Exps. The runtime
     constructor-match and `DHExp.ty_comparable` traversals don't
     carry a ctx, so they can't resolve `Var("Option")` —
     pre-normalize here. */
  let applied_normalized: Typ.t =
    sum_type([
      ("None", None),
      ("Some", Some(unknown_internal)),
    ]);

  open IdTagged.FreshGrammar;

  let none = Exp.constructor("None", Some(Some(applied_normalized)));

  let some =
    Exp.constructor(
      "Some",
      Some(Some(arrow(unknown_internal, applied_normalized))),
    );

  let pat_none = Pat.constructor("None", Some(Some(applied_normalized)));

  let pat_some =
    Pat.constructor(
      "Some",
      Some(Some(arrow(unknown_internal, applied_normalized))),
    );

  let builtins: list(hazel_fn) = [
    {
      str: {|fix option_map -> fun (opt, f) -> case opt
               | None => None
               | Some(x) => Some(f(x))
             end|},
      name: "option_map",
      arg: Prod([applied, arrow(unknown_internal, unknown_internal)]),
      ret: applied.term,
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("option_map"),
              fn(
                Pat.tuple([Pat.var("opt"), Pat.var("f")]),
                match(
                  var("opt"),
                  [
                    (pat_none, none),
                    (
                      Pat.ap(pat_some, Pat.var("x")),
                      ap(Forward, some, ap(Forward, var("f"), var("x"))),
                    ),
                  ],
                ),
                None,
                Some("option_map+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix option_bind -> fun (opt, f) -> case opt
               | None => None
               | Some x => f(x)
             end|},
      name: "option_bind",
      arg: Prod([applied, arrow(unknown_internal, unknown_internal)]),
      ret: applied.term,
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("option_bind"),
              fn(
                Pat.tuple([Pat.var("opt"), Pat.var("f")]),
                match(
                  var("opt"),
                  [
                    (pat_none, none),
                    (
                      Pat.ap(pat_some, Pat.var("x")),
                      ap(Forward, var("f"), var("x")),
                    ),
                  ],
                ),
                None,
                Some("option_bind+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      name: "option_to_list",
      arg: applied.term,
      ret: List(unknown_internal),
      str: {|fix option_to_list -> fun opt -> case opt
               | None => []
               | Some x => [x]
             end|},
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("option_to_list"),
              fn(
                Pat.var("opt"),
                match(
                  var("opt"),
                  [
                    (pat_none, list_lit([])),
                    (
                      Pat.ap(pat_some, Pat.var("x")),
                      list_lit([var("x")]),
                    ),
                  ],
                ),
                None,
                Some("option_to_list+"),
              ),
              None,
            )
          )
        );
      },
    },
  ];
};

module Either = {
  let params: list(TPat.t) = [var_tpat("a"), var_tpat("b")];
  let body: Typ.t =
    sum_type([
      ("L", Some(var_typ("a"))),
      ("R", Some(var_typ("b"))),
    ]);
  /* `TypFun(Tuple([a, b]), body)` — the parameterized alias stored
     in the ctx with kind `(Type, Type) -> Type`. */
  let t: Typ.t = {
    let tuple_binder: TPat.t = Tuple(params) |> TPat.fresh;
    TypFun(tuple_binder, body) |> Typ.fresh;
  };
  /* `Either(?, ?)` — gradually-typed instance used in builtin
     function signatures. Surface form for nice display. */
  let applied: Typ.t = {
    let args: Typ.t =
      TypTuple([unknown_internal, unknown_internal]) |> Typ.fresh;
    TypParamAp(var_typ("Either"), args) |> Typ.fresh;
  };
  /* Normalized form for runtime constructor annotations. */
  let applied_normalized: Typ.t =
    sum_type([
      ("L", Some(unknown_internal)),
      ("R", Some(unknown_internal)),
    ]);

  open IdTagged.FreshGrammar;
  let left =
    Exp.constructor(
      "L",
      Some(Some(arrow(unknown_internal, applied_normalized))),
    );
  let right =
    Exp.constructor(
      "R",
      Some(Some(arrow(unknown_internal, applied_normalized))),
    );

  let pat_left =
    Pat.constructor(
      "L",
      Some(Some(arrow(unknown_internal, applied_normalized))),
    );
  let pat_right =
    Pat.constructor(
      "R",
      Some(Some(arrow(unknown_internal, applied_normalized))),
    );
};

/* Type aliases registered in the prelude ctx. Each entry carries
   the params list (empty for `Ord` and `$Meta`, [a] for `Option`,
   [a, b] for `Either`) so we can register constructors with
   matching polymorphic schemas via `add_ctrs_with_params`. */
let parameterized_aliases:
  list((string, list(TPat.t), Typ.t, Typ.t)) = [
  /* (name, params, alias_stored_type, alias_body_for_ctrs) */
  ("Ord", [], Ord.t, Ord.t),
  ("Option", Option.params, Option.t, Option.body),
  ("Either", Either.params, Either.t, Either.body),
  ("$Meta", [], meta_type, meta_type),
];

let create_type_alias =
    (name: string, typ: Typ.t, params: list(TPat.t)): Ctx.entry =>
  Ctx.TVarEntry({
    name,
    id: Id.invalid,
    kind: Ctx.Singleton(typ),
    typ_kind: TypKind.of_param_count(List.length(params)),
  });

/* TVarEntries for each alias, with the kind matching the param count
   (`Type` for n=0, `(Type) -> Type` for n=1, etc.). */
let types: list(Ctx.entry) =
  List.map(
    ((name, params, typ, _body)) => create_type_alias(name, typ, params),
    parameterized_aliases,
  );

/* Constructor entries. For each parameterized alias, register its
   constructors with the params so they get polymorphic schemas
   (`Some : poly a -> a -> Option(a)`, etc.). For non-parameterized
   aliases (Ord, $Meta), `add_ctrs_with_params` with `params=[]`
   collapses to the original `add_ctrs` behavior. */
let constructors: Ctx.t = {
  List.fold_left(
    (ctx, (name, params, _typ, body)) => {
      let cons_map =
        switch (Typ.term_of(body)) {
        | Sum(cons_map) => cons_map
        | _ => failwith("Type alias body must be a sum type")
        };
      Ctx.add_ctrs_with_params(ctx, name, params, cons_map);
    },
    Ctx.empty,
    parameterized_aliases,
  );
};

let builtins = Option.builtins;
let constructor_entries = constructors.entries @ types;
