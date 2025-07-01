module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;

type fn = {
  name: string,
  arg: Typ.term,
  ret: Typ.term,
  str: string,
  imp: Exp.t,
};

let sum_type = (variants: list((string, option(Typ.t)))): Typ.t =>
  variants
  |> List.map(((name, typ_opt)) =>
       ConstructorMap.Variant(name, [Id.mk()], typ_opt)
     )
  |> sum;

let meta_type: Typ.t = sum_type([("$e", None), ("$v", None)]);

let option_type: Typ.t =
  sum_type([
    ("None", None),
    ("Some", Some(Unknown(Internal) |> Typ.fresh)),
  ]);

let result_type: Typ.t =
  sum_type([
    ("Ok", Some(Unknown(Internal) |> Typ.fresh)),
    ("Error", Some(Unknown(Internal) |> Typ.fresh)),
  ]);

let ord_type: Typ.t = sum_type([("Lt", None), ("Eq", None), ("Gt", None)]);

module Option = {
  // Confirm that we want the type on the constructors for both expressions and patterns
  let some =
    IdTagged.FreshGrammar.Exp.constructor(
      "Some",
      Some(Some(arrow(unknown(SynSwitch), option_type))),
    );
  let pat_some =
    IdTagged.FreshGrammar.Pat.constructor(
      "Some",
      Some(Some(arrow(unknown(SynSwitch), option_type))),
    );
  let pat_none =
    IdTagged.FreshGrammar.Pat.constructor("None", Some(Some(option_type)));
  let none =
    IdTagged.FreshGrammar.Exp.constructor("None", Some(Some(option_type)));

  let builtins = [
    {
      str: {|fun (opt, f) -> case opt
               | None => None
               | Some x => Some(f(x))
             end|},
      name: "option_map",
      arg:
        Prod([option_type, arrow(unknown(Internal), unknown(Internal))]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fn(
              Pat.tuple([Pat.var("opt"), Pat.var("f")]),
              match(
                var("opt"),
                [
                  (Pat.constructor("None", None), none),
                  (
                    Pat.ap(Pat.constructor("Some", None), Pat.var("x")),
                    ap(Forward, some, ap(Forward, var("f"), var("x"))),
                  ),
                ],
              ),
              None,
              None,
            )
          )
        );
      },
    },
    {
      str: {|fun (opt, f) -> case opt
               | None => None
               | Some x => f(x)
             end|},
      name: "option_bind",
      arg:
        Prod([option_type, arrow(unknown(Internal), unknown(Internal))]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fn(
              Pat.tuple([Pat.var("opt"), Pat.var("f")]),
              match(
                var("opt"),
                [
                  (Pat.constructor("None", None), none),
                  (
                    Pat.ap(Pat.constructor("Some", None), Pat.var("x")),
                    ap(Forward, var("f"), var("x")),
                  ),
                ],
              ),
              None,
              None,
            )
          )
        );
      },
    },
    {
      name: "option_to_list",
      arg: option_type.term,
      ret: List(unknown(Internal)),
      str: {|fun opt -> case opt
               | None => []
               | Some x => [x]
             end|},
      imp: {
        Fresh.(
          Exp.(
            fn(
              Pat.var("opt"),
              match(
                var("opt"),
                [
                  (Pat.constructor("None", None), list_lit([])),
                  (
                    Pat.ap(Pat.constructor("Some", None), Pat.var("x")),
                    list_lit([var("x")]),
                  ),
                ],
              ),
              None,
              None,
            )
          )
        );
      },
    },
  ];
};

// List of type aliases to add to the context
let type_aliases: list((string, Typ.t)) = [
  ("Ord", ord_type),
  ("Result", result_type),
  ("Option", option_type),
  ("$Meta", meta_type),
];

let create_type_alias = (name: string, typ: Typ.t): Ctx.entry => {
  Ctx.TVarEntry({
    name,
    id: Id.invalid,
    kind: Ctx.Singleton(typ),
  });
};

// Convert type aliases to context entries
let types: list(Ctx.entry) =
  List.map(((name, typ)) => create_type_alias(name, typ), type_aliases);

// Add constructors for type aliases to the context
let constructors: Ctx.t = {
  List.fold_left(
    (ctx, (name, typ)) => {
      let cons_map =
        switch (Typ.term_of(typ)) {
        | Sum(cons_map) => cons_map
        | _ => failwith("Type alias must be a sum type")
        };
      Ctx.add_ctrs(ctx, name, Id.invalid, cons_map);
    },
    Ctx.empty,
    type_aliases,
  );
};
