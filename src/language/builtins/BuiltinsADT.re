open BuiltinsUtil;
module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;

let sum_type = (variants: list((string, option(Typ.t)))): Typ.t =>
  variants
  |> List.map(((name, typ_opt)) =>
       ConstructorMap.Variant(name, [Id.mk()], typ_opt)
     )
  |> sum;

let meta_type: Typ.t = sum_type([("$e", None), ("$v", None)]);

module Ord = {
  let t: Typ.t = sum_type([("Lt", None), ("Eq", None), ("Gt", None)]);

  open IdTagged.FreshGrammar;
  let lt = Exp.constructor("Lt", Some(Some(t)));
  let eq = Exp.constructor("Eq", Some(Some(t)));
  let gt = Exp.constructor("Gt", Some(Some(t)));
  let lt_pat = Pat.constructor("Lt", Some(Some(t)));
  let eq_pat = Pat.constructor("Eq", Some(Some(t)));
  let gt_pat = Pat.constructor("Gt", Some(Some(t)));
};

module Either = {
  let t: Typ.t =
    sum_type([
      ("Left", Some(Unknown(Internal) |> Typ.fresh)),
      ("Right", Some(Unknown(Internal) |> Typ.fresh)),
    ]);

  open IdTagged.FreshGrammar;
  let left =
    Exp.constructor("Left", Some(Some(arrow(unknown(SynSwitch), t))));
  let right =
    Exp.constructor("Right", Some(Some(arrow(unknown(SynSwitch), t))));

  let pat_left =
    Pat.constructor("Left", Some(Some(arrow(unknown(SynSwitch), t))));
  let pat_right =
    Pat.constructor("Right", Some(Some(arrow(unknown(SynSwitch), t))));
};

module Option = {
  let t: Typ.t =
    sum_type([
      ("None", None),
      ("Some", Some(Unknown(Internal) |> Typ.fresh)),
    ]);

  open IdTagged.FreshGrammar;

  // Confirm that we want the type on the constructors for both expressions and patterns
  let none = Exp.constructor("None", Some(Some(t)));

  let some =
    Exp.constructor("Some", Some(Some(arrow(unknown(SynSwitch), t))));

  let pat_none = Pat.constructor("None", Some(Some(t)));

  let pat_some =
    Pat.constructor("Some", Some(Some(arrow(unknown(SynSwitch), t))));

  let builtins: list(hazel_fn) = [
    {
      str: {|fix option_map -> fun (opt, f) -> case opt
               | None => None
               | Some(x) => Some(f(x))
             end|},
      name: "option_map",
      arg: Prod([t, arrow(unknown(Internal), unknown(Internal))]),
      ret: Unknown(Internal),
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
                ~name="option_map+",
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
      arg: Prod([t, arrow(unknown(Internal), unknown(Internal))]),
      ret: Unknown(Internal),
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
                ~name="option_bind+",
              ),
              None,
            )
          )
        );
      },
    },
    {
      name: "option_to_list",
      arg: t.term,
      ret: List(unknown(Internal)),
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
                ~name="option_to_list+",
              ),
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
  ("Ord", Ord.t),
  ("Option", Option.t),
  ("Either", Either.t),
  ("$Meta", meta_type),
];

let create_type_alias = (name: string, typ: Typ.t): Ctx.entry =>
  Ctx.TVarEntry({
    name,
    id: Id.invalid,
    kind: Ctx.Singleton(typ),
  });

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

let builtins = Option.builtins;
let constructor_entries = constructors.entries @ types;
