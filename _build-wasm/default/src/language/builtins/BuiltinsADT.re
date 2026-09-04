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

module JSON = {
  /* Self-reference for the recursive type */
  let self: Typ.t = var("JSON");

  /* type JSON =
     + Assoc([(String, JSON)])
     + Bool(Bool)
     + Float(Float)
     + Int(Int)
     + List([JSON])
     + String(String)
     + Null */
  let t: Typ.t =
    rec_(
      Fresh.TPat.var("JSON"),
      sum_type([
        ("Assoc", Some(list(prod([string(), self])))),
        ("Bool", Some(bool())),
        ("Float", Some(float())),
        ("Int", Some(int())),
        ("List", Some(list(self))),
        ("String", Some(string())),
        ("Null", None),
      ]),
    );
};

// List of type aliases to add to the context
let type_aliases: list((string, Typ.t)) = [
  ("Ord", Ord.t),
  ("Option", Option.t),
  ("Either", Either.t),
  ("JSON", JSON.t),
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
        | Rec(_, tbody) =>
          switch (Typ.term_of(tbody)) {
          | Sum(cons_map) => cons_map
          | _ => failwith("Type alias must be a sum type")
          }
        | _ => failwith("Type alias must be a sum type")
        };
      Ctx.add_ctrs(ctx, name, cons_map);
    },
    Ctx.empty,
    type_aliases,
  );
};

let builtins = Option.builtins;
let constructor_entries = constructors.entries @ types;

/* Build an Ord-returning compare builtin from an Atom.compare_entry, the
 * same way of_atom_builtin handles atom-to-atom conversions. */
let of_atom_compare =
    ((name, Atom.Cmp(kind, cmp)): (string, Atom.compare_entry))
    : BuiltinsUtil.fn => {
  let ty = Typ.fresh_atom(Atom.cls_of_kind(kind));
  BuiltinsUtil.{
    name,
    arg: Prod([ty, ty]),
    ret: Ord.t.term,
    imp:
      binary((d1, d2) => {
        let-unbox n1 = (Atom(kind), d1);
        let-unbox n2 = (Atom(kind), d2);
        Some(
          switch (cmp(n1, n2)) {
          | 0 => Ord.eq
          | n when n < 0 => Ord.lt
          | _ => Ord.gt
          },
        );
      }),
    custom_statics: None,
  };
};

/* Flip Lt ↔ Gt, leave Eq alone. Lets a descending sort reuse an ascending
 * comparator without a second pass to reverse the list. */
let invert_ord: BuiltinsUtil.fn =
  BuiltinsUtil.{
    name: "invert_ord",
    arg: Ord.t.term,
    ret: Ord.t.term,
    imp: d =>
      switch (DHExp.term_of(d)) {
      | Constructor("Lt", _) => Some(Ord.gt)
      | Constructor("Gt", _) => Some(Ord.lt)
      | Constructor("Eq", _) => Some(Ord.eq)
      | _ => None
      },
    custom_statics: None,
  };

let ord_builtins: list(BuiltinsUtil.fn) =
  [invert_ord] @ List.map(of_atom_compare, Atom.compare_builtins);
