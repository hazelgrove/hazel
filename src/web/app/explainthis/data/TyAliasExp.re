open Haz3lcore;
open Example;
open ExplainThisForm;

let tpat = tpat("p");
let typ_def = typ("ty_def");
let tyalias_base_exp_coloring_ids = (~tpat_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(tpat), tpat_id),
  (Piece.id(typ_def), def_id),
];
let tyalias_exp = (~tpat_id: Id.t, ~def_id: Id.t): form => {
  let explanation =
    Printf.sprintf(
      "The [*type*](%s) is bound to the [*type variable*](%s) in the body.",
      Id.to_string(def_id),
      Id.to_string(tpat_id),
    );
  let form = [
    mk_tyalias([[space(), tpat, space()], [space(), typ_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TyAliasExp,
    syntactic_form: form,
    colorings: tyalias_base_exp_coloring_ids(~tpat_id, ~def_id),
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tyalias_exps = (~tpat_id: Id.t, ~def_id: Id.t): group =>
  singleton(tyalias_exp(~tpat_id, ~def_id));

let option_ex = {
  sub_id: ParamTyAlias1,
  term:
    mk_example(
      "type Opt(a) = + Nothing + Just(a) in \n let x : Opt(Int) = Just(1) in x",
    ),
  message: "Opt is a type function; its constructors are polymorphic, so Just(1) has type Opt(Int) here.",
};

let list_ex = {
  sub_id: ParamTyAlias2,
  term:
    mk_example(
      "type L(a) = + Nil + Cons(a, L(a)) in \n let xs : L(Bool) = Cons(true, Nil) in xs",
    ),
  message: "A recursive parameterized type: the list of a's refers to itself in its definition.",
};

/* `type T(a, b) = ty in e` */
let param_tyalias_exp =
    (~head_id: Id.t, ~param_ids: list(Id.t), ~def_id: Id.t): form => {
  let n = List.length(param_ids);
  let head = Example.tpat("T");
  let (params_seg, param_pieces) = tpat_list("a", n);
  let typ_def = typ("ty_def");
  {
    id: ParamTyAliasExp,
    syntactic_form: [
      mk_tyalias([
        [space(), head, mk_ap_tpat([params_seg]), space()],
        [space(), typ_def, space()],
      ]),
      linebreak(),
      exp("e_body"),
    ],
    colorings:
      [(Piece.id(head), head_id), (Piece.id(typ_def), def_id)]
      @ zip_colorings(param_pieces, param_ids),
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "Defines the parameterized type [*T*](%s) in the body: a type function of the %s that gives the [*type*](%s) with arguments substituted for them, so `T(%s)` is a type. Constructors of a sum type defined this way are polymorphic in the parameters.",
        Id.to_string(head_id),
        links("parameter", param_ids),
        Id.to_string(def_id),
        String.concat(", ", List.init(n, _ => "X")),
      ),
    examples: [option_ex, list_ex],
  };
};

let param_tyalias_exps =
    (~head_id: Id.t, ~param_ids: list(Id.t), ~def_id: Id.t): group =>
  singleton(param_tyalias_exp(~head_id, ~param_ids, ~def_id));
