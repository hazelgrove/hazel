open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type tag = {
  name: Token.t,
  arg: option(Typ.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type adt = (Token.t, list(tag));

let alftyp = Typ.Var("ALFTyp");
let alfexpr = Typ.Var("ALFExpr");
let option_alftyp = Typ.Var("Option_ALFTyp");

let adts: list(adt) = [
  (
    "Color",
    [
      {name: "Red", arg: None},
      {name: "Green", arg: None},
      {name: "Blue", arg: None},
    ],
  ),
  (
    "Option_ALFTyp",
    [{name: "None", arg: None}, {name: "Some", arg: Some(alftyp)}],
  ),
  (
    "ALFTyp",
    {
      [
        {name: "Num", arg: None},
        {name: "Bool", arg: None},
        {name: "Arrow", arg: Some(Prod([alftyp, alftyp]))},
        {name: "Prod", arg: Some(Prod([alftyp, alftyp]))},
      ];
    },
  ),
  (
    "ALFExpr",
    {
      [
        {name: "NumLit", arg: Some(Int)},
        {name: "Plus", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "Times", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "Minus", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "Eq", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "Lt", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "Gt", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "Neg", arg: Some(alfexpr)},
        {name: "BoolLit", arg: Some(Bool)},
        {name: "If", arg: Some(Prod([alfexpr, alfexpr, alfexpr]))},
        {name: "Var", arg: Some(String)},
        {name: "Let", arg: Some(Prod([String, alfexpr, alfexpr]))},
        {name: "Fun", arg: Some(Prod([String, alftyp, alfexpr]))},
        {name: "Ap", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "Pair", arg: Some(Prod([alfexpr, alfexpr]))},
        {name: "PrjL", arg: Some(alfexpr)},
        {name: "PrjR", arg: Some(alfexpr)},
        {
          name: "LetPair",
          arg: Some(Prod([String, String, alfexpr, alfexpr])),
        },
      ];
    },
  ),
];

let is_typ_var = name => List.assoc_opt(name, adts);

//TODO(andrew):cleanup
let tags: list((Token.t, Typ.t)) =
  List.map(
    ((name, tags)) => {
      List.map(
        adt =>
          (
            adt.name,
            switch (adt.arg) {
            | None => Typ.Var(name)
            | Some(typ) => Arrow(typ, Var(name))
            },
          ),
        tags,
      )
    },
    adts,
  )
  |> List.flatten;

let get_tag_typ = (tag_name: Token.t): option(Typ.t) =>
  List.assoc_opt(tag_name, tags);

// Check type names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, adts)));
// Check tag names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, tags)));
