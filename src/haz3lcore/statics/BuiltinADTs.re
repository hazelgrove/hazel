open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type adt = (Token.t, list(Typ.tagged));

let alftyp = Typ.Var("ALFTyp");
let alfexpr = Typ.Var("ALFExpr");
let option_alftyp = Typ.Var("Option_ALFTyp");

let adts: list(adt) = [
  (
    "Color",
    [
      {tag: "Red", typ: Prod([])},
      {tag: "Green", typ: Prod([])},
      {tag: "Blue", typ: Prod([])},
    ],
  ),
  (
    "Option_ALFTyp",
    [{tag: "None", typ: Prod([])}, {tag: "Some", typ: alftyp}],
  ),
  (
    "ALFTyp",
    {
      [
        {tag: "Num", typ: Prod([])},
        {tag: "Bool", typ: Prod([])},
        {tag: "Arrow", typ: Prod([alftyp, alftyp])},
        {tag: "Prod", typ: Prod([alftyp, alftyp])},
      ];
    },
  ),
  (
    "ALFExpr",
    {
      [
        {tag: "NumLit", typ: Int},
        {tag: "Plus", typ: Prod([alfexpr, alfexpr])},
        {tag: "Times", typ: Prod([alfexpr, alfexpr])},
        {tag: "Minus", typ: Prod([alfexpr, alfexpr])},
        {tag: "Eq", typ: Prod([alfexpr, alfexpr])},
        {tag: "Lt", typ: Prod([alfexpr, alfexpr])},
        {tag: "Gt", typ: Prod([alfexpr, alfexpr])},
        {tag: "Neg", typ: alfexpr},
        {tag: "BoolLit", typ: Bool},
        {tag: "If", typ: Prod([alfexpr, alfexpr, alfexpr])},
        {tag: "Var", typ: String},
        {tag: "Let", typ: Prod([String, alfexpr, alfexpr])},
        {tag: "Fun", typ: Prod([String, alftyp, alfexpr])},
        {tag: "Ap", typ: Prod([alfexpr, alfexpr])},
        {tag: "Pair", typ: Prod([alfexpr, alfexpr])},
        {tag: "PrjL", typ: alfexpr},
        {tag: "PrjR", typ: alfexpr},
        {tag: "LetPair", typ: Prod([String, String, alfexpr, alfexpr])},
      ];
    },
  ),
];

let is_typ_var = name => List.assoc_opt(name, adts);

//TODO(andrew):cleanup
let tags = (adts: list(adt)): list((Token.t, Typ.t)) =>
  List.map(
    ((name, tags)) => {
      List.map(
        (adt: Typ.tagged) =>
          (
            adt.tag,
            switch (adt.typ) {
            | Prod([]) => Typ.Var(name)
            | typ => Arrow(typ, Var(name))
            },
          ),
        tags,
      )
    },
    adts,
  )
  |> List.flatten;

let get_tag_typ = (tag: Token.t): option(Typ.t) =>
  List.assoc_opt(tag, tags(adts));

// Check type names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, adts)));
// Check tag names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, tags(adts))));
