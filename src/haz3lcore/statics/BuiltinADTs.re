open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type tag = {
  name: string,
  arg: option(Typ.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type adt = (string, list(tag));

/* Add Built-In ADTs here. Type names and constructor names
   must be globally unique (enforced by assertions below) */
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
    "OptionInt",
    [{name: "None", arg: None}, {name: "Some", arg: Some(Int)}],
  ),
  (
    "Exp",
    [
      {name: "Error", arg: Some(Int)},
      {name: "Var", arg: Some(String)},
      {name: "Fun", arg: Some(Prod([String, Var("exp")]))},
      {name: "Ap", arg: Some(Prod([Var("exp"), Var("exp")]))},
    ],
  ),
];

let is_typ_var = name => List.assoc_opt(name, adts);

let tags: list((string, Typ.t)) =
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

let get_tag_typ = (tag_name: string): option(Typ.t) =>
  List.assoc_opt(tag_name, tags);

// Check type names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, adts)));
// Check tag names are unique
assert(Util.ListUtil.are_duplicates(List.map(fst, tags)));
