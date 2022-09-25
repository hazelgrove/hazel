open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type tag = {
  name: string,
  arg: option(Typ.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type adt = (string, list(tag));

/* Add Built-In ADTs here */
let adts: list(adt) = [
  (
    "option_int",
    [{name: "None", arg: None}, {name: "Some", arg: Some(Int)}],
  ),
  (
    "exp",
    [
      {name: "IntLit", arg: Some(Int)},
      {name: "Var", arg: Some(String)},
      {name: "Fun", arg: Some(Prod([String, TypeVar("exp")]))},
      {name: "Ap", arg: Some(Prod([TypeVar("exp"), TypeVar("exp")]))},
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
            | None => Typ.TypeVar(name)
            | Some(typ) => Arrow(typ, TypeVar(name))
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

let are_duplicates = xs =>
  List.length(List.sort_uniq(compare, xs)) == List.length(xs);

// Check type names are unique
assert(are_duplicates(List.map(fst, adts)));
// Check tag names are unique
assert(are_duplicates(List.map(fst, tags)));
