[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Unknown(Prov.cls)
  | Var;

include TermBase.TPat;

let fast_equal = Equality.syntactic.tpat;
let equal = fast_equal;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.TPat.term =>
  switch (tms) {
  | [] => Unknown(Hole(EmptyHole) |> Prov.fresh)
  | [_, ..._] => Unknown(Hole(MultiHole(tms)) |> Prov.fresh)
  };

let cls_of_term: Grammar.tpat_term('a) => cls =
  fun
  | Unknown({term: p, _}) => Unknown(Prov.cls_of_term(p))
  | Var(_) => Var;

let show_cls: cls => string =
  fun
  | Unknown(p) => Prov.show_cls(p)
  | Var => "Type alias";

let temp: term => t =
  term => {
    term,
    annotation: {
      ids: [Id.invalid],
    },
  };
