[@deriving (show({with_path: false}), sexp, yojson)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | Var;

include TermBase.TPat;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

module Fresh = {
  open TermBase;
  let tpinvalid = s => Invalid(s) |> fresh;
  let tpempty_hole = () => EmptyHole |> fresh;
  let tpmulti_hole = tms => MultiHole(tms) |> fresh;
  let tpvar = x => Var(x) |> fresh;

  // The following function exists only as a reminder to update the above when a new constructor is added.
  let ok = (_: 'a) => failwith("covered should never be called");
  let covered = (e: tpat_term) => {
    switch (e) {
    | Invalid(_) => ok(tpinvalid)
    | EmptyHole => ok(tpempty_hole)
    | MultiHole(_) => ok(tpmulti_hole)
    | Var(_) => ok(tpvar)
    };
  };
};

let hole = (tms: list(TermBase.Any.t)): TermBase.TPat.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: term => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | Var(_) => Var;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid type alias"
  | MultiHole => "Broken type alias"
  | EmptyHole => "Empty type alias hole"
  | Var => "Type alias";

let temp: term => t = term => {term, ids: [Id.invalid], copied: false};
