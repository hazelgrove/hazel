[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | List
  | Invalid
  | EmptyHole
  | MultiHole;

include TermBase.Hole;

let rep_id: t => Id.t = IdTagged.rep_id;

let fresh: term => t = IdTagged.fresh;

let hole = (tms: list(TermBase.Any.t)): TermBase.Hole.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.hole_term('a) => cls =
  fun
  | List => List
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | ErrorHole
  | MultiHole(_) => MultiHole;

let show_cls: cls => string =
  fun
  | List => "List"
  | Invalid => "Invalid term"
  | MultiHole => "Error term"
  | EmptyHole => "Empty hole";

let temp: term => t =
  term => {
    term,
    annotation: {
      ids: [Id.invalid],
    },
  };
