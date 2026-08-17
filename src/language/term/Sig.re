[@deriving (show({with_path: false}), sexp, yojson, enumerate, eq)]
type cls =
  | Invalid
  | EmptyHole
  | MultiHole
  | SigLet
  | SigType;

include TermBase.Sig;

let fresh: term => t = IdTagged.fresh;

let rep_id: t => Id.t = IdTagged.rep_id;

let hole = (tms: list(TermBase.Any.t)): TermBase.Sig.term =>
  switch (tms) {
  | [] => EmptyHole
  | [_, ..._] => MultiHole(tms)
  };

let cls_of_term: Grammar.sig_term('a) => cls =
  fun
  | Invalid(_) => Invalid
  | EmptyHole => EmptyHole
  | MultiHole(_) => MultiHole
  | SigLet(_) => SigLet
  | SigType(_, _) => SigType;

let show_cls: cls => string =
  fun
  | Invalid => "Invalid signature"
  | MultiHole => "Broken signature"
  | EmptyHole => "Signature hole"
  | SigLet => "Let declaration"
  | SigType => "Type declaration";

let temp: term => t =
  term => {
    term,
    annotation: IdTagged.IdTag.temp(),
  };
