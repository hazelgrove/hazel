// open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('term, 'a) = {
  [@show.opaque]
  annotation: 'a,
  term: 'term,
};

let fresh = (term: 'term): t('term, IdTag.t) => {
  {
    annotation: {
      ids: [Id.mk()],
      copied: false,
    },
    term,
  };
};

// let term_of = x => x.term;
let unwrap = x => (x.term, term' => {...x, term: term'});
let rep_id = (t: t('term, IdTag.t)) => List.hd(t.annotation.ids);
// let fast_copy = (id, {term, _}) => {ids: [id], term, copied: true};
// let new_ids =
//   fun
//   | {ids: _, term, copied} => {ids: [Id.mk()], term, copied};
