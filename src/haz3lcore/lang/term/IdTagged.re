open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = {
  [@show.opaque]
  ids: list(Id.t),
  [@show.opaque]
  /* UExp invariant: copied should always be false, and the id should be unique
     DHExp invariant: if copied is true, then this term and its children may not
     have unique ids. The flag is used to avoid deep-copying expressions during
     evaluation, while keeping track of where we will need to replace the ids
     at the end of evaluation to keep them unique.*/
  copied: bool,
  term: 'a,
};

let fresh = term => {
  {ids: [Id.mk()], copied: false, term};
};

let term_of = x => x.term;
let unwrap = x => (x.term, term' => {...x, term: term'});
let rep_id = ({ids, _}) => List.hd(ids);
let fast_copy = (id, {term, _}) => {ids: [id], term, copied: true};
let new_ids =
  fun
  | {ids: _, term, copied} => {ids: [Id.mk()], term, copied};
