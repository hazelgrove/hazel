open Util;

module IdTag = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    [@show.opaque]
    ids: list(Id.t),
    [@show.opaque]
    /* Exp invariant: copied should always be false, and the id should be unique
       DHExp invariant: if copied is true, then this term and its children may not
       have unique ids. The flag is used to avoid deep-copying expressions during
       evaluation, while keeping track of where we will need to replace the ids
       at the end of evaluation to keep them unique.*/
    copied: bool,
  };

  let fresh = (): t => {
    {ids: [Id.mk()], copied: false};
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Grammar.Annotated.t('a, IdTag.t);

// To be used if you want to remove the id from the debug output
// let pp: ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit =
//   (fmt_a, formatter, ta) => {
//     fmt_a(formatter, ta.term);
//   };
let fresh = (term: 'a): Grammar.Annotated.t('a, IdTag.t) => {
  {
    term,
    annotation: {
      ids: [Id.mk()],
      copied: false,
    },
  };
};
let fresh_deterministic = (prev_id, term): t('a) => {
  {
    term,
    annotation: {
      ids: [Id.next(prev_id)],
      copied: false,
    },
  };
};

let term_of = (x: Grammar.Annotated.t('a, 'b)) => x.term;
let unwrap = (x: t('a)) => (x.term, term' => {...x, term: term'});
let rep_id = ({annotation: {ids, _}, _}: Grammar.Annotated.t('a, IdTag.t)) =>
  List.hd(ids);

let fast_copy = (id, {term, _}: t('a)): t('a) => {
  term,
  annotation: {
    copied: true,
    ids: [id],
  },
};
let new_ids = ({term, annotation: {ids: _, copied}}: t('a)): t('a) => {
  term,
  annotation: {
    ids: [Id.mk()],
    copied,
  },
};

let ids = ({annotation: {ids, _}, _}: t('a)) => ids;
let copied = ({annotation: {copied, _}, _}: t('a)) => copied;

let replace_temp = ({term, annotation: {ids, copied}}: t('a)): t('a) => {
  term,
  annotation: {
    ids: ids == [Id.invalid] ? [Id.mk()] : ids,
    copied,
  },
};
