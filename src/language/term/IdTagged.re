open Util;

module IdTag = {
  /* Secondary runs stored as (before, after) pairs.
     - before: secondary immediately before this term (after preceding delimiter or sibling)
     - after: secondary immediately after this term (before following delimiter or sibling) */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type secondary_runs = (list(Secondary.t), list(Secondary.t));

  let empty_secondary: secondary_runs = ([], []);

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    [@show.opaque]
    ids: list(Id.t),
    secondary: secondary_runs,
  };

  let fresh = (): t => {
    ids: [Id.mk()],
    secondary: empty_secondary,
  };
  let temp = (): t => {
    ids: [Id.invalid],
    secondary: empty_secondary,
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
    annotation: IdTag.fresh(),
  };
};
let fresh_deterministic = (prev_id, term): t('a) => {
  {
    term,
    annotation: {
      ids: [Id.next(prev_id)],
      secondary: IdTag.empty_secondary,
    },
  };
};

let term_of = (x: Grammar.Annotated.t('a, 'b)) => x.term;
let unwrap = (x: t('a)) => (
  x.term,
  term' => {
    ...x,
    term: term',
  },
);
let rep_id = ({annotation: {ids, _}, _}: Grammar.Annotated.t('a, IdTag.t)) =>
  List.hd(ids);

let fast_copy = (id, {term, _}: t('a)): t('a) => {
  term,
  annotation: {
    ids: [id],
    secondary: IdTag.empty_secondary,
  },
};
let new_ids = ({term, annotation: {ids: _, secondary}}: t('a)): t('a) => {
  term,
  annotation: {
    ids: [Id.mk()],
    secondary,
  },
};

let ids = ({annotation: {ids, _}, _}: t('a)) => ids;

let replace_temp = ({term, annotation: {ids, secondary}}: t('a)): t('a) => {
  term,
  annotation: {
    ids: ids == [Id.invalid] ? [Id.mk()] : ids,
    secondary,
  },
};

module FreshGrammar =
  Grammar.Factory({
    type t = IdTag.t;
    let default_value = (): IdTag.t => IdTag.fresh();
  });
