open Util;

module IdTag = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {ids: list(Id.t)};

  let fresh = (): t => {ids: [Id.mk()]};
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Grammar.Annotated.t('a, IdTag.t);

// To be used if you want to remove the id from the debug output
let pp: ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit =
  (fmt_a, formatter, ta) => {
    fmt_a(formatter, ta.term);
  };
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
  },
};
let new_ids = ({term, annotation: {ids: _}}: t('a)): t('a) => {
  term,
  annotation: {
    ids: [Id.mk()],
  },
};

let ids = ({annotation: {ids, _}, _}: t('a)) => ids;

let replace_temp = ({term, annotation: {ids}}: t('a)): t('a) => {
  term,
  annotation: {
    ids: ids == [Id.invalid] ? [Id.mk()] : ids,
  },
};

module FreshGrammar =
  Grammar.Factory({
    type t = IdTag.t;
    let default_value = (): IdTag.t => IdTag.fresh();
  });
