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

  /* Constructors for IdTag.t */

  /* Create annotation with fresh id and empty secondary */
  let fresh = (): t => {
    ids: [Id.mk()],
    secondary: empty_secondary,
  };

  /* Constant annotation with invalid id and empty secondary (for temporary terms).
     Not thunked — returns the same physical object every call, enabling === checks. */
  let temp: t = {
    ids: [Id.invalid],
    secondary: empty_secondary,
  };

  /* Create annotation with specific ids and empty secondary.
     Use for internally-generated terms that don't touch surface syntax. */
  let mk_internal = (ids: list(Id.t)): t => {
    ids,
    secondary: empty_secondary,
  };

  /* Create annotation with specific ids and secondary.
     Use for terms from surface syntax where formatting should be preserved. */
  let mk = (ids: list(Id.t), secondary: secondary_runs): t => {
    ids,
    secondary,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Grammar.Annotated.t('a, IdTag.t);

// To be used if you want to remove the id from the debug output
// let pp: ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit =
//   (fmt_a, formatter, ta) => {
//     fmt_a(formatter, ta.term);
//   };
/* Constructors for t('a) - wrapped terms */

/* Create term with fresh id and empty secondary */
let fresh = (term: 'a): t('a) => {
  term,
  annotation: IdTag.fresh(),
};

/* Create term with deterministic next id and empty secondary */
let fresh_deterministic = (prev_id, term): t('a) => {
  term,
  annotation: IdTag.mk_internal([Id.next(prev_id)]),
};

/* Create term with specific ids and empty secondary.
   Use for internally-generated terms that don't touch surface syntax. */
let mk_internal = (ids: list(Id.t), term: 'a): t('a) => {
  term,
  annotation: IdTag.mk_internal(ids),
};

/* Create term with specific ids and secondary.
   Use for terms from surface syntax where formatting should be preserved. */
let mk = (ids: list(Id.t), secondary: IdTag.secondary_runs, term: 'a): t('a) => {
  term,
  annotation: IdTag.mk(ids, secondary),
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

/* Copy term with a new id, discarding secondary.
   Note: This discards secondary (formatting) information. If preserving
   formatting through evaluation becomes important, this would need to
   accept a source term and copy its secondary, or we'd need a variant
   like fast_copy_with_secondary(id, source, term). */
let fast_copy = (id, {term, _}: t('a)): t('a) => {
  term,
  annotation: {
    ids: [id],
    secondary: IdTag.empty_secondary,
  },
};

/* Generate new ids for term, preserving secondary */
let new_ids = ({term, annotation: {ids: _, secondary}}: t('a)): t('a) => {
  term,
  annotation: {
    ids: [Id.mk()],
    secondary,
  },
};

let ids = ({annotation: {ids, _}, _}: t('a)) => ids;

/* Replace invalid temp ids with fresh ids, preserving secondary */
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
