open Util;

module IdTag = {
  /* Secondary runs stored as (before, after) pairs.
     - before: secondary immediately before this term (after preceding delimiter or sibling)
     - after: secondary immediately after this term (before following delimiter or sibling) */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type secondary_runs = (list(Secondary.t), list(Secondary.t));

  let empty_secondary: secondary_runs = ([], []);

  /* Shard provenance for canonical completion: for each tile of this term
     that was completed, the shard indices physically present in the visible
     segment (missing shards were synthesized). Empty for fully-typed terms.
     Printing emits only the listed shards; see ExpToSegment. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type incomplete_tiles = list((Id.t, list(int)));

  /* Copy semantics: every field must declare what happens under the
     copy/re-id operations (fast_copy, Exp.replace_all_ids, replace_temp).
     Two axes decide it:
     - id-bearing? secondary entries and incomplete masks reference ids,
       so value copies must DROP them (or remap, which nothing needs yet);
       naively preserving them would duplicate ids one level down.
     - source- or value-meaning? lexeme is id-free and carries semantics
       into values (hole flavor, stuck unknown ops), so copies KEEP it;
       literal spellings are gated at display instead
       (ExpToSegment.Settings.use_literal_lexemes).
     Cache/recompute gates comparing terms must not use annotation-blind
     equality — see Exp.fast_equal_with_lexemes. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    [@show.opaque]
    ids: list(Id.t),
    secondary: secondary_runs,
    incomplete: incomplete_tiles,
    /* Surface spelling of single-token terms whose canonical print
       differs (int/float spellings, quoted labels, explicit/LLM hole
       tokens). Printing uses it verbatim after validating it still
       matches the term's value; None for rebuilt/internal terms. */
    lexeme: option(string),
  };

  /* Constructors for IdTag.t */

  /* Create annotation with fresh id and empty secondary */
  let fresh = (): t => {
    ids: [Id.mk()],
    secondary: empty_secondary,
    incomplete: [],
    lexeme: None,
  };

  /* Create annotation with invalid id and empty secondary (for temporary terms) */
  let temp = (): t => {
    //TODO(andrew): understand why this is thunked
    ids: [Id.invalid],
    secondary: empty_secondary,
    incomplete: [],
    lexeme: None,
  };

  /* Create annotation with specific ids and empty secondary.
     Use for internally-generated terms that don't touch surface syntax. */
  let mk_internal = (ids: list(Id.t)): t => {
    ids,
    secondary: empty_secondary,
    incomplete: [],
    lexeme: None,
  };

  /* Create annotation with specific ids and secondary.
     Use for terms from surface syntax where formatting should be preserved. */
  let mk =
      (
        ~incomplete: incomplete_tiles=[],
        ~lexeme: option(string)=None,
        ids: list(Id.t),
        secondary,
      )
      : t => {
    ids,
    secondary,
    incomplete,
    lexeme,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Annotated.t('a, IdTag.t);

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
let mk =
    (
      ~incomplete: IdTag.incomplete_tiles=[],
      ~lexeme: option(string)=None,
      ids: list(Id.t),
      secondary: IdTag.secondary_runs,
      term: 'a,
    )
    : t('a) => {
  term,
  annotation: IdTag.mk(~incomplete, ~lexeme, ids, secondary),
};

let term_of = (x: Annotated.t('a, 'b)) => x.term;
let unwrap = (x: t('a)) => (
  x.term,
  term' => {
    ...x,
    term: term',
  },
);
let rep_id = ({annotation: {ids, _}, _}: Annotated.t('a, IdTag.t)) =>
  List.hd(ids);

/* Copy term with a new id, discarding secondary.
   Note: This discards secondary (formatting) information. If preserving
   formatting through evaluation becomes important, this would need to
   accept a source term and copy its secondary, or we'd need a variant
   like fast_copy_with_secondary(id, source, term).
   Also discards shard provenance: the new ids no longer reference the
   original tiles. */
let fast_copy = (id, {term, annotation}: t('a)): t('a) => {
  term,
  annotation: {
    ids: [id],
    secondary: IdTag.empty_secondary,
    incomplete: [],
    lexeme: annotation.lexeme,
  },
};

let ids = ({annotation: {ids, _}, _}: t('a)) => ids;

/* Replace invalid temp ids with fresh ids, preserving secondary */
let replace_temp =
    ({term, annotation: {ids, secondary, incomplete, lexeme}}: t('a))
    : t('a) => {
  term,
  annotation: {
    ids: ids == [Id.invalid] ? [Id.mk()] : ids,
    secondary,
    incomplete,
    lexeme,
  },
};

module FreshGrammar =
  Grammar.Factory({
    type t = IdTag.t;
    let default_value = (): IdTag.t => IdTag.fresh();
  });
