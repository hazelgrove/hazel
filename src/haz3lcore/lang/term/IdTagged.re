[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = Grammar.Annotated.t('a, Grammar.IdTag.t);

// To be used if you want to remove the id from the debug output
// let pp: ((Format.formatter, 'a) => unit, Format.formatter, t('a)) => unit =
//   (fmt_a, formatter, ta) => {
//     fmt_a(formatter, ta.term);
//   };
let fresh = (term: 'a): Grammar.Annotated.t('a, Grammar.IdTag.t) => {
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
let rep_id =
    ({annotation: {ids, _}, _}: Grammar.Annotated.t('a, Grammar.IdTag.t)) =>
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
