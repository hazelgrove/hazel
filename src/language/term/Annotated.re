[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a, 'b) = {
  term: 'a,
  annotation: 'b,
};
/* uncomment to make terms pp without annotation */
//   let pp:
//     type a b.
//       (
//         (Format.formatter, a) => unit,
//         (Format.formatter, b) => unit,
//         Format.formatter,
//         t(a, b)
//       ) =>
//       unit =
//     (fmt_a, _, fmtr, t) => {
//       fmt_a(fmtr, t.term);
//     };

let term_of = x => x.term;
let unwrap = x => (
  x.term,
  term' => {
    ...x,
    term: term',
  },
);

let empty = term => {
  term,
  annotation: (),
};
