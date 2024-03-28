include Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = {
  ids: list(Id.t),
  /* UExp invariant: copied should always be false, and the id should be unique
     DHExp invariant: if copied is true, then this term and its children may not
     have unique ids. */
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

// let serialization = (f1, f2) =>
//   StructureShareSexp.structure_share_here(
//     rep_id,
//     sexp_of_t(f1),
//     t_of_sexp(f2),
//   );

// let sexp_of_t = f1 => serialization(f1, Obj.magic()) |> fst;
// let t_of_sexp = f2 => serialization(Obj.magic(), f2) |> snd;
