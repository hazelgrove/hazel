open Util;

type t = {
  hole_id: Id.t,
  slice: TypSlice.t,
  enum: Futures.t,
};

open Sequence;
let alternate = (s1, s2) =>
  zip_full(s1, s2)
  |> Sequence.concat_map(
       ~f=
         fun
         | `Left(x)
         | `Right(x) => singleton(x)
         | `Both(x, y) => of_list([x, y]),
     );

let bool_lits: Futures.t =
  of_list([Bool(true) |> DHExp.fresh, Bool(false) |> DHExp.fresh]);
let nonneg_ints = unfold(~init=0, ~f=i => Some((i, i + 1))); // 0, 1, 2, 3, ...
let neg_ints = unfold(~init=-1, ~f=i => Some((i, i - 1))); // -1, -2, -3, ...
let ints = alternate(nonneg_ints, neg_ints);
let int_lits = ints >>| (i => Int(i) |> DHExp.fresh);
let float_lits = ints >>| (i => Float(Float.of_int(i)) |> DHExp.fresh); // Approximating floats by just enumerating ints
let char_lits =
  init(256, ~f=i =>
    i |> Char.chr |> String.make(1) |> (x => String(x) |> DHExp.fresh)
  );
let string_lits = char_lits; // TODO: all strings, this is a huge state space though...

let fresh_hole = () => DHExp.hole([]) |> DHExp.fresh;
let rec enum_typ: Typ.term => Futures.t =
  fun
  | Var(_) => failwith("Expeted normalised types during instantiation")
  | Unknown(_) => fresh_hole() |> singleton
  | Bool => bool_lits
  | Int => int_lits
  | Float => float_lits
  | String => string_lits
  | Parens(t) => enum_typ(Typ.term_of(t))
  // NOTE: Arrow instantiation only produces constant functions, are there situations where a constant function is not enough?
  | Arrow(_, _) =>
    Fun(EmptyHole |> DHPat.fresh, fresh_hole(), None, None)
    |> DHExp.fresh
    |> singleton // TODO: Check casting logic for potential need for re-elaboration?
  | List(_) =>
    unfold_step(~init=0, ~f=i =>
      Yield({
        value: ListLit(List.init(i, _ => fresh_hole())) |> DHExp.fresh,
        state: i + 1,
      })
    )
  | Prod(ts) =>
    Tuple(List.init(List.length(ts), _ => fresh_hole()))
    |> DHExp.fresh
    |> singleton
  | Sum(_) => failwith("TODO")
  | Ap(_)
  | Forall(_)
  | Rec(_) => failwith("Extension Task"); // Normalised types should mean these aren't even needed much?

let construct = (hole_id: Id.t, slice: TypSlice.t) => {
  hole_id,
  slice,
  enum: enum_typ(slice |> TypSlice.typ_term_of),
};

// Substitutes all terms with a rep_id corresponding to the hole id to give every possible instantiation
// Wrapping in a cast from the slice to the hole type to allow evaluation to proceed
// i.e. ? : ? -> Int may instantiate to 0 : Int -> ? -> Int
// Note that this requires the hole to have a UNIQUE id. TODO: ensure this
let subst = (d, {hole_id, enum, slice}) =>
  enum
  >>| (
    d' =>
      d
      |> DHExp.map_term(~f_exp=(continue, d) =>
           DHExp.rep_id(d) == hole_id
             ? Cast(d', slice, TypSlice.hole([]) |> TypSlice.fresh)
               |> DHExp.fresh
             : continue(d)
         )
  );
