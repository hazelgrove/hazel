open Util;

open Sequence;

type t = {
  hole_id: Id.t,
  slice: TypSlice.t,
  enum: Futures.t,
};

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

// Note: Casts should always be to ground types, so lists, products, etc. are instantiated to ground instantiations (i.e. a tuple of holes)
let rec enum_typ: TypSlice.t => Futures.t =
  slc => {
    switch (TypSlice.typ_term_of(slc)) {
    | Var(_) => failwith("Expeted normalised types during instantiation")
    | Unknown(_) => fresh_hole() |> singleton
    | Bool => bool_lits
    | Int => int_lits
    | Float => float_lits
    | String => string_lits
    | Parens(_) => enum_typ(TypSlice.unparens(slc))
    // NOTE: Arrow instantiation only produces constant functions, are there situations where a constant function is not enough?
    | Arrow(_, _) =>
      Fun(EmptyHole |> DHPat.fresh, fresh_hole(), None, None)
      |> DHExp.fresh
      |> singleton // TODO: Check casting logic for potential need for re-elaboration?
    | List(_) =>
      Sequence.of_list([
        ListLit([]) |> DHExp.fresh,
        Cons(
          fresh_hole(),
          Cast(fresh_hole(), TypSlice.hole([]) |> TypSlice.fresh, slc)
          |> DHExp.fresh,
        )
        |> DHExp.fresh,
      ])
    | Prod(ts) =>
      Tuple(List.init(List.length(ts), _ => fresh_hole()))
      |> DHExp.fresh
      |> singleton
    | Sum(_) => failwith("TODO")
    | Ap(_)
    | Forall(_)
    | Rec(_) => failwith("Extension Task") // Normalised types should mean these aren't even needed much?
    };
  };

let construct_typ = (hole_id: Id.t, slice: TypSlice.t) => {
  hole_id,
  slice,
  enum: enum_typ(slice),
};

let construct_enum = (hole_id, slice, enum) => {hole_id, slice, enum};

//Least specific instantiation from a pattern. d is an Indet term.
let rec enum_pat = (d: DHExp.t, dp: Pat.t): list(t) => {
  let (term, rewrap) = IdTagged.unwrap(d);
  let rep_id = DHExp.rep_id(d);
  let pat_slice = typ_term =>
    TypSlice.(
      typ_term
      |> Typ.fresh
      |> t_of_typ_t
      |> wrap_global(slice_of_ids(dp.ids))
    );
  let singleton_list = x => [x];
  switch (term, DHPat.term_of(dp)) {
  // Invalid patterns
  | (_, Invalid(_))
  | (_, EmptyHole)
  | (_, MultiHole(_)) => [] // TODO: exploring branches behind holes/instantiating pattern holes
  // No instantiations required to match wildcard pattern
  | (_, Wild) => []
  // Instantiating Single Holes
  | (EmptyHole, Int(n)) =>
    Int(n)
    |> DHExp.fresh
    |> singleton
    |> construct_enum(rep_id, pat_slice(Int))
    |> singleton_list
  | (EmptyHole, Float(n)) =>
    Float(n)
    |> DHExp.fresh
    |> singleton
    |> construct_enum(rep_id, pat_slice(Float))
    |> singleton_list
  | (EmptyHole, Bool(b)) =>
    Bool(b)
    |> DHExp.fresh
    |> singleton
    |> construct_enum(rep_id, pat_slice(Bool))
    |> singleton_list
  | (EmptyHole, String(s)) =>
    String(s)
    |> DHExp.fresh
    |> singleton
    |> construct_enum(rep_id, pat_slice(String))
    |> singleton_list
  | (EmptyHole, ListLit(xs)) =>
    ListLit(xs |> List.map(_ => fresh_hole()))
    |> DHExp.fresh
    |> singleton
    |> construct_enum(
         rep_id,
         pat_slice(List(Unknown(Internal) |> Typ.fresh)),
       )
    |> singleton_list
  | (EmptyHole, Cons(x, xs)) =>
    let list_slice = pat_slice(List(Unknown(Internal) |> Typ.fresh));
    Cons(
      fresh_hole(),
      Cast(fresh_hole(), TypSlice.hole([]) |> TypSlice.fresh, list_slice)
      |> DHExp.fresh,
    )
    |> DHExp.fresh
    |> singleton
    |> construct_enum(rep_id, list_slice)
    |> singleton_list;
  | (EmptyHole, Constructor(ctr, _))
  | (EmptyHole, Ap({term: Constructor(ctr, _), _}, _)) =>
    failwith("TODO: Instantiate Constructors")
  | _ => failwith("TODO")
  };
};

// Dual of the above, enumerates all instantiations that do NOT match with pattern
// Note that this also excludes instantiations that return IndetMatch
let rec enum_not_pat = (d: DHExp.t, dp: Pat.t): list(t) => {
  failwith("TODO: not pattern");
};

// Instantiate match branches
// TODO: Keep track of constraints which the instantiation must not be in order to avoid earlier branches.
//       At the moment, | x::xs => x | l => l would instantiate the second branch to ?, and still remain stuck
let rec enum_match = (d: DHExp.t, dps: Pat.t): list(list(t)) => {
  failwith("TODO: Enum Match");
};

// Substitutes all terms with a rep_id corresponding to the hole id to give every possible instantiation
// Wrapping in a cast from the slice to the hole type to allow evaluation to proceed
// i.e. ? : ? -> Int may instantiate to 0 : Int -> ? -> Int
// Note that this requires the hole to have a UNIQUE id. TODO: ensure this
// The holes may also exist inside the closures, so must be substituted there too
// Also evaluate the substitutions within closures in order to maintain that closures only contain values. (Somewhat inefficient)
let subst = (d, {hole_id, enum, slice}) => {
  let rec subst_term = (d', d) =>
    d
    |> DHExp.map_term(~f_exp=(continue, d'') =>
         DHExp.rep_id(d'') == hole_id
           ? Cast(d', slice, TypSlice.hole([]) |> TypSlice.fresh)
             |> DHExp.fresh
           : (
             switch (DHExp.term_of(d'')) {
             | Closure(env, d) => {
                 ...d'',
                 term:
                   Closure(
                     env
                     |> ClosureEnvironment.map(((_, d)) =>
                          d
                          |> subst_term(d')
                          |> Evaluator.evaluate''(Builtins.env_init)
                        ),
                     continue(d),
                   ),
               }
             | FixF(p, d, env_opt) => {
                 ...d'',
                 term:
                   FixF(
                     p,
                     continue(d),
                     env_opt
                     |> Option.map(
                          ClosureEnvironment.map(((_, d)) =>
                            d
                            |> subst_term(d')
                            |> Evaluator.evaluate''(Builtins.env_init)
                          ),
                        ),
                   ),
               }
             | _ => continue(d'')
             }
           )
       );
  enum >>| (d' => d |> subst_term(d'));
};
