open Util;
open Nondeterminism;

module Make =
       (S: Search)
       : {
         let instantiate: (Environment.t, DHExp.t) => S.t(DHExp.t);
       } => {
  open S;
  open S.Infix;
  open S.Syntax;

  let fresh_hole = () => DHExp.hole([]) |> DHExp.fresh;
  let fresh_hole_slice = () => TypSlice.hole([]) |> TypSlice.fresh;

  let bool_lits =
    return(Bool(true) |> DHExp.fresh)
    <|> return(Bool(false) |> DHExp.fresh);

  let rec ints_from = n =>
    return(n) <|> wrap(n |>- (n => ints_from(n + 1)));
  let rec ints_to = n => return(n) <|> wrap(n |>- (n => ints_to(n - 1)));
  let ints = ints_to(0) <|> ints_from(1);
  let int_lits = ints >>| (i => Int(i) |> DHExp.fresh);
  let float_lits = ints >>| (i => Float(Float.of_int(i)) |> DHExp.fresh); // Approximating floats by just enumerating ints

  let chars =
    List.init(94, i => return(i + 32 |> Char.chr |> String.make(1)))
    |> concat;
  let rec strings = () =>
    return("") <|> wrap(chars >>= (c => strings() >>| (s => c ++ s)));
  let string_lits = strings() >>| (s => String(s) |> DHExp.fresh);
  let rec enum_typ = (t, env) =>
    switch (t |> TypSlice.typ_term_of) {
    | Var(_) => failwith("Expeted normalised types during instantiation?")
    | Label(name) => return(Label(name) |> DHExp.temp)
    | Unknown(_) => fail
    | Bool => bool_lits
    | Int => int_lits
    | Float => float_lits
    | String => string_lits
    | Parens(_) => enum_typ(TypSlice.unparens(t), env)
    // NOTE: Arrow instantiation does not currently instantiate different patterns or bindings
    // This is not required for finding cast errors, but would be desirable for program generation or logic programming.
    // Note: any cast errors requiring non-constant functions will not be found
    | Arrow(_, _) =>
      return(
        Cast(
          Fun(EmptyHole |> DHPat.fresh, fresh_hole(), None, None)
          |> DHExp.fresh,
          `Typ(
            Arrow(
              Unknown(Internal) |> Typ.fresh,
              Unknown(Internal) |> Typ.fresh,
            ),
          )
          |> TypSlice.fresh,
          t,
        )
        |> DHExp.fresh,
      )
    // Note: must cast tail hole back to list!
    | List(_) =>
      return(ListLit([]) |> DHExp.fresh)
      <|> return(
            Cons(
              fresh_hole(),
              Cast(
                fresh_hole(),
                TypSlice.hole([]) |> TypSlice.fresh,
                `Typ(List(Unknown(Internal) |> Typ.fresh)) |> TypSlice.fresh,
              )
              |> DHExp.fresh,
            )
            |> DHExp.fresh,
          )
      >>| (
        e =>
          Cast(
            e,
            `Typ(List(Unknown(Internal) |> Typ.fresh)) |> TypSlice.fresh,
            t,
          )
          |> DHExp.fresh
      )
    | Prod(ts) =>
      return(
        Cast(
          Tuple(List.map(_ => fresh_hole(), ts)) |> DHExp.fresh,
          `Typ(Prod(List.map(_ => Unknown(Internal) |> Typ.fresh, ts)))
          |> TypSlice.fresh,
          t,
        )
        |> DHExp.fresh,
      )
    | TupLabel(label, ty) =>
      // TODO: make this produce a ground type!
      let* label = enum_typ(label |> TypSlice.t_of_typ_t, env);
      let* body = enum_typ(ty |> TypSlice.t_of_typ_t, env);
      return(TupLabel(label, body) |> DHExp.fresh);
    // Note: sum ground instantiations are the variants with unknown argument: i.e.
    //       type T = A + B(Int) would get A or B(?) with ground type A + B(?)
    //       These ground types differ with the + ? used in the dynamics/elaboration
    | Sum(m) =>
      m
      |> List.map(
           fun
           | ConstructorMap.BadEntry(_) => fail
           | Variant(ctr, _, None) =>
             return(
               Constructor(ctr, Some(t |> TypSlice.typ_of)) |> DHExp.fresh,
             )
           | Variant(ctr, _, Some(_)) =>
             return(
               Ap(
                 Forward,
                 Constructor(ctr, Some(t |> TypSlice.typ_of)) |> DHExp.fresh,
                 fresh_hole(),
               )
               |> DHExp.fresh,
             ),
         )
      |> concat
      >>| (
        e =>
          Cast(
            e,
            `Typ(
              Sum(
                m
                |> List.map(
                     fun
                     | ConstructorMap.Variant(ctr, ids, Some(_)) =>
                       ConstructorMap.Variant(
                         ctr,
                         ids,
                         Some(Unknown(Internal) |> Typ.fresh),
                       )
                     | v => v,
                   ),
              ),
            )
            |> TypSlice.fresh,
            `Typ(Unknown(Internal)) |> TypSlice.fresh,
          )
          |> DHExp.fresh
      )
    | Ap(_)
    | Forall(_)
    | Rec(_) => failwith("Extension Task") // Normalised types should mean these aren't even needed much?
    };
  // TODO: Check environment for variables which have the given type.
  //<|> (Environment.of_typ(t) |> List.map(x => return(Var(x) |> DHExp.fresh)) |> List.fold(choice, fail))
  let enum_typ = (t: TypSlice.t, ctx) =>
    enum_typ(t, ctx)
    >>| (
      e =>
        Cast(e, t, TypSlice.hole([]) |> TypSlice.fresh)
        |> DHExp.fresh
        |> Evaluator.evaluate(~env=Builtins.env_init)
        |> fst
    ); // Evaluate to fixup casts

  // Hole Substitution. Replaces all holes of id: hole_id with an instantiation
  // substitutes d' for holes of hole_id in d
  let rec subst_term = (d', hole_id, d) =>
    d
    |> DHExp.map_term(~f_exp=(continue, d'') =>
         DHExp.rep_id(d'') == hole_id
           ? d'
           : (
             switch (DHExp.term_of(d'')) {
             | Closure(env, d) => {
                 ...d'',
                 term:
                   Closure(
                     env
                     |> ClosureEnvironment.map(((_, d)) =>
                          d
                          |> subst_term(d', hole_id)
                          |> Evaluator.evaluate(~env=Builtins.env_init)
                          |> fst
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
                            |> subst_term(d', hole_id)
                            |> Evaluator.evaluate(~env=Builtins.env_init)
                            |> fst
                          ),
                        ),
                   ),
               }
             | _ => continue(d'')
             }
           )
       );

  // Substitutes all terms with a rep_id corresponding to the hole id to give every possible instantiation
  // Wrapping in a cast from the slice to the hole type to allow evaluation to proceed
  // i.e. ? : ? -> Int may instantiate to 0 : Int -> ? -> Int
  // Note that this requires the hole to have a UNIQUE id. TODO: ensure this
  // The holes may also exist inside the closures, so must be substituted there too
  // Also evaluate the substitutions within closures in order to maintain that closures only contain values. (Somewhat inefficient)
  let rec instantiate = (env, d) =>
    RedexHoleType.(
      find(env, d)
      |> (
        fun
        | None => fail // No hole in redex
        | Hole(_) => fail // No useful type information present
        | HoleCast(id, t) =>
          enum_typ(t, env) >>| (d' => subst_term(d', id, d))
        | Match(d', branches) =>
          // Nondeterministically wrap scrutinee in casts from match branches
          branches
          |> List.filter_map(p =>
               switch (Pat.term_of(p)) {
               | Cast(_, t, _) => Some(t)
               | _ => None
               }
             )
          |> ListUtil.remove_duplicates(TypSlice.fast_equal)
          // Such a scrutinee only occurs when it is of the dynamic type, so cast ? -> t
          |> List.map(t =>
               return(
                 subst_term(
                   Cast(
                     Cast(d', TypSlice.hole([]) |> TypSlice.fresh, t)
                     |> Exp.fresh,
                     t,
                     TypSlice.hole([]) |> TypSlice.fresh,
                   )
                   |> Exp.fresh,
                   Exp.rep_id(d'),
                   d,
                 ),
               )
             )
          |> List.fold_left(S.choice, fail)
      )
    );
};
