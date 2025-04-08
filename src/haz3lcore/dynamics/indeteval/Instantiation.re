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
    <||> return(Bool(false) |> DHExp.fresh);
  // Using bind to carefully avoid infinite recursion as OCaml is strict.
  // To represent lazily
  let rec ints_from = n =>
    return(n) <||> wrap(n |>- (n => ints_from(n + 1)));
  let rec ints_to = n => return(n) <||> wrap(n |>- (n => ints_to(n - 1)));
  let ints = ints_to(0) <|> ints_from(1);
  let int_lits = ints >>| (i => Int(i) |> DHExp.fresh);
  let float_lits = ints >>| (i => Float(Float.of_int(i)) |> DHExp.fresh); // Approximating floats by just enumerating ints
  let char_lits =
    List.init(256, i =>
      i |> Char.chr |> String.make(1) |> (x => String(x) |> DHExp.fresh)
    )
    |> List.map(return)
    |> concat;
  let string_lits = char_lits; // TODO: all strings, this is a huge state space though...
  let rec enum_typ: (Typ.term, Environment.t) => t(DHExp.t) =
    (t, env) =>
      switch (t) {
      | Var(_) => failwith("Expeted normalised types during instantiation?")
      | Label(name) => return(Label(name) |> DHExp.temp)
      | Unknown(_) => return(fresh_hole())
      | Bool => bool_lits
      | Int => int_lits
      | Float => float_lits
      | String => string_lits
      | Parens(t) => enum_typ(Typ.term_of(t), env)
      // NOTE: Arrow instantiation does not currently instantiate different patterns or bindings
      // The above is not required for finding cast errors, but would be desirable for program generation or logic programming.
      | Arrow(_, t2) =>
        enum_typ(Typ.term_of(t2), env)
        >>| (e => Fun(EmptyHole |> DHPat.fresh, e, None, None) |> DHExp.fresh) // TODO: Check casting logic for potential need for re-elaboration?
      | List(_) =>
        return(ListLit([]) |> DHExp.fresh)
        <||> return(Cons(fresh_hole(), fresh_hole()) |> DHExp.fresh)
      | Prod(ts) =>
        ts
        |> List.map(t => enum_typ(Typ.term_of(t), env))
        |> (
          l =>
            List.fold_right(
              (t, acc) => {
                let. t' = t;
                let. acc' = acc;
                return([t', ...acc']);
              },
              l,
              return([]),
            )
            >>| (l => Tuple(l) |> DHExp.fresh)
        )
      | TupLabel(label, ty) =>
        let. label = enum_typ(Typ.term_of(label), env);
        let. body = enum_typ(Typ.term_of(ty), env);
        return(TupLabel(label, body) |> DHExp.fresh);
      | Sum(_) => failwith("TODO")
      | Ap(_)
      | Forall(_)
      | Rec(_) => failwith("Extension Task") // Normalised types should mean these aren't even needed much?
      };
  // TODO: Check environment for variables which have the given type.
  //<|> (Environment.of_typ(t) |> List.map(x => return(Var(x) |> DHExp.fresh)) |> List.fold(choice, fail))
  let enum_typ = (t, ctx) =>
    enum_typ(t |> TypSlice.typ_term_of, ctx)
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
  let instantiate = (env, d) =>
    RedexHoleType.(
      find(env, d)
      |> (
        fun
        | None => fail // No hole in redex
        | Hole(_) => fail // No useful type information present
        | HoleCast(id, t) =>
          enum_typ(t, env) >>| (d' => subst_term(d', id, d))
        | Match(d, branches) => fail // TODO!  May be possible to refactor branch to inside RedexHoleType, by using the indetmatch returns from unboxing in transition?
      )
    );
};
