open Lang;
open Nondet.Syntax;

/*******************************************************************************
 * Identifier generation
 */

let fresh_ident = (gamma, first_char) => {
  let extract_number = (ident: string): option(int) => {
    let ident_len = String.length(ident);

    if (ident_len > 0 && Char.equal(ident.[0], first_char)) {
      ident
      |> StringLabels.sub(~pos=1, ~len=ident_len - 1)
      |> int_of_string_opt;
    } else {
      None;
    };
  };

  let fresh_number: int = (
    gamma
    |> Type_ctx.names
    |> List.filter_map(extract_number)
    |> List2.maximum
    |> Option2.map((+)(1))
    |> Option2.with_default(1): int
  );

  String.make(1, first_char) ++ string_of_int(fresh_number);
};

let function_char = 'f';

let variable_char = 'x';

let match_char = 'y';

/*******************************************************************************
 * Polymorphic helpers
 */

let simple_types: (datatype_ctx, type_ctx) => Nondet.t(typ) = (
  (sigma, gamma) => {
    let polymorphic_variables =
      gamma |> Type_ctx.all_poly |> List.map(x => TVar(x));

    let rank_zero_nd = Nondet.from_list(polymorphic_variables);

    let datatypes_nd = {
      let* (datatype_name, datatype_params) =
        sigma
        |> List.map(((name, (type_params, _))) => (name, type_params))
        |> Nondet.from_list;
      datatype_params
      |> List.map(_ => rank_zero_nd)
      |> Nondet.one_of_each
      |> Nondet.map(args => TData(datatype_name, args));
    };

    Nondet.union([Nondet.from_list(polymorphic_variables), datatypes_nd]);
  }:
    (datatype_ctx, type_ctx) => Nondet.t(typ)
  /* Uncomment for monomorphic instantiations */
  /*
   let monomorphic_datatypes =
     sigma
       |> List2.concat_map
            ( fun (_name, (type_params, _)) ->
                if type_params = [] then
                  [TData (name, [])]
                else
                  []
            )
   in
   */
  /* @ monomorphic_datatypes */
);

let instantiations:
  (datatype_ctx, type_ctx, string, typ) => Nondet.t((typ, exp)) = (
  (sigma, gamma, name, tau) =>
    switch (Type.peel_forall(tau)) {
    | ([], _) => Nondet.pure((tau, EVar(name)))

    | (params, bound_type) =>
      let simple_types_nd = simple_types(sigma, gamma);

      params
      |> List.map(_ => simple_types_nd)
      |> Nondet.one_of_each
      |> Nondet.map(args =>
           (
             Type.substitute_many(
               ~bindings=List.combine(params, args),
               bound_type,
             ),
             Desugar.app(EVar(name), List.map(a => EAType(a), args)),
           )
         );
    }:
    (datatype_ctx, type_ctx, string, typ) => Nondet.t((typ, exp))
);

/*******************************************************************************
 * Term permission helpers
 */

type term_permission =
  | Must
  | May
  | Not;

let parts = (k: int): Nondet.t(list(term_permission)) => {
  let+ i = Nondet.from_list(List2.range(~low=1, ~high=k));
  List2.repeat(i - 1, Not) @ [Must] @ List2.repeat(k - i, May);
};

/*******************************************************************************
 * Caching
 */

/* Types */

type term_kind =
  | E
  | I;

type gen_input = {
  sigma: datatype_ctx,
  term_kind,
  term_size: int,
  rel_binding: option(type_binding),
  goal: gen_goal,
};

/* Hashing */

let hash = ({term_kind, term_size, rel_binding, goal}: gen_input): string => {
  let rec hash_type = (tau: typ): string =>
    switch (tau) {
    | TArr(tau1, tau2) =>
      "[" ++ hash_type(tau1) ++ ">" ++ hash_type(tau2) ++ "]"

    | TTuple(taus) =>
      taus
      |> List.map(hash_type)
      |> String.concat(",")
      |> (s => "(" ++ s ++ ")")

    | TData(d, type_args) =>
      d ++ "<" ++ String.concat(",", List.map(hash_type, type_args))

    | TForall(a, bound_type) => "f:" ++ a ++ "." ++ hash_type(bound_type)

    | TVar(x) => x
    };

  let hash_bind_spec = (bind_spec: bind_spec): string =>
    switch (bind_spec) {
    | NoSpec => "."
    | Rec(name) => "r:" ++ name
    | Arg(name) => "a:" ++ name
    | Dec(name) => "d:" ++ name
    };

  let tk_string =
    switch (term_kind) {
    | E => "E"
    | I => "I"
    };

  let ts_string = string_of_int(term_size);

  let rb_string =
    switch (rel_binding) {
    | None => ""

    | Some((name, (tau, bind_spec))) =>
      name ++ ";" ++ hash_type(tau) ++ ";" ++ hash_bind_spec(bind_spec)
    };

  let goal_string = {
    let (gamma, goal_type, goal_dec) = goal;

    /* Sigma never changes, so no need to keep track of it in the cache */
    let gamma_type_string =
      gamma
      |> Type_ctx.all_type
      |> List.map(((name, (tau, bind_spec))) =>
           name ++ "`" ++ hash_type(tau) ++ "`" ++ hash_bind_spec(bind_spec)
         )
      |> String.concat("&");

    let gamma_poly_string =
      gamma
      |> Type_ctx.all_poly
      |> List.map(name => name)
      |> String.concat("&");

    let gt_string = hash_type(goal_type);

    let gd_string = Option2.with_default(".", goal_dec);

    gamma_type_string
    ++ "!"
    ++ gamma_poly_string
    ++ "!"
    ++ gt_string
    ++ "!"
    ++ gd_string;
  };

  tk_string ++ "!" ++ ts_string ++ "@" ++ rb_string ++ "#" ++ goal_string;
};

/* Caching */

let gen_cache: Hashtbl.t(string, Nondet.t(exp)) = (
  Hashtbl.create(100): Hashtbl.t(string, Nondet.t(exp))
);

let lookup = (gen_input: gen_input): option(Nondet.t(exp)) =>
  gen_input |> hash |> Hashtbl.find_opt(gen_cache);

let record = (gen_input: gen_input, solution: Nondet.t(exp)): Nondet.t(exp) => {
  Hashtbl.add(gen_cache, hash(gen_input), solution);
  solution;
};

/*******************************************************************************
 * Term generation
 */

/* --- Important info about the term generation helpers! ---
 *
 * Do NOT call gen_e, rel_gen_e, gen_i, or rel_gen_i from anywhere EXCEPT inside
 * the actual gen function. The gen function handles caching; no other code
 * should worry about directly manipulating the cache.
 *
 * So, if, for example, genE wants to recursively call itself, it should
 * actually do so indirectly via calling gen with the appropriate arguments.
 *
 * Also, these helpers assume term_size > 0.
 */

let rec gen_e =
        (
          sigma: datatype_ctx,
          term_size: int,
          (gamma, goal_type, goal_dec): gen_goal,
        )
        : Nondet.t(exp) =>
  switch (Type_ctx.peel_type(gamma)) {
  | Some((binding, gamma_rest)) =>
    Nondet.union([
      gen({
        sigma,
        term_kind: E,
        term_size,
        rel_binding: Some(binding),
        goal: (gamma_rest, goal_type, goal_dec),
      }),
      gen({
        sigma,
        term_kind: E,
        term_size,
        rel_binding: None,
        goal: (gamma_rest, goal_type, goal_dec),
      }),
    ])

  | None => Nondet.none
  }

/* A helper for the application part of rel_gen_e */
and rel_gen_e_app =
    (
      sigma: datatype_ctx,
      term_size: int,
      rel_binding: type_binding,
      (gamma, goal_type, goal_dec): gen_goal,
    )
    : Nondet.t(exp) => {
  let* _ = Nondet.guard(Option.is_none(goal_dec)); /* -1 for application */
  /* TODO: perhaps arg_type should differ for rel and non-rel cases? */
  /* Will always happen*/
  let combined_gamma = Type_ctx.add_type(rel_binding, gamma);

  let app_combine =
      (head_nd: Nondet.t(exp), arg_nd: Nondet.t(exp)): Nondet.t(exp) => {
    let* head = head_nd;
    let special =
      switch (Type.bind_spec(combined_gamma, head)) {
      | Rec(_) => true

      | _ => false
      };

    let* arg = arg_nd;
    let+ _ =
      Nondet.guard @@
      Type.structurally_decreasing(combined_gamma, ~head, ~arg);
    EApp(special, head, EAExp(arg));
  };

  let* arg_type =
    combined_gamma
    |> Type_ctx.all_type
    |> List.map(((name, (tau, _))) => {
         let* (specialized_tau, _) =
           instantiations(sigma, combined_gamma, name, tau);
         Nondet.lift_option(
           Type.domain_of_codomain(~codomain=goal_type, specialized_tau),
         );
       })
    |> Nondet.union; /* -1 for application */
  let* partition =
    Nondet.from_list @@
    Int2.partition_permutations(
      ~n=term_size - 1, /* -1 for application */
      ~k=2,
    );
  switch (partition) {
  | [k_head, k_arg] =>
    let head_goal = (gamma, TArr(arg_type, goal_type), None);

    let arg_goal = (gamma, arg_type, None);

    let head_solution_nd =
      gen({
        sigma,
        term_kind: E,
        term_size: k_head,
        rel_binding: None,
        goal: head_goal,
      });

    let rel_head_solution_nd =
      gen({
        sigma,
        term_kind: E,
        term_size: k_head,
        rel_binding: Some(rel_binding),
        goal: head_goal,
      });

    let arg_solution_nd =
      gen({
        sigma,
        term_kind: I,
        term_size: k_arg,
        rel_binding: None,
        goal: arg_goal,
      });

    let rel_arg_solution_nd =
      gen({
        sigma,
        term_kind: I,
        term_size: k_arg,
        rel_binding: Some(rel_binding),
        goal: arg_goal,
      });

    Nondet.union @@
    [
      app_combine(rel_head_solution_nd, arg_solution_nd),
      app_combine(head_solution_nd, rel_arg_solution_nd),
      app_combine(rel_head_solution_nd, rel_arg_solution_nd),
    ];

  | _ =>
    Log.warn(
      "integer partition is incorrect size (is "
      ++ string_of_int(List.length(partition))
      ++ ", should be 2, called with n = "
      ++ string_of_int(term_size - 1)
      ++ ")",
    );
    Nondet.none;
  };
}

and rel_gen_e =
    (
      sigma: datatype_ctx,
      term_size: int,
      (rel_name, (rel_type, rel_bind_spec)) as rel_binding: type_binding,
      (gamma, goal_type, goal_dec) as goal: gen_goal,
    )
    : Nondet.t(exp) =>
  switch (term_size) {
  | 1 =>
    let* (specialized_type, specialized_exp) =
      instantiations(sigma, gamma, rel_name, rel_type);
    /* "Focusing" */
    /* Should be 1-indexed, so use i + 1 */
    if (Type.matches(goal_type, specialized_type)
        && Type.matches_dec(goal_dec, rel_bind_spec)) {
      Nondet.pure(specialized_exp);
    } else if (Type.matches_dec(goal_dec, Type.sub_bind_spec(rel_bind_spec))) {
      switch (rel_type) {
      | TTuple(component_types) =>
        let n = List.length(component_types);

        component_types
        |> List.mapi(Pair2.pair)
        |> List.filter(snd >> Type.matches(goal_type))
        |> List.map(((i, _)) => EProj(n, i + 1, EVar(rel_name)))
        |> Nondet.from_list;

      | _ => Nondet.none
      };
    } else {
      Nondet.none;
    };

  /* No unary operators */
  | 2 => Nondet.none

  /* All applications have size > 2 */
  | _ => rel_gen_e_app(sigma, term_size, rel_binding, goal)
  }

and genp_i =
    (
      sigma: datatype_ctx,
      term_size: int,
      tp: term_permission,
      rel_binding: type_binding,
      (gamma, goal_type, goal_dec): gen_goal,
    )
    : Nondet.t(exp) => {
  let (rel_binding', gamma') =
    switch (tp) {
    | Must => (Some(rel_binding), gamma)

    | May => (None, Type_ctx.add_type(rel_binding, gamma))

    | Not => (None, gamma)
    };

  gen({
    sigma,
    term_kind: I,
    term_size,
    rel_binding: rel_binding',
    goal: (gamma', goal_type, goal_dec),
  });
}

and gen_i =
    (
      sigma: datatype_ctx,
      term_size: int,
      (gamma, goal_type, goal_dec): gen_goal,
    )
    : Nondet.t(exp) => {
  let* _ = Nondet.guard(Option.is_none(goal_dec)); /* -1 for lambda */ /* -1 for tuple */ /* -1 for constructor */ /* -1 for lambda */
  /* No introduction form for a type variable */
  switch (Type_ctx.peel_type(gamma)) {
  | Some((binding, gamma_rest)) =>
    Nondet.union([
      gen({
        sigma,
        term_kind: I,
        term_size,
        rel_binding: Some(binding),
        goal: (gamma_rest, goal_type, goal_dec),
      }),
      gen({
        sigma,
        term_kind: I,
        term_size,
        rel_binding: None,
        goal: (gamma_rest, goal_type, goal_dec),
      }),
    ])

  | None =>
    switch (goal_type) {
    | TArr(tau1, tau2) =>
      let f_name = fresh_ident(Type_ctx.empty, function_char);

      let arg_name = fresh_ident(Type_ctx.empty, variable_char);

      let+ body =
        gen({
          sigma,
          term_kind: I,
          term_size: term_size - 1, /* -1 for lambda */
          rel_binding: None,
          goal: (
            Type_ctx.concat_type(
              [
                (arg_name, (tau1, Arg(f_name))),
                (f_name, (goal_type, Rec(f_name))),
              ],
              Type_ctx.empty,
            ),
            tau2,
            None,
          ),
        });
      EFix(Some(f_name), PatParam(PVar(arg_name)), body);

    | TTuple(taus) =>
      let tuple_size = List.length(taus);

      let* partition =
        Nondet.from_list @@
        Int2.partition_permutations(
          ~n=term_size - 1, /* -1 for tuple */
          ~k=tuple_size,
        );
      Nondet.map(es => ETuple(es)) @@
      Nondet.one_of_each @@
      List.map2(
        (tau, n) =>
          gen({
            sigma,
            term_kind: I,
            term_size: n,
            rel_binding: None,
            goal: (Type_ctx.empty, tau, None),
          }),
        taus,
        partition,
      );

    | TData(datatype_name, datatype_args) =>
      let* (ctor_name, arg_type) =
        List.assoc_opt(datatype_name, sigma)
        |> Option2.map(snd >> Nondet.from_list)
        |> Option2.with_default(Nondet.none); /* -1 for constructor */
      let+ arg =
        gen({
          sigma,
          term_kind: I,
          term_size: term_size - 1, /* -1 for constructor */
          rel_binding: None,
          goal: (Type_ctx.empty, arg_type, None),
        });
      ECtor(ctor_name, datatype_args, arg);

    | TForall(a, bound_type) =>
      let+ body =
        gen({
          sigma,
          term_kind: I,
          term_size: term_size - 1, /* -1 for lambda */
          rel_binding: None,
          goal: (Type_ctx.add_poly(a, Type_ctx.empty), bound_type, None),
        });
      EFix(None, TypeParam(a), body);

    | TVar(_) => Nondet.none
    }
  };
}

and rel_gen_i =
    (
      sigma: datatype_ctx,
      term_size: int,
      rel_binding: type_binding,
      (gamma, goal_type, goal_dec) as goal: gen_goal,
    )
    : Nondet.t(exp) => {
  let* _ = Nondet.guard(Option.is_none(goal_dec)); /* -1 for lambda */ /* -1 for tuple */ /* -1 for constructor */ /* -1 for lambda */
  /* All E-forms are I-forms */
  /* No introduction form for a type variable */
  let e_option =
    gen({
      sigma,
      term_kind: E,
      term_size,
      rel_binding: Some(rel_binding),
      goal,
    });

  let i_option =
    switch (goal_type) {
    | TArr(tau1, tau2) =>
      let f_name = fresh_ident(gamma, function_char);

      let arg_name = fresh_ident(gamma, variable_char);

      let+ body =
        gen({
          sigma,
          term_kind: I,
          term_size: term_size - 1, /* -1 for lambda */
          rel_binding: Some(rel_binding),
          goal: (
            Type_ctx.concat_type(
              [
                (arg_name, (tau1, Arg(f_name))),
                (f_name, (goal_type, Rec(f_name))),
              ],
              gamma,
            ),
            tau2,
            None,
          ),
        });
      EFix(Some(f_name), PatParam(PVar(arg_name)), body);

    | TTuple(taus) =>
      let tuple_size = List.length(taus);

      let* partition =
        Nondet.from_list @@
        Int2.partition_permutations(
          ~n=term_size - 1, /* -1 for tuple */
          ~k=tuple_size,
        );
      let* part = parts(tuple_size);
      Nondet.map(es => ETuple(es)) @@
      Nondet.one_of_each @@
      List2.map3(
        (tau, n, tp) =>
          genp_i(sigma, n, tp, rel_binding, (gamma, tau, None)),
        taus,
        partition,
        part,
      );

    | TData(datatype_name, datatype_args) =>
      let* (ctor_name, arg_type) =
        List.assoc_opt(datatype_name, sigma)
        |> Option2.map(snd >> Nondet.from_list)
        |> Option2.with_default(Nondet.none); /* -1 for constructor */
      let+ arg =
        gen({
          sigma,
          term_kind: I,
          term_size: term_size - 1, /* -1 for constructor */
          rel_binding: Some(rel_binding),
          goal: (gamma, arg_type, None),
        });
      ECtor(ctor_name, datatype_args, arg);

    | TForall(a, bound_type) =>
      let+ body =
        gen({
          sigma,
          term_kind: I,
          term_size: term_size - 1, /* -1 for lambda */
          rel_binding: Some(rel_binding),
          goal: (Type_ctx.add_poly(a, gamma), bound_type, None),
        });
      EFix(None, TypeParam(a), body);

    | TVar(_) => Nondet.none
    };

  Nondet.union([e_option, i_option]);
}

and gen = (gen_input: gen_input): Nondet.t(exp) =>
  if (gen_input.term_size <= 0) {
    Nondet.none;
  } else {
    switch (lookup(gen_input)) {
    | Some(solution) => solution

    | None =>
      let {term_size, sigma, goal, _} = gen_input;

      record(gen_input) @@
      Nondet.dedup @@
      (
        switch (gen_input.term_kind, gen_input.rel_binding) {
        | (E, None) => gen_e(sigma, term_size, goal)

        | (E, Some(rb)) => rel_gen_e(sigma, term_size, rb, goal)

        | (I, None) => gen_i(sigma, term_size, goal)

        | (I, Some(rb)) => rel_gen_i(sigma, term_size, rb, goal)
        }
      );
    };
  };

/*******************************************************************************
 * Term generation exports
 */

let clear_cache = _ => Hashtbl.reset(gen_cache);

let up_to_e = (sigma, max_size, goal) =>
  List2.range(~low=1, ~high=max_size)
  |> List.map(term_size =>
       gen({sigma, term_kind: E, rel_binding: None, term_size, goal})
     )
  |> Nondet.union;
