open Util;

/* The standardness routine: whether `!!e` holds by the shape of `e`, so
 * that a side condition `!!e` needs no proof.
 *
 * `!!e` says that `e` has a value whose parts are standard, and that a
 * function is standard when it gives a standard result on every standard
 * argument. The routine answers `Standard(conditions)`: `e` is standard
 * wherever the boolean `conditions` hold. They are domain conditions as
 * `DomainConditions` states them (`b != 0` for a division), so a step
 * that needs `!!e` incurs the same obligations it would for definedness
 * alone. Otherwise it answers `Unknown(reason)`, and `!!e` is left to be
 * proved.
 *
 * Standard:
 *   - a variable the caller says is standard (a quantified binder), or
 *     one bound inside `e` to something standard;
 *   - literals, constructors, and tuples, lists and labels of standard
 *     parts;
 *   - an operator on standard operands of its type, total or with its
 *     domain condition;
 *   - `if`, `let`, and a `case` that covers its scrutinee's type, whose
 *     parts are standard;
 *   - a lambda whose body is standard for every standard argument with no
 *     condition on that argument: `fun x -> 1 / x` isn't standard;
 *   - an application of a standard function to a standard argument. A
 *     lambda applied is judged through its body, its conditions taking
 *     the argument; a structurally recursive function applied, through
 *     its body with its recursive calls standard, by induction on the
 *     argument;
 *   - a primitive builtin that is total, or whose domain condition is
 *     known;
 *   - `forall x -> p` with `p` standard for every standard `x`. Only this
 *     way round: a forall can be standard when its body isn't;
 *   - `!!e` for a standard `e`.
 * Anything else, holes, errors and operands of a type the routine can't
 * see among them, is Unknown.
 *
 * Types come from binders, literals and operators, not from the statics'
 * type at a node: that is the type the node is analysed against, which
 * an operand of unknown type takes without being one. `e` names the
 * program's definitions, as goals do; a definition's value is judged
 * once, closed, where a name for it is met, with the definition's own
 * type for its recursive calls. */

[@deriving (show({with_path: false}), sexp, yojson)]
type verdict =
  | Standard(list(Exp.t))
  | Unknown(string);

/* Primitive builtins that are total on standard arguments of their
 * types. The partial ones with a known domain condition are in
 * `DomainConditions.builtin_conditions`, and any other is Unknown. */
let total_builtins = [
  "abs",
  "abs_float",
  "ceil",
  "floor",
  "round",
  "exp",
  "log",
  "log10",
  "sqrt",
  "sin",
  "cos",
  "tan",
  "asin",
  "acos",
  "atan",
  "monus",
  "float_min",
  "float_max",
  "is_finite",
  "is_infinite",
  "is_nan",
  "string_length",
  "string_trim",
  "string_uppercase",
  "string_lowercase",
  "string_capitalize",
  "string_uncapitalize",
  "fst",
  "snd",
];

let partial_builtins = [
  "int_mod",
  "sint_mod",
  "nat_mod",
  "float_mod",
  "int_of_float",
  "sint_of_float",
];

/* What the walk knows about the names bound inside `e`: each is standard,
 * and some have a known type. */
type scope = {
  bound: list(Var.t),
  types: list((Var.t, Typ.t)),
};

let check =
    (
      ~ctx: Ctx.t,
      ~env: Environment.t(Exp.t),
      ~is_standard: Var.t => bool,
      e: Exp.t,
    )
    : verdict => {
  open Result.Syntax;
  let definitions = Stdlib.Hashtbl.create(8);
  let is_builtin = (x: Var.t): bool =>
    Option.is_some(Environment.lookup(Builtins.env_init, x));
  let known = (t: Typ.t): option(Typ.t) =>
    switch (Typ.term_of(t)) {
    | Unknown(_) => None
    | _ => Some(t)
    };
  let ctx_type = (x: Var.t): option(Typ.t) =>
    Ctx.lookup_var(ctx, x)
    |> Option.map(~f=(entry: Ctx.var_entry) =>
         Typ.normalize(ctx, entry.typ)
       )
    |> Option.bind(~f=known);
  /* The type of `e`, where its binders, literals and operators show it. */
  let rec type_of = (scope: scope, e: Exp.t): option(Typ.t) =>
    switch (Exp.term_of(e)) {
    | Parens(e)
    | Asc(e, _)
    | Closure(_, e) => type_of(scope, e)
    | Atom(a) => Some(Typ.temp(Atom(Atom.cls_of_t(a))))
    | Var(x) =>
      switch (List.Assoc.find(scope.types, x, ~equal=String.equal)) {
      | Some(_) as t => t
      | None when List.mem(scope.bound, x, ~equal=String.equal) => None
      | None => ctx_type(x)
      }
    | BinOp(Int(Plus | Minus | Times | Power | Divide), _, _) =>
      Some(Typ.temp(Atom(Int)))
    | BinOp(SInt(Plus | Minus | Times | Power | Divide), _, _) =>
      Some(Typ.temp(Atom(SInt)))
    | BinOp(Nat(Plus | Minus | Times | Power | Divide), _, _) =>
      Some(Typ.temp(Atom(Nat)))
    | BinOp(Float(Plus | Minus | Times | Power | Divide), _, _) =>
      Some(Typ.temp(Atom(Float)))
    | BinOp(String(Concat), _, _) => Some(Typ.temp(Atom(String)))
    | BinOp(
        Int(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual) |
        SInt(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual) |
        Nat(LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual) |
        Float(
          LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual |
          Equals |
          NotEquals,
        ) |
        Bool(_) |
        Poly(_),
        _,
        _,
      )
    | UnOp(Bool(Not), _)
    | Standard(_)
    | Forall(_) => Some(Typ.temp(Atom(Bool)))
    | UnOp(Int(Minus), _) => Some(Typ.temp(Atom(Int)))
    | UnOp(SInt(Minus), _) => Some(Typ.temp(Atom(SInt)))
    | UnOp(Nat(Minus), _) => Some(Typ.temp(Atom(Nat)))
    | UnOp(Float(Minus), _) => Some(Typ.temp(Atom(Float)))
    | If(_, a, _) => type_of(scope, a)
    | Tuple(es) =>
      List.map(~f=type_of(scope), es)
      |> OptUtil.sequence
      |> Option.map(~f=ts => Typ.temp(Prod(ts)))
    | ListLit([e, ..._])
    | Cons(e, _) =>
      type_of(scope, e) |> Option.map(~f=t => Typ.temp(List(t)))
    | Ap(_, f, _) =>
      switch (Option.map(~f=Typ.term_of, type_of(scope, f))) {
      | Some(Arrow(_, cod)) => known(cod)
      | _ => None
      }
    | _ => None
    };
  let is_atom = (cls: Atom.cls, scope, e) =>
    switch (Option.map(~f=Typ.term_of, type_of(scope, e))) {
    | Some(Atom(c)) => Atom.equal_cls(c, cls)
    | _ => false
    };
  let is_list = (scope, e) =>
    switch (Option.map(~f=Typ.term_of, type_of(scope, e))) {
    | Some(List(_)) => true
    | _ => false
    };
  /* `==` errors on functions, and a type it can't see might hold one. */
  let comparable = (scope, e) =>
    switch (type_of(scope, e)) {
    | Some(t) => !Typ.has_fun(t) && !Typ.contains_unknown(t)
    | None => false
    };
  let within = (scope: scope, names: list(Var.t), types) => {
    bound: names @ scope.bound,
    types:
      types
      @ List.filter(
          ~f=((x, _)) => !List.mem(names, x, ~equal=String.equal),
          scope.types,
        ),
  };
  /* The types a pattern's variables get from matching a value of type
   * `t`, where the routine can see them. */
  let rec pattern_types = (p: Pat.t, t: option(Typ.t)) =>
    switch (Pat.term_of(p), t) {
    | (Var(x), Some(t)) => [(x, t)]
    | (Parens(p) | Standard(p, _), _) => pattern_types(p, t)
    | (Asc(p, a), _) => pattern_types(p, known(Typ.normalize(ctx, a)))
    | (Cons(h, rest), Some(t)) =>
      switch (Typ.term_of(t)) {
      | List(elem) =>
        pattern_types(h, Some(elem)) @ pattern_types(rest, Some(t))
      | _ => []
      }
    | (ListLit(ps), Some(t)) =>
      switch (Typ.term_of(t)) {
      | List(elem) =>
        List.concat_map(~f=p => pattern_types(p, Some(elem)), ps)
      | _ => []
      }
    | (Tuple(ps), Some(t)) =>
      switch (Typ.term_of(t)) {
      | Prod(ts) when List.length(ps) == List.length(ts) =>
        List.concat(
          List.map2_exn(~f=(p, t) => pattern_types(p, Some(t)), ps, ts),
        )
      | _ => []
      }
    | _ => []
    };
  let rec irrefutable = (p: Pat.t) =>
    switch (Pat.term_of(p)) {
    | Var(_)
    | Wild => true
    | Parens(p)
    | Asc(p, _)
    | Standard(p, _) => irrefutable(p)
    | Tuple(ps) => List.for_all(~f=irrefutable, ps)
    | _ => false
    };
  /* Whether the rules' patterns cover every value of the scrutinee's
   * type: an irrefutable pattern, or each of a list's, Bool's or sum's
   * forms with irrefutable parts. */
  let covers = (scope, scrut, pats: list(Pat.t)) => {
    let rec head = (p: Pat.t) =>
      switch (Pat.term_of(p)) {
      | Parens(p)
      | Asc(p, _) => head(p)
      | _ => p
      };
    let has = (form: Pat.term => bool) =>
      List.exists(~f=p => form(Pat.term_of(head(p))), pats);
    List.exists(~f=irrefutable, pats)
    || (
      switch (Option.map(~f=Typ.term_of, type_of(scope, scrut))) {
      | Some(List(_)) =>
        has(
          fun
          | ListLit([]) => true
          | _ => false,
        )
        && has(
             fun
             | Cons(h, t) => irrefutable(h) && irrefutable(t)
             | _ => false,
           )
      | Some(Atom(Bool)) =>
        has(
          fun
          | Atom(Bool(true)) => true
          | _ => false,
        )
        && has(
             fun
             | Atom(Bool(false)) => true
             | _ => false,
           )
      | Some(Sum(variants)) =>
        List.for_all(
          ~f=
            fun
            | ConstructorMap.Variant(c, _, None) =>
              has(
                fun
                | Constructor(c', _) => String.equal(c, c')
                | _ => false,
              )
            | ConstructorMap.Variant(c, _, Some(_)) =>
              has(
                fun
                | Ap(ctr, arg) =>
                  switch (Pat.term_of(head(ctr))) {
                  | Constructor(c', _) =>
                    String.equal(c, c') && irrefutable(arg)
                  | _ => false
                  }
                | _ => false,
              )
            | ConstructorMap.BadEntry(_) => false,
          variants,
        )
      | _ => false
      }
    );
  };
  let mentions = (names, condition) => Exp.occurs_free_any(names, condition);
  /* Conditions under a binder of `names` hold only for some values of
   * them, so they can't be lifted out. */
  let lift = (names, conditions) =>
    List.exists(~f=mentions(names), conditions)
      ? Error(
          "a domain condition here depends on "
          ++ String.concat(~sep=", ", names)
          ++ ", so it doesn't hold for every value",
        )
      : Ok(conditions);
  /* The conditions of a lambda's body, with its parameter `x` given the
   * argument `arg`. */
  let instantiate = (x, arg, conditions) =>
    List.map(
      ~f=
        c =>
          Substitution.in_exp(
            Substitution.env_for([(x, arg)], ~into=c),
            c,
          ),
      conditions,
    );
  /* Whether `arg` is known to be of the domain of a function of type
   * `f_type`. A standard function is standard on standard arguments of its
   * domain, and nothing is known on others. An unknown part of the domain
   * takes any argument. */
  let fits = (scope, f_type: option(Typ.t), arg: Exp.t): bool => {
    let rec accepts = (dom: Typ.t, t: Typ.t) =>
      switch (Typ.term_of(dom), Typ.term_of(t)) {
      | (Unknown(_), _) => true
      | (List(dom), List(t)) => accepts(dom, t)
      | (Prod(doms), Prod(ts)) =>
        List.length(doms) == List.length(ts)
        && List.for_all2_exn(~f=accepts, doms, ts)
      | _ => Typ.fast_equal(dom, t)
      };
    switch (Option.map(~f=Typ.term_of, f_type), type_of(scope, arg)) {
    | (Some(Arrow(dom, _)), Some(t)) =>
      accepts(Typ.normalize(ctx, dom), t)
    | _ => false
    };
  };
  let outside_domain = (name: string) =>
    Error("the argument to " ++ name ++ " isn't known to be of its domain");
  let rec go = (scope: scope, e: Exp.t): result(list(Exp.t), string) => {
    let all = es =>
      List.fold_left(
        ~f=
          (acc, e) => {
            let* acc = acc;
            let+ cs = go(scope, e);
            acc @ cs;
          },
        ~init=Ok([]),
        es,
      );
    let operands = (cls, a, b) =>
      is_atom(cls, scope, a) && is_atom(cls, scope, b)
        ? all([a, b])
        : Error("an operand's type isn't known to be " ++ Atom.show_cls(cls));
    switch (Exp.term_of(e)) {
    | Parens(e) => go(scope, e)
    /* A cast fails on a value not of its type, unless it is to `?`. */
    | Asc(inner, t) =>
      switch (known(Typ.normalize(ctx, t)), type_of(scope, inner)) {
      | (None, _) => go(scope, inner)
      | (Some(t), Some(t')) when Typ.fast_equal(t, Typ.normalize(ctx, t')) =>
        go(scope, inner)
      | (Some(_), _) => Error("a cast from a type that isn't known")
      }
    | Var(x)
        when List.mem(scope.bound, x, ~equal=String.equal) || is_standard(x) =>
      Ok([])
    | Var(x) when Definitions.is_definition(env, x) => definition(x)
    | Var(x) when is_builtin(x) => go(scope, Exp.fresh(BuiltinFun(x)))
    | Var(x) => Error("`" ++ x ++ "` isn't known to be standard")
    | Atom(_)
    | Label(_)
    | ExplicitNonlabel
    | Constructor(_)
    | ListLit([]) => Ok([])
    | ListLit(es)
    | Tuple(es) => all(es)
    | TupLabel(_, e)
    | Standard(e) => go(scope, e)
    | Cons(h, t) =>
      is_list(scope, t)
        ? all([h, t]) : Error("`::` onto something not a list")
    | ListConcat(a, b) =>
      is_list(scope, a) && is_list(scope, b)
        ? all([a, b]) : Error("`@` of something not a list")
    | BinOp(Int(Divide), a, b) =>
      let+ cs = operands(Int, a, b);
      cs @ [DomainConditions.neq_zero(DomainConditions.int_zero(), b)];
    | BinOp(SInt(Divide), a, b) =>
      let+ cs = operands(SInt, a, b);
      cs @ [DomainConditions.neq_zero(DomainConditions.sint_zero(), b)];
    | BinOp(Nat(Divide), a, b) =>
      let+ cs = operands(Nat, a, b);
      cs @ [DomainConditions.neq_zero(DomainConditions.nat_zero(), b)];
    | BinOp(Int(Power), a, b) =>
      let+ cs = operands(Int, a, b);
      cs @ [DomainConditions.geq_zero_int(b)];
    | BinOp(SInt(Power), a, b) =>
      let+ cs = operands(SInt, a, b);
      cs
      @ [
        Exp.fresh(
          BinOp(SInt(GreaterThanOrEqual), b, DomainConditions.sint_zero()),
        ),
      ];
    /* A natural number below zero is an error, and its condition isn't
     * stated. */
    | BinOp(Nat(Minus), _, _)
    | UnOp(Nat(Minus), _) =>
      Error("natural subtraction can leave the naturals")
    | BinOp(
        Int(
          Plus | Minus | Times | LessThan | LessThanOrEqual | GreaterThan |
          GreaterThanOrEqual,
        ),
        a,
        b,
      ) =>
      operands(Int, a, b)
    | BinOp(
        SInt(
          Plus | Minus | Times | LessThan | LessThanOrEqual | GreaterThan |
          GreaterThanOrEqual,
        ),
        a,
        b,
      ) =>
      operands(SInt, a, b)
    | BinOp(
        Nat(
          Plus | Times | Power | LessThan | LessThanOrEqual | GreaterThan |
          GreaterThanOrEqual,
        ),
        a,
        b,
      ) =>
      operands(Nat, a, b)
    | BinOp(Float(_), a, b) => operands(Float, a, b)
    | BinOp(Bool(_), a, b) => operands(Bool, a, b)
    | BinOp(String(Concat), a, b) => operands(String, a, b)
    | BinOp(Poly(_), a, b) =>
      comparable(scope, a) && comparable(scope, b)
        ? all([a, b]) : Error("`==` on something that may be a function")
    | UnOp(Int(Minus), a) =>
      is_atom(Int, scope, a) ? go(scope, a) : Error("`-` on a non-Int")
    | UnOp(SInt(Minus), a) =>
      is_atom(SInt, scope, a) ? go(scope, a) : Error("`-` on a non-SInt")
    | UnOp(Float(Minus), a) =>
      is_atom(Float, scope, a) ? go(scope, a) : Error("`-` on a non-Float")
    | UnOp(Bool(Not), a) =>
      is_atom(Bool, scope, a) ? go(scope, a) : Error("`!` on a non-Bool")
    | If(c, a, b) =>
      is_atom(Bool, scope, c)
        ? all([c, a, b]) : Error("an `if` on a non-Bool")
    | Match(scrut, rules) =>
      if (!covers(scope, scrut, List.map(~f=fst, rules))) {
        Error("a `case` that may not cover its scrutinee");
      } else {
        let* cs = go(scope, scrut);
        let scrut_ty = type_of(scope, scrut);
        List.fold_left(
          ~f=
            (acc, (p, rhs)) => {
              let* acc = acc;
              let names = Pat.bound_vars(p);
              let* rhs_cs =
                go(within(scope, names, pattern_types(p, scrut_ty)), rhs);
              let+ rhs_cs = lift(names, rhs_cs);
              acc @ rhs_cs;
            },
          ~init=Ok(cs),
          rules,
        );
      }
    | Let(p, def, body) =>
      switch (ProofSyntax.pat_to_name(p)) {
      | Some(x) =>
        /* A declared type also types a recursive definition's own calls. */
        let declared = pattern_types(p, None);
        let* def_cs =
          go(
            {
              ...scope,
              types: declared @ scope.types,
            },
            def,
          );
        let types =
          switch (declared, type_of(scope, def)) {
          | ([], Some(t)) => [(x, t)]
          | (declared, _) => declared
          };
        let+ body_cs = go(within(scope, [x], types), body);
        def_cs @ instantiate(x, def, body_cs);
      | None => Error("a `let` that destructures")
      }
    | Fun(p, body, t, _) =>
      let names = Pat.bound_vars(p);
      let types =
        switch (ProofSyntax.pat_to_name(p), Option.bind(t, ~f=known)) {
        | (Some(x), Some(t)) => [(x, t)]
        | _ => pattern_types(p, Option.bind(t, ~f=known))
        };
      let* cs = go(within(scope, names, types), body);
      lift(names, cs);
    | FixF(p, body, _) =>
      switch (StructuralRecursion.check(e)) {
      | Ok () =>
        let names = Pat.bound_vars(p);
        let self_types =
          List.filter(
            ~f=((x, _)) => List.mem(names, x, ~equal=String.equal),
            scope.types,
          )
          @ pattern_types(p, None);
        let* cs = go(within(scope, names, self_types), body);
        lift(names, cs);
      | Error(reason) => Error("a recursive function: " ++ reason)
      }
    | Forall(p, body) =>
      let names = Pat.bound_vars(p);
      let* cs = go(within(scope, names, pattern_types(p, None)), body);
      lift(names, cs);
    | Ap(Forward, f, arg) =>
      let* arg_cs = go(scope, arg);
      let+ f_cs = apply(scope, f, arg);
      arg_cs @ f_cs;
    | BuiltinFun(name) =>
      List.mem(total_builtins, name, ~equal=String.equal)
        ? Ok([]) : Error("the builtin `" ++ name ++ "` may be partial")
    | Closure(_)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | DynamicErrorHole(_)
    | Deferral(_)
    | Undefined
    | DrvQuote(_)
    | TypFun(_)
    | Dot(_)
    | LivelitName(_)
    | Theorem(_)
    | TyAlias(_)
    | Use(_)
    | Ap(Reverse, _, _)
    | TypAp(_)
    | DeferredAp(_)
    | Seq(_)
    | Test(_)
    | HintedTest(_)
    | Filter(_)
    | Projector(_)
    | TupleExtension(_)
    | Module(_)
    | ModuleExp(_) => Error("this form isn't judged standard by its shape")
    };
  }
  /* `f` applied to a standard `arg`. */
  and apply = (scope, f: Exp.t, arg: Exp.t): result(list(Exp.t), string) =>
    switch (Exp.term_of(f)) {
    | Parens(f)
    | Asc(f, _) => apply(scope, f, arg)
    | Fun(p, body, t, _) =>
      switch (ProofSyntax.pat_to_name(p)) {
      | Some(x) =>
        let types =
          switch (Option.bind(t, ~f=known)) {
          | Some(t) => [(x, t)]
          | None =>
            switch (type_of(scope, arg)) {
            | Some(t) => [(x, t)]
            | None => []
            }
          };
        let+ cs = go(within(scope, [x], types), body);
        instantiate(x, arg, cs);
      | None => go(scope, f)
      }
    /* A partial builtin's condition has to be one the scan can state,
     * which needs a tuple argument written out. */
    | BuiltinFun(name) =>
      let conditions = DomainConditions.builtin_conditions(name, arg);
      if (!fits(scope, ctx_type(name), arg)) {
        outside_domain(name);
      } else if (List.mem(total_builtins, name, ~equal=String.equal)) {
        Ok([]);
      } else if (List.mem(partial_builtins, name, ~equal=String.equal)
                 && !List.is_empty(conditions)) {
        Ok(conditions);
      } else {
        Error("the builtin `" ++ name ++ "` may fail here");
      };
    | Var(x)
        when List.mem(scope.bound, x, ~equal=String.equal) || is_standard(x) =>
      fits(scope, type_of(scope, f), arg)
        ? go(scope, f) : outside_domain(x)
    /* A lambda's conditions take the argument; any other definition is
     * judged as a whole. */
    | Var(x) when Definitions.is_definition(env, x) =>
      switch (Definitions.unfold(env, x)) {
      | Some({term: Fun(_), _} as value) => apply(scope, value, arg)
      | Some(_)
      | None =>
        fits(scope, ctx_type(x), arg) ? definition(x) : outside_domain(x)
      }
    | Var(x) when is_builtin(x) =>
      apply(scope, Exp.fresh(BuiltinFun(x)), arg)
    | Constructor(_) => Ok([])
    | _ =>
      fits(scope, type_of(scope, f), arg)
        ? go(scope, f) : outside_domain("the function")
    }
  /* A definition's value, closed, with the definition's type for the
   * name a recursive one calls itself by. */
  and definition = (x: Var.t): result(list(Exp.t), string) =>
    switch (Stdlib.Hashtbl.find_opt(definitions, x)) {
    | Some(verdict) => verdict
    | None =>
      let verdict =
        switch (Definitions.unfold(env, x)) {
        | Some(value) =>
          let self_types =
            switch (Exp.term_of(value), ctx_type(x)) {
            | (FixF(p, _, _), Some(t)) =>
              List.map(~f=name => (name, t), Pat.bound_vars(p))
            | _ => []
            };
          go(
            {
              bound: [],
              types: self_types,
            },
            value,
          );
        | None => Error("`" ++ x ++ "` has no definition")
        };
      Stdlib.Hashtbl.add(definitions, x, verdict);
      verdict;
    };
  switch (
    go(
      {
        bound: [],
        types: [],
      },
      e,
    )
  ) {
  | Ok(conditions) => Standard(DomainConditions.dedup(conditions))
  | Error(reason) => Unknown(reason)
  };
};
