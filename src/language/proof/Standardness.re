open Util;

/* The standardness routine: whether `!!e` holds by the shape of `e`, so
 * that a side condition `!!e` needs no proof.
 *
 * `!!e` says that `e` has a value whose parts are standard, and that a
 * function is standard when it gives a standard result on every standard
 * argument. The routine answers `Standard(conditions)`: `e` is standard
 * wherever the boolean `conditions` hold. They are the partial
 * primitives' conditions, written as `DomainConditions` writes them
 * (`b != 0` for a division), so a step that needs `!!e` incurs the
 * obligations it would for definedness alone. Otherwise it answers
 * `Unknown(reason)`, and `!!e` is left to be proved.
 *
 * Standard:
 *   - a variable the caller says is standard (a quantified binder), or
 *     one bound inside `e` to something standard;
 *   - literals, constructors, and tuples, lists and labels of standard
 *     parts;
 *   - a total operator on standard operands of its type;
 *   - a partial primitive, `a / b` or `int_mod(a, b)`, through its rule in
 *     `Axioms.standard_primitives`, `!!(a / b) == (b != 0)`: the rule a
 *     proof cites, its condition left as the routine's;
 *   - `if`, `let`, and a `case` that covers its scrutinee's type, whose
 *     parts are standard. A branch's conditions are needed only under
 *     its guard: those the guard implies are dropped (`Bounds`), so
 *     `if b == 0 then 0 else a / b` needs nothing, and the rest are
 *     conditional on it;
 *   - a lambda whose body is standard for every standard argument with no
 *     condition on that argument: `fun x -> 1 / x` isn't standard;
 *   - an application of a standard function to a standard argument. A
 *     lambda applied is judged through its body, its conditions taking
 *     the argument; a structurally recursive function applied, through
 *     its body with its recursive calls standard, by induction on the
 *     argument;
 *   - a total primitive builtin;
 *   - `forall x -> p` with `p` standard for every standard `x`. Only this
 *     way round: a forall can be standard when its body isn't;
 *   - `!!e` for a standard `e`.
 * Anything else, holes, errors and operands of a type the routine can't
 * see among them, is Unknown.
 *
 * Types come from binders, ascriptions, literals, operators and the
 * constructor types elaboration leaves in values, not from the statics'
 * type at a node: that is the type the node is analysed against, which
 * an operand of unknown type takes without being one. `e` names the
 * program's definitions, as goals do; a definition's value is judged
 * once, closed, where a name for it is met, and a recursive one at the
 * type of the argument it is applied to. */

[@deriving (show({with_path: false}), sexp, yojson)]
type verdict =
  | Standard(list(Exp.t))
  | Unknown(string);

/* Primitive builtins that are total on standard arguments of their
 * types. A partial one is standard through its rule in
 * `Axioms.standard_primitives`, if it has one. */
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

/* What the walk knows about the names bound inside `e`: each is standard,
 * and some have a known type. `guards` is what the guards of the
 * enclosing branches say about numbers. */
type scope = {
  bound: list(Var.t),
  types: list((Var.t, Typ.t)),
  guards: Bounds.t,
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
  let definition_types = Stdlib.Hashtbl.create(8);
  let is_builtin = (x: Var.t): bool =>
    Option.is_some(Environment.lookup(Builtins.env_init, x));
  let known = (t: Typ.t): option(Typ.t) =>
    switch (Typ.term_of(t)) {
    | Unknown(_) => None
    | _ => Some(t)
    };
  /* A definition's value, closed. Evaluation leaves a recursive function
   * unrolled once: a lambda whose own calls are its fixpoint, inlined.
   * That fixpoint is the definition. The statics name a recursive
   * function `f+`, and its fixpoint's lambda has the same name. */
  let definition_value = (x: Var.t): option(Exp.t) =>
    Definitions.unfold(env, x)
    |> Option.map(~f=(value: Exp.t) =>
         switch (Exp.term_of(value)) {
         | Fun(_, body, _, Some(name))
             when
               String.length(name) > 0
               && Char.equal(name.[String.length(name) - 1], '+') =>
           let rec lambda_name = (e: Exp.t) =>
             switch (Exp.term_of(e)) {
             | Parens(e)
             | Asc(e, _) => lambda_name(e)
             | Fun(_, _, _, name) => name
             | _ => None
             };
           let found = ref(None);
           let _: Exp.t =
             Exp.map_term(
               ~f_exp=
                 (continue, e) =>
                   switch (Exp.term_of(e)) {
                   | FixF(_, f, _)
                       when
                         Option.is_none(found^)
                         && Option.equal(
                              String.equal,
                              lambda_name(f),
                              Some(name),
                            ) =>
                     found := Some(e);
                     e;
                   | _ => continue(e)
                   },
               body,
             );
           Option.value(~default=value, found^);
         | _ => value
         }
       );
  let ctx_type = (x: Var.t): option(Typ.t) =>
    Ctx.lookup_var(ctx, x)
    |> Option.map(~f=(entry: Ctx.var_entry) =>
         Typ.normalize(ctx, entry.typ)
       )
    |> Option.bind(~f=known);
  /* A builtin's type, which a proof's context doesn't hold. */
  let builtin_type = (name: string): option(Typ.t) =>
    switch (ctx_type(name)) {
    | Some(_) as t => t
    | None =>
      let builtins = Builtins.ctx_init(None);
      Ctx.lookup_var(builtins, name)
      |> Option.map(~f=(entry: Ctx.var_entry) =>
           Typ.normalize(builtins, entry.typ)
         )
      |> Option.bind(~f=known);
    };
  let within = (scope: scope, names: list(Var.t), types) => {
    bound: names @ scope.bound,
    types:
      types
      @ List.filter(
          ~f=((x, _)) => !List.mem(names, x, ~equal=String.equal),
          scope.types,
        ),
    guards: Bounds.forget(scope.guards, names),
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
    | (Tuple(ps), None) =>
      List.concat_map(~f=p => pattern_types(p, None), ps)
    | (Ap(ctr, arg), Some(t)) =>
      switch (Pat.term_of(ctr)) {
      | Constructor(c, _) =>
        switch (
          Option.bind(Typ.get_sum_constructors(ctx, t), ~f=ctrs =>
            ConstructorMap.get_entry(c, ctrs)
          )
        ) {
        | Some(Some(payload)) => pattern_types(arg, Some(payload))
        | Some(None)
        | None => []
        }
      | _ => []
      }
    | _ => []
    };
  /* The type a pattern's ascriptions give the values it matches. */
  let rec pattern_type = (p: Pat.t): option(Typ.t) =>
    switch (Pat.term_of(p)) {
    | Parens(p)
    | Standard(p, _) => pattern_type(p)
    | Asc(_, a) => known(Typ.normalize(ctx, a))
    | Tuple(ps) =>
      ps
      |> List.map(~f=pattern_type)
      |> OptUtil.sequence
      |> Option.map(~f=ts => Typ.temp(Prod(ts)))
    | _ => None
    };
  /* The type a lambda's parameter `p` declares: its ascriptions', which
   * elaboration writes out in full, or else the lambda's own parameter
   * type `t`, which can name an alias. */
  let domain = (p: Pat.t, t: option(Typ.t)) =>
    switch (pattern_type(p)) {
    | Some(_) as d => d
    | None => Option.bind(t, ~f=known)
    };
  let rec is_nil = (e: Exp.t) =>
    switch (Exp.term_of(e)) {
    | Parens(e) => is_nil(e)
    | ListLit([]) => true
    | _ => false
    };
  /* The type all the branches `arms` have, if they are known and agree.
   * Branches of a `case` or `if` can differ in a type the statics leave
   * unknown. A `[]` branch agrees with any list type. */
  let agreed = (arms: list((Exp.t, option(Typ.t)))): option(Typ.t) => {
    let typed = List.filter(~f=((e, _)) => !is_nil(e), arms);
    switch (OptUtil.sequence(List.map(~f=snd, typed))) {
    | Some([t, ...rest]) when List.for_all(~f=Typ.fast_equal(t), rest) =>
      switch (Typ.term_of(t)) {
      | List(_) => Some(t)
      | _ when List.length(typed) == List.length(arms) => Some(t)
      | _ => None
      }
    | _ => None
    };
  };
  let rec lambda = (f: Exp.t) =>
    switch (Exp.term_of(f)) {
    | Parens(f)
    | Asc(f, _) => lambda(f)
    | Fun(param, body, t, name) => Some((param, body, t, name))
    | _ => None
    };
  let empty = {
    bound: [],
    types: [],
    guards: Bounds.empty,
  };
  /* The scope of a recursive function's body, applied to an argument of
   * type `at`, with result type `cod`. */
  let fix_scope = (~at, ~cod, self: Pat.t, param: Pat.t) => {
    let unknown = Typ.temp(Unknown(Internal));
    let self_type =
      Typ.temp(
        Arrow(
          Option.value(~default=unknown, at),
          Option.value(~default=unknown, cod),
        ),
      );
    within(
      empty,
      Pat.bound_vars(self) @ Pat.bound_vars(param),
      List.map(~f=name => (name, self_type), Pat.bound_vars(self))
      @ pattern_types(param, at),
    );
  };
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
      | None when Definitions.is_definition(env, x) => definition_type(x)
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
    | If(_, a, b) =>
      agreed([(a, type_of(scope, a)), (b, type_of(scope, b))])
    | Match(scrut, rules) => agreed(arm_types(scope, scrut, rules))
    | Constructor(_, Some(Some(t))) => known(t)
    | BuiltinFun(name) => builtin_type(name)
    | Fun(p, body, t, _) =>
      switch (domain(p, t)) {
      | Some(dom) =>
        type_of(
          within(scope, Pat.bound_vars(p), pattern_types(p, Some(dom))),
          body,
        )
        |> Option.map(~f=cod => Typ.temp(Arrow(dom, cod)))
      | None => None
      }
    | FixF(self, f, _) =>
      switch (lambda(f)) {
      | Some((param, body, t, _)) =>
        switch (domain(param, t)) {
        | Some(dom) =>
          fix_result(~declared=None, ~at=Some(dom), self, param, body)
          |> Option.map(~f=cod => Typ.temp(Arrow(dom, cod)))
        | None => None
        }
      | None => None
      }
    | ListLit([]) => Some(Typ.temp(List(Typ.temp(Unknown(Internal)))))
    | ListConcat(a, b) =>
      switch (type_of(scope, a), type_of(scope, b)) {
      | (Some(t), Some(t')) => Typ.fast_equal(t, t') ? Some(t) : None
      | (Some(t), None)
      | (None, Some(t)) =>
        switch (Typ.term_of(t)) {
        | List(_) => Some(t)
        | _ => None
        }
      | (None, None) => None
      }
    | Tuple(es) =>
      List.map(~f=type_of(scope), es)
      |> OptUtil.sequence
      |> Option.map(~f=ts => Typ.temp(Prod(ts)))
    | ListLit([e, ..._])
    | Cons(e, _) =>
      type_of(scope, e) |> Option.map(~f=t => Typ.temp(List(t)))
    | Ap(_, f, arg) =>
      /* A definition applied, by its value. */
      let f =
        switch (Exp.term_of(f)) {
        | Var(x)
            when
              !List.mem(scope.bound, x, ~equal=String.equal)
              && !List.Assoc.mem(scope.types, x, ~equal=String.equal)
              && Definitions.is_definition(env, x) =>
          Option.value(~default=f, definition_value(x))
        | _ => f
        };
      switch (Exp.term_of(f)) {
      | FixF(self, f, _) =>
        switch (lambda(f)) {
        | Some((param, body, _, _)) =>
          fix_result(
            ~declared=None,
            ~at=type_of(scope, arg),
            self,
            param,
            body,
          )
        | None => None
        }
      | _ =>
        switch (Option.map(~f=Typ.term_of, type_of(scope, f)), lambda(f)) {
        | (Some(Arrow(_, cod)), _) => known(cod)
        /* A lambda's result where its parameter has the argument's type. */
        | (_, Some((p, body, _, _))) =>
          switch (ProofSyntax.pat_to_name(p), type_of(scope, arg)) {
          | (Some(x), Some(t)) =>
            type_of(within(scope, [x], [(x, t)]), body)
          | _ => None
          }
        | _ => None
        }
      };
    | _ => None
    }
  /* The result type of a recursive function applied to an argument of
   * type `at`: `declared`'s, or else the one its base case gives, if
   * every branch then gives it. */
  and fix_result = (~declared, ~at, self, param, body): option(Typ.t) =>
    switch (Option.map(~f=Typ.term_of, declared)) {
    | Some(Arrow(_, cod)) when Option.is_some(known(cod)) => Some(cod)
    | _ =>
      let first_known = (scope, e: Exp.t) => {
        let typed = arms =>
          List.find_map(~f=((e, t)) => is_nil(e) ? None : t, arms);
        switch (Exp.term_of(e)) {
        | Match(scrut, rules) => typed(arm_types(scope, scrut, rules))
        | If(_, a, b) =>
          typed([(a, type_of(scope, a)), (b, type_of(scope, b))])
        | _ => type_of(scope, e)
        };
      };
      switch (first_known(fix_scope(~at, ~cod=None, self, param), body)) {
      | Some(c) =>
        switch (type_of(fix_scope(~at, ~cod=Some(c), self, param), body)) {
        | Some(c') when Typ.fast_equal(c, c') => Some(c)
        | _ => None
        }
      | None => None
      };
    }
  /* A definition's type, by its value. */
  and definition_type = (x: Var.t): option(Typ.t) =>
    switch (Stdlib.Hashtbl.find_opt(definition_types, x)) {
    | Some(t) => t
    | None =>
      Stdlib.Hashtbl.add(definition_types, x, None);
      let t =
        Option.bind(definition_value(x), ~f=value => type_of(empty, value));
      Stdlib.Hashtbl.replace(definition_types, x, t);
      t;
    }
  and arm_types = (scope, scrut, rules) => {
    let scrut_ty = type_of(scope, scrut);
    List.map(
      ~f=
        ((p, rhs)) =>
          (
            rhs,
            type_of(
              within(scope, Pat.bound_vars(p), pattern_types(p, scrut_ty)),
              rhs,
            ),
          ),
      rules,
    );
  };
  /* Whether every value of type `t` is one of type `dom`, where an
   * unknown part of `dom` takes anything. */
  let rec accepts = (dom: Typ.t, t: Typ.t) =>
    switch (Typ.term_of(dom), Typ.term_of(t)) {
    | (Unknown(_), _) => true
    | (List(dom), List(t)) => accepts(dom, t)
    | (Prod(doms), Prod(ts)) =>
      List.length(doms) == List.length(ts)
      && List.for_all2_exn(~f=accepts, doms, ts)
    | _ => Typ.fast_equal(dom, t)
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
  /* What `p` matches, as the statics' coverage check reads a pattern;
   * None for a form the routine doesn't read. */
  let rec constraint_of = (p: Pat.t): option(Coverage.Constraint.t) => {
    open OptUtil.Syntax;
    let all = ps => ps |> List.map(~f=constraint_of) |> OptUtil.sequence;
    switch (Pat.term_of(p)) {
    | Var(_)
    | Wild => Some(Truth)
    | Parens(p)
    | Asc(p, _)
    | Standard(p, _)
    | Projector(_, p) => constraint_of(p)
    | Atom(Int(n) | Nat(n)) => Some(BigInt(n))
    | Atom(SInt(n)) => Some(SInt(n))
    | Atom(Float(f)) => Some(Float(f))
    | Atom(Bool(b)) =>
      Some(b ? Coverage.Constraint.true_ : Coverage.Constraint.false_)
    | Atom(String(s)) => Some(String(s))
    | Constructor(c, _) => Some(Ap(c, None))
    | Ap(ctr, arg) =>
      switch (constraint_of(ctr)) {
      | Some(Ap(c, None)) =>
        let+ arg = constraint_of(arg);
        Coverage.Constraint.Ap(c, Some(arg));
      | _ => None
      }
    | Tuple(ps) =>
      let+ cs = all(ps);
      Coverage.Constraint.Tuple(cs);
    | ListLit(ps) =>
      let+ cs = all(ps);
      List.fold_right(
        cs,
        ~f=Coverage.Constraint.cons,
        ~init=Coverage.Constraint.nil,
      );
    | Cons(h, t) =>
      let* h = constraint_of(h);
      let+ t = constraint_of(t);
      Coverage.Constraint.cons(h, t);
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | ExplicitNonlabel
    | Label(_)
    | TupLabel(_) => None
    };
  };
  /* Whether the rules' patterns cover every value of the scrutinee's
   * type, by the statics' coverage check. Unknown parts of the type
   * aren't taken as covered. */
  let covers = (scope, scrut, pats: list(Pat.t)) =>
    List.exists(~f=irrefutable, pats)
    || (
      switch (
        type_of(scope, scrut),
        pats |> List.map(~f=constraint_of) |> OptUtil.sequence,
      ) {
      | (Some(t), Some(constraints)) =>
        Poly.equal(
          Coverage.check(~strict_unknown=true, constraints, t).exhaustiveness,
          Exhaustive,
        )
      | _ => false
      }
    );
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
    switch (Option.map(~f=Typ.term_of, f_type), type_of(scope, arg)) {
    | (Some(Arrow(dom, _)), _) when is_nil(arg) =>
      switch (Typ.term_of(Typ.normalize(ctx, dom))) {
      | List(_)
      | Unknown(_) => true
      | _ => false
      }
    | (Some(Arrow(dom, _)), Some(t)) =>
      accepts(Typ.normalize(ctx, dom), t)
    | _ => false
    };
  };
  let outside_domain = (name: string) =>
    Error("the argument to " ++ name ++ " isn't known to be of its domain");
  /* The rule in `Axioms.standard_primitives` for the partial primitive
   * `e` applies, with the rule's binders and their instantiation. A
   * builtin's name counts only where it still names the builtin. */
  let primitive_rule = (e: Exp.t) => {
    let names_builtin = (x: Var.t) =>
      switch (Environment.lookup(env, x)) {
      | Some({term: BuiltinFun(y), _}) => String.equal(x, y)
      | Some(_) => false
      | None => is_builtin(x)
      };
    let rec unify = (binders, pat: Exp.t, e: Exp.t, instantiation) =>
      switch (instantiation) {
      | None => None
      | Some(bound) =>
        switch (Exp.term_of(pat), Exp.term_of(e)) {
        | (_, Parens(e)) => unify(binders, pat, e, instantiation)
        | (Var(v), _) when List.Assoc.mem(binders, v, ~equal=String.equal) =>
          switch (List.Assoc.find(bound, v, ~equal=String.equal)) {
          | Some(e') => Exp.fast_equal(e', e) ? instantiation : None
          | None => Some([(v, e), ...bound])
          }
        | (Var(f), Var(g)) =>
          String.equal(f, g) && names_builtin(g) ? instantiation : None
        | (Var(f), BuiltinFun(g)) =>
          String.equal(f, g) ? instantiation : None
        | (BinOp(op, p1, p2), BinOp(op', e1, e2))
            when Operators.equal_op_bin(op, op') =>
          unify(binders, p2, e2, unify(binders, p1, e1, instantiation))
        | (Ap(_, p1, p2), Ap(Forward, e1, e2)) =>
          unify(binders, p2, e2, unify(binders, p1, e1, instantiation))
        | (Tuple(ps), Tuple(es)) when List.length(ps) == List.length(es) =>
          List.fold2_exn(
            ~f=(instantiation, p, e) => unify(binders, p, e, instantiation),
            ~init=instantiation,
            ps,
            es,
          )
        | _ => None
        }
      };
    List.find_map(
      ~f=
        ((_, binders, application, condition)) =>
          unify(binders, application, e, Some([]))
          |> Option.map(~f=instantiation =>
               (binders, instantiation, condition)
             ),
      Axioms.standard_primitives,
    );
  };
  let rec go = (scope: scope, e: Exp.t): result(list(Exp.t), string) =>
    switch (primitive_rule(e)) {
    /* Each argument standard and of its binder's type, and the rule's
     * condition on them. */
    | Some((binders, instantiation, condition)) =>
      let* conditions =
        List.fold_left(
          ~f=
            (acc, (v, t)) => {
              let* acc = acc;
              let arg =
                List.Assoc.find_exn(instantiation, v, ~equal=String.equal);
              switch (type_of(scope, arg)) {
              | Some(t') when Typ.fast_equal(t, t') =>
                let+ cs = go(scope, arg);
                acc @ cs;
              | _ =>
                Error(
                  "an argument isn't known to be " ++ Typ.pretty_print(t),
                )
              };
            },
          ~init=Ok([]),
          binders,
        );
      Ok(
        conditions
        @ [
          Substitution.in_exp(
            Substitution.env_for(instantiation, ~into=condition),
            condition,
          ),
        ],
      );
    | None => structure(scope, e)
    }
  /* `e` judged by its form. */
  and structure = (scope: scope, e: Exp.t): result(list(Exp.t), string) => {
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
    /* A branch's conditions, needed only where its guard `guard` holds:
     * dropped where `guards`, what is known there, implies them, and
     * otherwise conditional on `guard`. */
    let guarded = (~guards, guard: Exp.t, conditions) => {
      let is_nat = x =>
        switch (
          Option.map(~f=Typ.term_of, type_of(scope, Exp.fresh(Var(x))))
        ) {
        | Some(Atom(Nat)) => true
        | _ => false
        };
      List.filter_map(
        ~f=
          c =>
            Bounds.implies(~nat=is_nat, guards, c)
              ? None : Some(Exp.fresh(BinOp(Bool(Implies), guard, c))),
        conditions,
      );
    };
    let operands = (cls, a, b) =>
      is_atom(cls, scope, a) && is_atom(cls, scope, b)
        ? all([a, b])
        : Error("an operand's type isn't known to be " ++ Atom.show_cls(cls));
    switch (Exp.term_of(e)) {
    | Parens(e) => go(scope, e)
    /* A cast fails on a value not of its type, where the type is known. */
    | Asc(inner, t) =>
      switch (known(Typ.normalize(ctx, t)), type_of(scope, inner)) {
      | (None, _) => go(scope, inner)
      | (Some(t), Some(t')) when accepts(t, Typ.normalize(ctx, t')) =>
        go(scope, inner)
      | (Some({term: List(_), _}), _) when is_nil(inner) => Ok([])
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
    /* Each matches its rule in `Axioms.standard_primitives` first. */
    | BinOp(Int(Divide | Power) | SInt(Divide | Power) | Nat(Divide), _, _) =>
      Error("a partial operator outside its rule")
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
      let branch = (holds, e) => {
        let guards = Bounds.assume(scope.guards, c, holds);
        let+ cs =
          go(
            {
              ...scope,
              guards,
            },
            e,
          );
        guarded(~guards, holds ? c : Exp.fresh(UnOp(Bool(Not), c)), cs);
      };
      if (is_atom(Bool, scope, c)) {
        let* c_cs = go(scope, c);
        let* a_cs = branch(true, a);
        let+ b_cs = branch(false, b);
        c_cs @ a_cs @ b_cs;
      } else {
        Error("an `if` on a non-Bool");
      };
    | Match(scrut, rules) =>
      if (!covers(scope, scrut, List.map(~f=fst, rules))) {
        Error("a `case` that may not cover its scrutinee");
      } else {
        let* cs = go(scope, scrut);
        let scrut_ty = type_of(scope, scrut);
        /* On a number, a rule is reached only past the literals of the
         * rules before it, and a variable pattern is the scrutinee, so
         * its conditions are about the scrutinee. */
        let rec unwrapped = (p: Pat.t) =>
          switch (Pat.term_of(p)) {
          | Parens(p)
          | Asc(p, _) => unwrapped(p)
          | _ => p
          };
        let is_literal = (p: Pat.t) =>
          switch (Pat.term_of(unwrapped(p))) {
          | Atom(Int(_) | Nat(_)) => true
          | _ => false
          };
        let compared = (op, p) =>
          Exp.fresh(BinOp(Poly(op), scrut, Exp.of_pat(unwrapped(p))));
        let+ (cs, _) =
          List.fold_left(
            ~f=
              (acc, (p, rhs)) => {
                let* (acc, past) = acc;
                let names = Pat.bound_vars(p);
                let guard =
                  switch (
                    past @ (is_literal(p) ? [compared(Equals, p)] : [])
                  ) {
                  | [] => None
                  | [g, ...gs] =>
                    Some(
                      List.fold_left(
                        ~f=(g, g') => Exp.fresh(BinOp(Bool(And), g, g')),
                        ~init=g,
                        gs,
                      ),
                    )
                  };
                let guards =
                  Option.value_map(guard, ~default=scope.guards, ~f=g =>
                    Bounds.assume(scope.guards, g, true)
                  );
                let rhs_scope =
                  within(
                    {
                      ...scope,
                      guards,
                    },
                    names,
                    pattern_types(p, scrut_ty),
                  );
                let rhs_scope =
                  switch (
                    Pat.term_of(unwrapped(p)),
                    Bounds.variable(scrut),
                  ) {
                  | (Var(m), Some(x)) => {
                      ...rhs_scope,
                      guards: Bounds.alias(rhs_scope.guards, m, x),
                    }
                  | _ => rhs_scope
                  };
                let* rhs_cs = go(rhs_scope, rhs);
                let rhs_cs =
                  switch (Pat.term_of(unwrapped(p))) {
                  | Var(m) => instantiate(m, scrut, rhs_cs)
                  | _ => rhs_cs
                  };
                let* rhs_cs = lift(names, rhs_cs);
                let rhs_cs =
                  switch (guard) {
                  | Some(g) => guarded(~guards, g, rhs_cs)
                  | None => rhs_cs
                  };
                let past =
                  is_literal(p) ? past @ [compared(NotEquals, p)] : past;
                Ok((acc @ rhs_cs, past));
              },
            ~init=Ok((cs, [])),
            rules,
          );
        cs;
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
      let* cs =
        go(within(scope, names, pattern_types(p, domain(p, t))), body);
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
        let at =
          switch (type_of(scope, arg)) {
          | Some(_) as at => at
          | None => domain(p, t)
          };
        let+ cs = go(within(scope, [x], pattern_types(p, at)), body);
        instantiate(x, arg, cs);
      | None =>
        fits(scope, type_of(scope, f), arg)
          ? go(scope, f) : outside_domain("the function")
      }
    | BuiltinFun(name) =>
      if (!fits(scope, builtin_type(name), arg)) {
        outside_domain(name);
      } else if (List.mem(total_builtins, name, ~equal=String.equal)) {
        Ok([]);
      } else {
        Error("the builtin `" ++ name ++ "` may fail here");
      }
    | Var(x)
        when List.mem(scope.bound, x, ~equal=String.equal) || is_standard(x) =>
      fits(scope, type_of(scope, f), arg)
        ? go(scope, f) : outside_domain(x)
    /* A lambda's conditions take the argument; any other definition is
     * judged as a whole. */
    | Var(x) when Definitions.is_definition(env, x) =>
      switch (definition_value(x)) {
      | Some({term: Fun(_) | BuiltinFun(_), _} as value) =>
        apply(scope, value, arg)
      | Some(_)
      | None => definition(~at=type_of(scope, arg), x)
      }
    | Var(x) when is_builtin(x) =>
      apply(scope, Exp.fresh(BuiltinFun(x)), arg)
    | FixF(self, f, fix_env) =>
      fixpoint(~declared=None, ~at=type_of(scope, arg), self, f, fix_env)
    | Constructor(_) => Ok([])
    | _ =>
      fits(scope, type_of(scope, f), arg)
        ? go(scope, f) : outside_domain("the function")
    }
  /* A recursive function applied to an argument of type `at`, judged at
   * that type rather than the type it declares: its parameter gets that
   * type, from which the patterns it splits on give its parts theirs, and
   * its own calls take arguments of that type, as the induction on their
   * size needs. */
  and fixpoint =
      (
        ~declared: option(Typ.t),
        ~at: option(Typ.t),
        self: Pat.t,
        f: Exp.t,
        fix_env,
      )
      : result(list(Exp.t), string) =>
    switch (lambda(f)) {
    | Some((param, body, _, name)) =>
      let cod = fix_result(~declared, ~at, self, param, body);
      go(
        {
          ...empty,
          types:
            List.filter(
              ~f=
                ((n, _)) =>
                  List.mem(Pat.bound_vars(self), n, ~equal=String.equal),
              fix_scope(~at, ~cod, self, param).types,
            ),
        },
        Exp.fresh(
          FixF(self, Exp.fresh(Fun(param, body, at, name)), fix_env),
        ),
      );
    | None => go(empty, Exp.fresh(FixF(self, f, fix_env)))
    }
  /* A definition's value, closed and judged once, at its argument's type
   * `at` where it is applied. */
  and definition =
      (~at: option(option(Typ.t))=?, x: Var.t)
      : result(list(Exp.t), string) => {
    let judge = value =>
      switch (at, Exp.term_of(value)) {
      | (Some(at), FixF(self, f, fix_env)) =>
        fixpoint(~declared=ctx_type(x), ~at, self, f, fix_env)
      | _ =>
        let self_types =
          switch (Exp.term_of(value), ctx_type(x)) {
          | (FixF(p, _, _), Some(t)) =>
            List.map(~f=name => (name, t), Pat.bound_vars(p))
          | _ => []
          };
        go(
          {
            ...empty,
            types: self_types,
          },
          value,
        );
      };
    switch (Stdlib.Hashtbl.find_opt(definitions, (x, at))) {
    | Some(verdict) => verdict
    | None =>
      let verdict =
        switch (definition_value(x)) {
        | Some(value) => judge(value)
        | None => Error("`" ++ x ++ "` has no definition")
        };
      Stdlib.Hashtbl.add(definitions, (x, at), verdict);
      verdict;
    };
  };
  switch (go(empty, e)) {
  | Ok(conditions) => Standard(DomainConditions.dedup(conditions))
  | Error(reason) => Unknown(reason)
  };
};
