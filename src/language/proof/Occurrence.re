open Util;

/* Locating the subterm a proof focus selects.
 *
 * `from <nth> <exp> do` selects the nth subterm of the goal that equals
 * `<exp>`, counting left to right in the source and not descending into a
 * counted match. The stepper counts with `index`, which is 0-based; the checker
 * reads the occurrence back with `nth_env_target`.
 *
 * Past the subterms, a selector can also name a run of consecutive operands
 * of an associative operator: `b + c` in `a + b + c`, which the parser
 * groups as `(a + b) + c`, so `b + c` is not a subterm (`Run`). */

/* Immediate expression children, in `map_term` order, including those
 * inside patterns. */
let children = (e: Exp.t): list(Exp.t) => {
  let kids = ref([]);
  let root_id = Exp.rep_id(e);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (cont, inner) =>
          if (Id.equal(Exp.rep_id(inner), root_id)) {
            cont(inner);
          } else {
            kids := [inner, ...kids^];
            inner;
          },
      e,
    );
  List.rev(kids^);
};

/* The subterms of `e` in source order, each before its own subterms, not
 * descending into a subterm `stop` accepts. `map_term` can't give this
 * order: OCaml evaluates constructor arguments right to left, so it
 * visits `b` before `a` in `a + b`. */
let rec in_source_order = (~stop: Exp.t => bool, e: Exp.t): list(Exp.t) => {
  let in_pat = (p: Pat.t): list(Exp.t) => {
    let found = ref([]);
    let _: Pat.t =
      Pat.map_term(
        ~f_exp=
          (_, e) => {
            found := [e, ...found^];
            e;
          },
        p,
      );
    List.rev(found^);
  };
  let kids =
    switch (Exp.term_of(e)) {
    | Invalid(_)
    | EmptyHole
    | Deferral(_)
    | Undefined
    | Atom(_)
    | Constructor(_)
    | Label(_)
    | ExplicitNonlabel
    | LivelitName(_)
    | Var(_)
    | BuiltinFun(_) => []
    | DynamicErrorHole(a, _)
    | TypFun(_, a, _)
    | TyAlias(_, _, a)
    | Use(_, a)
    | TypAp(a, _)
    | Test(a)
    | Filter(_, a)
    | Closure(_, a)
    | Parens(a)
    | Projector(_, a)
    | UnOp(_, a)
    | Asc(a, _) => [a]
    | TupLabel(a, b)
    | Dot(a, b)
    | Ap(_, a, b)
    | Seq(a, b)
    | HintedTest(a, b)
    | Cons(a, b)
    | ListConcat(a, b)
    | BinOp(_, a, b)
    | TupleExtension(a, b) => [a, b]
    | If(a, b, c) => [a, b, c]
    | ListLit(es)
    | Tuple(es) => es
    | DeferredAp(f, args) => [f, ...args]
    | Fun(p, body, _, _)
    | Forall(p, body)
    | FixF(p, body, _) => in_pat(p) @ [body]
    | Let(p, def, body) => in_pat(p) @ [def, body]
    | Match(scrut, rules) => [
        scrut,
        ...List.concat_map(
             ~f=((p, branch)) => in_pat(p) @ [branch],
             rules,
           ),
      ]
    /* Rare in a goal; their children's order is `map_term`'s. */
    | MultiHole(_)
    | DrvQuote(_)
    | Theorem(_)
    | Module(_)
    | ModuleExp(_) => children(e)
    };
  stop(e)
    ? [e] : [e, ...List.concat_map(~f=in_source_order(~stop), kids)];
};

/* How many subterms equal to `e1` come before the node with `e1`'s id in
 * `e2`, not counting inside an earlier one. `None` if that id is not in
 * `e2`. */
let index = (e1: Exp.t, e2: Exp.t): option(int) => {
  let is_node = e => Id.equal(Exp.rep_id(e), Exp.rep_id(e1));
  let is_equal = e => Equality.ignoring_ascriptions.exp(e, e1);
  let rec count = (n, subterms) =>
    switch (subterms) {
    | [] => None
    | [e, ..._] when is_node(e) => Some(n)
    | [e, ...rest] => count(is_equal(e) ? n + 1 : n, rest)
    };
  count(0, in_source_order(~stop=e => !is_node(e) && is_equal(e), e2));
};

/* The (n+1)th subterm of `e` that `matches`. */
let nth_matching = (matches: Exp.t => bool, n: int, e: Exp.t): option(Exp.t) =>
  List.nth(in_source_order(~stop=matches, e) |> List.filter(~f=matches), n);

/* The (n+1)th occurrence of `e1` in `e2`. */
let nth = (e1: Exp.t, n: int, e2: Exp.t): option(Exp.t) =>
  nth_matching(exp => Equality.ignoring_ascriptions.exp(exp, e1), n, e2);

/* The (n+1)th subterm of `e2` that matches `pat`, an expression that may
 * contain `$e`/`$v`/`$x` metavariables (see MetaVar). A metavariable-free
 * pattern matches exactly the terms it is structurally equal to, so
 * indexing among matches generalizes indexing among occurrences. */
let nth_pattern =
    (
      ~statics: ProofStatics.t=ProofStatics.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      pat: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) =>
  nth_matching(MatchExp.matches_pattern(~statics, ~env, pat), n, e2);

/* Whether a subterm is one `selector` names as written. Pattern
 * matching (MatchExp) and the equality `nth` uses (Equality) are
 * different relations, so only a selector that uses the pattern language
 * goes through the matcher; a ground selector is compared with Equality. */
let names_as_written =
    (~statics: ProofStatics.t, ~env: Environment.t(Exp.t), selector: Exp.t)
    : (Exp.t => bool) =>
  MetaVar.has_meta(selector)
    ? MatchExp.matches_pattern(~statics, ~env, selector)
    : (e => Equality.ignoring_ascriptions.exp(e, selector));

/* The (n+1)th subterm of `e2` that `selector` names as written. */
let nth_target =
    (
      ~statics: ProofStatics.t=ProofStatics.empty,
      ~env: Environment.t(Exp.t)=Environment.empty,
      selector: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(Exp.t) =>
  nth_matching(names_as_written(~statics, ~env, selector), n, e2);

/* `exp` with the node whose id is `id` replaced by `new_exp`, ids and
 * all, so `new_exp` can be found again in the result. The caller ensures
 * none of its ids occur elsewhere in `exp`. */
let replace_by_id = (id: Id.t, exp: Exp.t, new_exp: Exp.t) =>
  Exp.map_term(
    ~f_exp=
      (cont, exp) => Id.equal(Exp.rep_id(exp), id) ? new_exp : cont(exp),
    exp,
  );

/* A chain of an associative operator, read as its list of operands. */
module Chain = {
  /* An operator whose grouping can't change a value or an error, named
   * by its `+` for a sum. Int and SInt sums subtract too, since
   * `(a + b) - c` is `a + (b - c)`; Nat's `-` stops at 0, so Nat sums
   * don't. Float arithmetic isn't regrouped: it rounds. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type op =
    | Bin(Operators.op_bin)
    | ListConcat;

  let rec op_of = (e: Exp.t): option(op) =>
    switch (Exp.term_of(e)) {
    | Parens(inner) => op_of(inner)
    | BinOp(Int(Plus | Minus), _, _) => Some(Bin(Int(Plus)))
    | BinOp(SInt(Plus | Minus), _, _) => Some(Bin(SInt(Plus)))
    | BinOp(
        (
          Int(Times) | SInt(Times) | Nat(Plus | Times) | Bool(And | Or) |
          String(Concat)
        ) as op,
        _,
        _,
      ) =>
      Some(Bin(op))
    | ListConcat(_) => Some(ListConcat)
    | _ => None
    };

  let is = (op: op, e: Exp.t): bool =>
    Option.equal(equal_op, op_of(e), Some(op));

  let subtracts = (e: Exp.t): bool =>
    switch (Exp.term_of(e)) {
    | BinOp(Int(Minus) | SInt(Minus), _, _) => true
    | _ => false
    };

  /* An operand, and whether the chain subtracts it, as it does `c` in
   * `a + b - c`. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type operand = {
    exp: Exp.t,
    subtracted: bool,
  };

  /* An operand keeps its own parentheses; those around a part of the
   * chain are looked through. What is subtracted is one operand, so
   * `a - (b + c)` has two. */
  let rec operands = (op: op, e: Exp.t): list(operand) =>
    switch (Exp.term_of(e)) {
    | Parens(inner) when is(op, inner) => operands(op, inner)
    | BinOp(_, l, r) when is(op, e) && subtracts(e) =>
      operands(op, l)
      @ [
        {
          exp: r,
          subtracted: true,
        },
      ]
    | BinOp(_, l, r)
    | ListConcat(l, r) when is(op, e) => operands(op, l) @ operands(op, r)
    | _ => [
        {
          exp: e,
          subtracted: false,
        },
      ]
    };

  let join = (op: op, l: Exp.t, r: operand): Exp.t =>
    Exp.fresh(
      switch (op) {
      | Bin(Int(_)) when r.subtracted => BinOp(Int(Minus), l, r.exp)
      | Bin(SInt(_)) when r.subtracted => BinOp(SInt(Minus), l, r.exp)
      | Bin(op) => BinOp(op, l, r.exp)
      | ListConcat => ListConcat(l, r.exp)
      },
    );

  /* `first` and `rest` joined as the parser groups them (Precedence.re):
   * arithmetic to the left, the others to the right. Only arithmetic
   * subtracts, so a right-grouped chain has nothing subtracted. */
  let rec join_all = (op: op, first: Exp.t, rest: list(operand)): Exp.t => {
    let to_left =
      switch (op) {
      | Bin(Int(_) | SInt(_) | Nat(_)) => true
      | Bin(Float(_) | Bool(_) | String(_) | Poly(_))
      | ListConcat => false
      };
    switch (rest) {
    | [] => first
    | [next, ...rest] when to_left =>
      join_all(op, join(op, first, next), rest)
    | [next, ...rest] =>
      join(
        op,
        first,
        {
          exp: join_all(op, next.exp, rest),
          subtracted: false,
        },
      )
    };
  };

  /* `operands` joined as `like`, a chain with as many operands, is
   * grouped, with its operators. None unless `like` subtracts the same
   * operands, as only then are the two equal. */
  let group_like = (op: op, like: Exp.t, run: list(operand)): option(Exp.t) => {
    let rec go = (like: Exp.t, run: list(operand)) =>
      switch (Exp.term_of(like), run) {
      | (Parens(inner), _) when is(op, inner) => go(inner, run)
      | (BinOp(o, l, r), _) when is(op, like) =>
        let (l, run) = go(l, run);
        let (r, run) =
          switch (subtracts(like), run) {
          | (true, [operand, ...run]) => (operand.exp, run)
          | _ => go(r, run)
          };
        (Exp.fresh(BinOp(o, l, r)), run);
      | (ListConcat(l, r), _) when is(op, like) =>
        let (l, run) = go(l, run);
        let (r, run) = go(r, run);
        (Exp.fresh(ListConcat(l, r)), run);
      | (_, [operand, ...run]) => (operand.exp, run)
      | (_, []) => (like, [])
      };
    let signs = List.map(~f=o => o.subtracted);
    List.equal(Bool.equal, signs(operands(op, like)), signs(run))
      ? Some(fst(go(like, run))) : None;
  };

  /* `e` and the parts of it that are chains of `op` too. */
  let rec spine = (op: op, e: Exp.t): list(Exp.t) =>
    switch (Exp.term_of(e)) {
    | Parens(inner) when is(op, inner) => [e, ...spine(op, inner)]
    | BinOp(_, l, r) when is(op, e) => [
        e,
        ...spine(op, l) @ (subtracts(e) ? [] : spine(op, r)),
      ]
    | ListConcat(l, r) when is(op, e) => [
        e,
        ...spine(op, l) @ spine(op, r),
      ]
    | _ => []
    };

  /* The chains of `op` in `e` that aren't part of a larger one: outer
   * chains before the chains in their operands. */
  let rec all = (op: op, e: Exp.t): list(Exp.t) =>
    in_source_order(~stop=is(op), e)
    |> List.filter(~f=is(op))
    |> List.concat_map(~f=chain =>
         [
           chain,
           ...List.concat_map(~f=o => all(op, o.exp), operands(op, chain)),
         ]
       );
};

/* What a selector names in a goal. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Node(Exp.t)
  /* Consecutive operands of a chain in the goal, which needn't be a
   * node of it. */
  | Run(run)
and run = {
  op: Chain.op,
  /* The whole chain, a node of the goal. */
  chain: Exp.t,
  before: list(Chain.operand),
  /* The first isn't subtracted. */
  operands: list(Chain.operand),
  after: list(Chain.operand),
  /* `operands` joined as the selector that names them is grouped. */
  exp: Exp.t,
};

/* What a focus on the target starts from. */
let exp = (target: t): Exp.t =>
  switch (target) {
  | Node(e) => e
  | Run({exp, _}) => exp
  };

/* The goal's node that is the target, or that its run is in. */
let node = (target: t): Exp.t =>
  switch (target) {
  | Node(e) => e
  | Run({chain, _}) => chain
  };

/* The goal's nodes the target starts and ends with: a node twice, or a
 * run's first and last operands. */
let ends = (target: t): (Exp.t, Exp.t) =>
  switch (target) {
  | Node(e) => (e, e)
  | Run({operands, chain, _}) =>
    switch (List.map(~f=(o: Chain.operand) => o.exp, operands)) {
    | [] => (chain, chain)
    | [first, ...rest] => (
        first,
        ListUtil.last_opt(rest) |> Option.value(~default=first),
      )
    }
  };

let same = (t1: t, t2: t): bool =>
  switch (t1, t2) {
  | (Node(e1), Node(e2)) => Id.equal(Exp.rep_id(e1), Exp.rep_id(e2))
  | (Run(r1), Run(r2)) =>
    let ids = List.map(~f=(o: Chain.operand) => Exp.rep_id(o.exp));
    Id.equal(Exp.rep_id(r1.chain), Exp.rep_id(r2.chain))
    && List.equal(Id.equal, ids(r1.operands), ids(r2.operands));
  | (Node(_), Run(_))
  | (Run(_), Node(_)) => false
  };

/* `goal` with `target` replaced by `e`. A run's chain is joined again as
 * the parser groups it, with `e`'s operands among its own when `e` is a
 * chain of the same operator, so the goal reads as if it were typed. */
let replace = (target: t, goal: Exp.t, e: Exp.t): Exp.t =>
  switch (target) {
  | Node(node) => replace_by_id(Exp.rep_id(node), goal, e)
  | Run({op, chain, before, after, _}) =>
    let joined =
      switch (before @ Chain.operands(op, e) @ after) {
      | [first, ...rest] => Chain.join_all(op, first.exp, rest)
      | [] => e
      };
    replace_by_id(Exp.rep_id(chain), goal, joined);
  };

/* The runs `matches` accepts, for a selector that is a chain: outer
 * chains before the chains in their operands, and in a chain by first
 * operand. A run that is a node `matches` accepts is left out, as the
 * search for subterms finds it. So is a run whose first operand is
 * subtracted: as a node of its own it would be added. */
let runs = (~matches: Exp.t => bool, selector: Exp.t, goal: Exp.t): list(run) =>
  switch (Chain.op_of(selector)) {
  | None => []
  | Some(op) =>
    let length = List.length(Chain.operands(op, selector));
    let ids = List.map(~f=(o: Chain.operand) => Exp.rep_id(o.exp));
    let in_chain = (chain: Exp.t) => {
      let all = Chain.operands(op, chain);
      let matched_nodes =
        Chain.spine(op, chain)
        |> List.filter(~f=matches)
        |> List.map(~f=node => ids(Chain.operands(op, node)));
      List.init(max(0, List.length(all) - length + 1), ~f=first =>
        switch (ListUtil.split_n(first, all)) {
        | (_, [{subtracted: true, _}, ..._]) => None
        | (before, rest) =>
          let (operands, after) = ListUtil.split_n(length, rest);
          Chain.group_like(op, selector, operands)
          |> Option.map(~f=exp =>
               {
                 op,
                 chain,
                 before,
                 operands,
                 after,
                 exp,
               }
             );
        }
      )
      |> List.filter_map(~f=Fn.id)
      |> List.filter(~f=run =>
           !
             List.mem(
               matched_nodes,
               ids(run.operands),
               ~equal=List.equal(Id.equal),
             )
           && matches(run.exp)
         );
    };
    List.concat_map(Chain.all(op, goal), ~f=in_chain);
  };

/* The (n+1)th target of `selector` in `e2`, modulo the definitions in
 * `env`. The goal is searched as written first. Only if it has too few
 * occurrences there is it searched again, unfolding a name where it
 * hides structure the selector has (`MatchExp`'s `~defs`). The runs the
 * selector names are numbered after the subterms it names either way, so
 * a subterm's number doesn't depend on them.
 *
 * Equality compares up to parens/projectors, ascriptions,
 * alpha-renaming (expression and type), function names, filters and
 * hole provenance, but not up to elaboration-supplied type information.
 * Two same-named constructors at different types are different terms,
 * and a step must not retarget across them. So both sides must be
 * elaborated: an unelaborated `Constructor(c, None)` never equals its
 * elaborated `Constructor(c, Some(Some(ty)))`, and the mismatch reads as
 * "target not found". This is why the theorem context records
 * elaborated statements (`Statics`' Theorem arm):
 * `ProofCtx.of_theorem_ctx` splices a cited rule's right-hand side into
 * elaborated goals. */
let nth_env_target =
    (
      ~statics: ProofStatics.t,
      ~env: Environment.t(Exp.t),
      selector: Exp.t,
      n: int,
      e2: Exp.t,
    )
    : option(t) => {
  let as_written = names_as_written(~statics, ~env, selector);
  let unfolding =
    MatchExp.matches_pattern(~statics, ~env, ~defs=env, selector);
  let subterms = matches =>
    in_source_order(~stop=matches, e2) |> List.filter(~f=matches);
  let written = subterms(as_written);
  switch (List.nth(written, n)) {
  | Some(node) => Some(Node(node))
  | None =>
    let unfolded = subterms(unfolding);
    switch (List.nth(unfolded, n)) {
    | Some(node) => Some(Node(node))
    | None =>
      List.nth(
        runs(~matches=e => as_written(e) || unfolding(e), selector, e2),
        n - max(List.length(written), List.length(unfolded)),
      )
      |> Option.map(~f=run => Run(run))
    };
  };
};

/* `selector` with as many of its subterms replaced by `$e` as keep
 * `nth_env_target` resolving it to `target` in `goal`, at an index no
 * greater than `index`, and the index it resolves at. Only a replacement
 * that passes this check is kept, so a `selector` that misses the target
 * comes back unchanged. Subterms are tried outermost first, then left to
 * right, so a whole argument becomes one `$e` before its parts are
 * tried. */
let generalize =
    (
      ~statics: ProofStatics.t,
      ~env: Environment.t(Exp.t),
      ~index: int,
      selector: Exp.t,
      target: t,
      goal: Exp.t,
    )
    : (Exp.t, int) => {
  let index_of = candidate =>
    List.init(index + 1, ~f=n => n)
    |> List.find(~f=n =>
         switch (nth_env_target(~statics, ~env, candidate, n, goal)) {
         | Some(found) => same(found, target)
         | None => false
         }
       );
  List.fold_left(
    ~f=
      ((current, _) as best, sub) =>
        switch (Exp.find_by_id(Exp.rep_id(sub), current)) {
        /* Inside a subterm already replaced. */
        | None => best
        /* The `_` of `f(_, x)` is what makes it a partial application:
         * written as `$e(…)` it reads back as a call. */
        | Some({term: Deferral(_), _}) => best
        | Some(_) =>
          let candidate =
            replace_by_id(
              Exp.rep_id(sub),
              current,
              Exp.fresh(Var(MetaVar.any_name)),
            );
          switch (index_of(candidate)) {
          | Some(n) => (candidate, n)
          | None => best
          };
        },
    ~init=(selector, index),
    in_source_order(~stop=_ => false, selector),
  );
};

/* The index `nth_env_target` finds `target` at with its own expression
 * as the selector. */
let index_of =
    (
      ~statics: ProofStatics.t,
      ~env: Environment.t(Exp.t),
      target: t,
      goal: Exp.t,
    )
    : option(int) => {
  let rec from = n =>
    switch (nth_env_target(~statics, ~env, exp(target), n, goal)) {
    | Some(found) when same(found, target) => Some(n)
    | Some(_) => from(n + 1)
    | None => None
    };
  from(0);
};

/* What a selection of the nodes `ids` in `goal` names, given `node`, the
 * smallest node holding them. When `node` is part of a chain, that is
 * the run of the operands the selection touches: those it is in, and the
 * two an operator it holds joins. The run is written as the parser
 * groups it. */
let of_selection = (~ids: list(Id.t), ~goal: Exp.t, node: Exp.t): t => {
  let in_spine = (op, chain) =>
    List.exists(Chain.spine(op, chain), ~f=s =>
      Id.equal(Exp.rep_id(s), Exp.rep_id(node))
    );
  let chain =
    Option.bind(Chain.op_of(node), ~f=op =>
      Chain.all(op, goal)
      |> List.find(~f=in_spine(op))
      |> Option.map(~f=chain => (op, chain))
    );
  switch (chain) {
  | None => Node(node)
  | Some((op, chain)) =>
    let operands = Chain.operands(op, chain);
    let index = (e: Exp.t) =>
      ListUtil.findi_opt(
        (o: Chain.operand) => Id.equal(Exp.rep_id(o.exp), Exp.rep_id(e)),
        operands,
      )
      |> Option.map(~f=fst);
    let rec holds = (id, e: Exp.t) =>
      List.mem(IdTagged.ids(e), id, ~equal=Id.equal)
      || List.exists(children(e), ~f=holds(id));
    let spine = Chain.spine(op, chain);
    let touched = id =>
      switch (
        ListUtil.findi_opt((o: Chain.operand) => holds(id, o.exp), operands)
      ) {
      | Some((i, _)) => [i]
      | None =>
        switch (
          List.find(spine, ~f=s =>
            List.mem(IdTagged.ids(s), id, ~equal=Id.equal)
          )
        ) {
        | Some({term: BinOp(_, l, r) | ListConcat(l, r), _} as s) =>
          let last_of_left =
            ListUtil.last_opt(Chain.operands(op, l))
            |> Option.map(~f=(o: Chain.operand) => o.exp);
          let first_of_right =
            Chain.subtracts(s)
              ? Some(r)
              : ListUtil.hd_opt(Chain.operands(op, r))
                |> Option.map(~f=(o: Chain.operand) => o.exp);
          Option.to_list(Option.bind(last_of_left, ~f=index))
          @ Option.to_list(Option.bind(first_of_right, ~f=index));
        | Some(_)
        | None => []
        }
      };
    let span = List.map(~f=(o: Chain.operand) => Exp.rep_id(o.exp));
    switch (List.concat_map(ids, ~f=touched)) {
    | [] => Node(node)
    | [i, ...is] =>
      let (lo, hi) = (
        List.fold(is, ~init=i, ~f=min),
        List.fold(is, ~init=i, ~f=max),
      );
      let (before, rest) = ListUtil.split_n(lo, operands);
      let (run, after) = ListUtil.split_n(hi - lo + 1, rest);
      switch (
        List.find(spine, ~f=s =>
          List.equal(Id.equal, span(Chain.operands(op, s)), span(run))
        ),
        run,
      ) {
      | (Some(s), _) => Node(s)
      | (None, [{subtracted: false, exp: first}, second, ...rest]) =>
        Run({
          op,
          chain,
          before,
          operands: run,
          after,
          exp: Chain.join_all(op, first, [second, ...rest]),
        })
      | (None, _) => Node(node)
      };
    };
  };
};

/* The node of `onto` at the position the node with id `id` has in
 * `from`, for two copies of one term with different ids. */
let rec at_same_position =
        (~from: Exp.t, ~onto: Exp.t, id: Id.t): option(Exp.t) =>
  if (Id.equal(Exp.rep_id(from), id)) {
    Some(onto);
  } else {
    let (from_kids, onto_kids) = (children(from), children(onto));
    List.length(from_kids) == List.length(onto_kids)
      ? List.zip_exn(from_kids, onto_kids)
        |> List.find_map(~f=((f, o)) =>
             at_same_position(~from=f, ~onto=o, id)
           )
      : None;
  };

/* The patterns binding variables around the node with id `id` in `e`,
 * outermost first: the parameters of the `fun`s, `fix`es and `forall`s
 * it is inside, the patterns of the `let`s whose body it is in, and of
 * the case arms it is in. Empty if `id` isn't in `e`. */
let binders = (id: Id.t, e: Exp.t): list(Pat.t) => {
  let rec go = (outer: list(Pat.t), e: Exp.t): option(list(Pat.t)) =>
    if (Id.equal(Exp.rep_id(e), id)) {
      Some(List.rev(outer));
    } else {
      let under = p => go([p, ...outer]);
      switch (Exp.term_of(e)) {
      | Fun(p, _, _, _)
      | FixF(p, _, _)
      | Forall(p, _) => List.find_map(~f=under(p), children(e))
      | Let(p, def, body) =>
        switch (go(outer, def)) {
        | Some(_) as found => found
        | None => under(p, body)
        }
      | Match(scrut, rules) =>
        switch (go(outer, scrut)) {
        | Some(_) as found => found
        | None =>
          List.find_map(~f=((p, branch)) => under(p, branch), rules)
        }
      | _ => List.find_map(~f=go(outer), children(e))
      };
    };
  go([], e) |> Option.value(~default=[]);
};
