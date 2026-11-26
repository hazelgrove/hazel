// Slicing minimality property tests
open Language;
open Test_Statics_Slicing_Prelude;

module G = QCheck.Gen;

type ty =
  | TInt
  | TBool
  | TArr(ty, ty)
  | TProd(ty, ty)
  | TLab(ty, ty);

let rec show_ty =
  fun
  | TInt => "Int"
  | TBool => "Bool"
  | TArr(a, b) => "(" ++ show_ty(a) ++ " -> " ++ show_ty(b) ++ ")"
  | TProd(a, b) => "(" ++ show_ty(a) ++ ", " ++ show_ty(b) ++ ")"
  | TLab(a, b) => "(a=" ++ show_ty(a) ++ ", b=" ++ show_ty(b) ++ ")";

let rec gen_ty = (d: int): G.t(ty) =>
  d <= 0
    ? G.oneofl([TInt, TBool])
    : G.frequency([
        (3, G.oneofl([TInt, TBool])),
        (1, G.map2((a, b) => TArr(a, b), gen_ty(d - 1), gen_ty(d - 1))),
        (1, G.map2((a, b) => TProd(a, b), gen_ty(d - 1), gen_ty(d - 1))),
        (1, G.map2((a, b) => TLab(a, b), gen_ty(d - 1), gen_ty(d - 1))),
      ]);

let prelude =
  "let id : poly A -> A -> A = abs A -> fun (x : A) -> x in "
  ++ "let dup : poly A -> A -> (A, A) = abs A -> fun (x : A) -> (x, x) in "
  ++ "let const : poly A -> poly B -> A -> B -> A = abs A -> abs B -> fun (x : A) -> fun (y : B) -> x in "
  ++ "let pair : poly A -> poly B -> A -> B -> (A, B) = abs A -> abs B -> fun (x : A) -> fun (y : B) -> (x, y) in ";

type flavour =
  | Core
  | WithJoins;

let rec gen_exp =
        (~flavour, env: list((string, ty)), t: ty, d: int): G.t(string) => {
  open G;
  let fresh = "x" ++ string_of_int(List.length(env));
  let sub = (~env=env, t, d) => gen_exp(~flavour, env, t, d);
  let vars = List.filter_map(((x, t')) => t' == t ? Some(x) : None, env);
  let fns =
    List.filter_map(
      ((f, t')) =>
        switch (t') {
        | TArr(a, b) when b == t => Some((f, a))
        | _ => None
        },
      env,
    );
  let intro =
    switch (t) {
    | TInt => map(string_of_int, int_bound(9))
    | TBool => oneofl(["true", "false"])
    | TArr(a, b) =>
      map(
        body =>
          "(fun (" ++ fresh ++ " : " ++ show_ty(a) ++ ") -> " ++ body ++ ")",
        sub(~env=[(fresh, a), ...env], b, max(0, d - 1)),
      )
    | TProd(a, b) =>
      map2(
        (x, y) => "(" ++ x ++ ", " ++ y ++ ")",
        sub(a, max(0, d - 1)),
        sub(b, max(0, d - 1)),
      )
    | TLab(a, b) =>
      map2(
        (x, y) => "(a=" ++ x ++ ", b=" ++ y ++ ")",
        sub(a, max(0, d - 1)),
        sub(b, max(0, d - 1)),
      )
    };
  let base = [(2, intro)] @ (vars == [] ? [] : [(3, oneofl(vars))]);
  if (d <= 0) {
    frequency(base);
  } else {
    let d' = d - 1;
    let let_ =
      gen_ty(1)
      >>= (
        t' =>
          bool
          >>= (
            annotated =>
              map2(
                (def, body) =>
                  "let "
                  ++ fresh
                  ++ (annotated ? " : " ++ show_ty(t') : "")
                  ++ " = "
                  ++ def
                  ++ " in "
                  ++ body,
                sub(t', d'),
                sub(~env=[(fresh, t'), ...env], t, d'),
              )
          )
      );
    let tuple_let =
      pair(gen_ty(1), gen_ty(1))
      >>= (
        ((t1, t2)) => {
          let y = fresh ++ "b";
          map2(
            (def, body) =>
              "let (" ++ fresh ++ ", " ++ y ++ ") = " ++ def ++ " in " ++ body,
            sub(TProd(t1, t2), d'),
            sub(~env=[(fresh, t1), (y, t2), ...env], t, d'),
          );
        }
      );
    let app_lam =
      gen_ty(1)
      >>= (
        t' =>
          map2(
            (body, arg) =>
              "(fun ("
              ++ fresh
              ++ " : "
              ++ show_ty(t')
              ++ ") -> "
              ++ body
              ++ ")("
              ++ arg
              ++ ")",
            sub(~env=[(fresh, t'), ...env], t, d'),
            sub(t', d'),
          )
      );
    let app_var =
      fns == []
        ? []
        : [
          (
            3,
            oneofl(fns)
            >>= (
              ((f, a)) => map(arg => f ++ "(" ++ arg ++ ")", sub(a, d'))
            ),
          ),
        ];
    let asc = map(e => "(" ++ e ++ " : " ++ show_ty(t) ++ ")", sub(t, d'));
    let poly_id =
      map(e => "id@<" ++ show_ty(t) ++ ">(" ++ e ++ ")", sub(t, d'));
    let poly_const =
      gen_ty(1)
      >>= (
        t' =>
          map2(
            (x, y) =>
              "const@<"
              ++ show_ty(t)
              ++ ">@<"
              ++ show_ty(t')
              ++ ">("
              ++ x
              ++ ")("
              ++ y
              ++ ")",
            sub(t, d'),
            sub(t', d'),
          )
      );
    let poly_prod =
      switch (t) {
      | TProd(a, b) when a == b => [
          (
            2,
            map(x => "dup@<" ++ show_ty(a) ++ ">(" ++ x ++ ")", sub(a, d')),
          ),
          (
            1,
            map2(
              (x, y) =>
                "pair@<"
                ++ show_ty(a)
                ++ ">@<"
                ++ show_ty(b)
                ++ ">("
                ++ x
                ++ ")("
                ++ y
                ++ ")",
              sub(a, d'),
              sub(b, d'),
            ),
          ),
        ]
      | TProd(a, b) => [
          (
            2,
            map2(
              (x, y) =>
                "pair@<"
                ++ show_ty(a)
                ++ ">@<"
                ++ show_ty(b)
                ++ ">("
                ++ x
                ++ ")("
                ++ y
                ++ ")",
              sub(a, d'),
              sub(b, d'),
            ),
          ),
        ]
      | _ => []
      };
    let proj =
      gen_ty(1)
      >>= (
        t' =>
          bool
          >>= (
            left =>
              map2(
                (x, y) =>
                  left
                    ? "(a=" ++ x ++ ", b=" ++ y ++ ").a"
                    : "(a=" ++ y ++ ", b=" ++ x ++ ").b",
                sub(t, d'),
                sub(t', d'),
              )
          )
      );
    let binop =
      switch (t) {
      | TInt => [
          (
            1,
            map2((x, y) => x ++ " + " ++ y, sub(TInt, d'), sub(TInt, d')),
          ),
        ]
      | TBool => [
          (
            1,
            map2((x, y) => x ++ " < " ++ y, sub(TInt, d'), sub(TInt, d')),
          ),
        ]
      | _ => []
      };
    let joins =
      switch (flavour) {
      | Core => []
      | WithJoins => [
          (
            2,
            map3(
              (c, x, y) => "if " ++ c ++ " then " ++ x ++ " else " ++ y,
              sub(TBool, d'),
              sub(t, d'),
              sub(t, d'),
            ),
          ),
        ]
      };
    frequency(
      base
      @ [
        (3, let_),
        (1, tuple_let),
        (2, app_lam),
        (1, asc),
        (1, poly_id),
        (1, poly_const),
        (1, proj),
      ]
      @ app_var
      @ poly_prod
      @ binop
      @ joins,
    );
  };
};

let gen_program = (~flavour, depth: int): G.t(string) =>
  G.(
    gen_ty(2)
    >>= (t => map(body => prelude ++ body, gen_exp(~flavour, [], t, depth)))
  );

let statics_map = (ctx: Ctx.t, e: Exp.t): option(Statics.Map.t) =>
  switch (Statics.mk(CoreSettings.on, ctx, e)) {
  | (m, _) => Some(m)
  | exception _ => None
  };

let marks = (m: Statics.Map.t): list(Mark.t) =>
  Id.Map.fold((_, info, acc) => acc @ Info.marks_of(info), m, []);

let is_substantive_exp = (e: Exp.t) =>
  switch (Exp.term_of(e)) {
  | EmptyHole
  | Label(_)
  | TupLabel(_)
  | Parens(_) => false
  | _ => true
  };
let is_substantive_pat = (p: Pat.t) =>
  switch (Pat.term_of(p)) {
  | EmptyHole
  | Label(_)
  | TupLabel(_)
  | Parens(_) => false
  | _ => true
  };
let is_substantive_typ = (t: Typ.t) =>
  switch (Typ.term_of(t)) {
  | Unknown(_)
  | Label(_)
  | TupLabel(_)
  | Parens(_) => false
  | _ => true
  };

let substantive = (e: Exp.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let note = (keep, id) =>
    if (keep) {
      acc := Id.Set.add(id, acc^);
    };
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          note(is_substantive_exp(e), Exp.rep_id(e));
          continue(e);
        },
      ~f_pat=
        (continue, p) => {
          note(is_substantive_pat(p), Pat.rep_id(p));
          continue(p);
        },
      ~f_typ=
        (continue, t) => {
          note(is_substantive_typ(t), Typ.rep_id(t));
          continue(t);
        },
      e,
    );
  acc^;
};

let strip_parens_deep = (t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=
      (continue, t) =>
        switch (Typ.term_of(t)) {
        | Parens(inner)
        | Projector(_, inner) => continue(inner)
        | _ => continue(t)
        },
    ~f_tpat=
      (continue, tp) =>
        switch (tp.term) {
        | Var(_) => continue(TPat.fresh(EmptyHole))
        | _ => continue(tp)
        },
    t,
  );

let precision_geq = (ctx: Ctx.t, ty: Typ.t, query: Typ.t): bool =>
  switch (Typ.meet(ctx, ty, query)) {
  | Some(met) =>
    Typ.fast_equal(met, ty)
    || Typ.fast_equal(strip_parens_deep(met), strip_parens_deep(ty))
  | None => false
  | exception _ => false
  };

let omit_at = (id: Id.t, t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=(continue, t) => Typ.rep_id(t) == id ? S.gap : continue(t),
    t,
  );

let typ_node_ids = (t: Typ.t): list(Id.t) => {
  let acc = ref([]);
  let _ =
    Typ.map_term(
      ~f_typ=
        (continue, t) => {
          if (is_substantive_typ(t)) {
            acc := [Typ.rep_id(t), ...acc^];
          };
          continue(t);
        },
      t,
    );
  acc^;
};

let query_chain = (t: Typ.t): list(Typ.t) => {
  let (rev, _) =
    List.fold_left(
      ((acc, cur), id) => {
        let next = omit_at(id, cur);
        ([next, ...acc], next);
      },
      ([t], t),
      typ_node_ids(t),
    );
  List.filter(q => typ_node_ids(q) != [], List.rev(rev));
};

type direction = [
  | `Syn
  | `Ana
];

let debug = Sys.getenv_opt("MINIMALITY_DEBUG") != None;

type outcome =
  | Minimal
  | Skipped
  | Invalid(string)
  | NonMinimal(list(string), string)
  | Crash(string);

let ty_at =
    (~direction: direction, m: Statics.Map.t, id: Id.t): option(Typ.t) =>
  switch (Statics.Map.lookup_exp(id, m)) {
  | None => None
  | Some(info) =>
    Some(
      switch (direction) {
      | `Syn => info.elab_syn_ty
      | `Ana => info.ana
      },
    )
  };

let render_id = (id: Id.t, e: Exp.t): string =>
  switch (find_any(id, e)) {
  | Some(a) => render_any(a)
  | None => "<gone>"
  };

let kind_of_any = (a: Any.t): string =>
  switch (a) {
  | Exp(x) => "exp " ++ Exp.show_cls(Exp.cls_of_term(Exp.term_of(x)))
  | Pat(p) => "pat " ++ Pat.show_cls(Pat.cls_of_term(Pat.term_of(p)))
  | Typ(t) => "typ " ++ Typ.show_cls(Typ.cls_of_term(Typ.term_of(t)))
  | _ => "other"
  };

let parent_of = (id: Id.t, e: Exp.t): option(Any.t) => {
  let stack = ref([]);
  let found = ref(None);
  let visit = (a: Any.t, substantive: bool, go) => {
    if (found^ == None && Any.rep_id(a) == id) {
      found :=
        (
          switch (stack^) {
          | [p, ..._] => Some(p)
          | [] => None
          }
        );
    };
    if (substantive) {
      stack := [a, ...stack^];
    };
    let r = go();
    if (substantive) {
      stack := List.tl(stack^);
    };
    r;
  };
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, x) =>
          visit(Exp(x), is_substantive_exp(x), () => continue(x)),
      ~f_pat=
        (continue, p) =>
          visit(Pat(p), is_substantive_pat(p), () => continue(p)),
      ~f_typ=
        (continue, t) =>
          visit(Typ(t), is_substantive_typ(t), () => continue(t)),
      e,
    );
  found^;
};

let check_one =
    (~direction: direction, e: Exp.t, focus: Id.t, query: Typ.t): outcome =>
  switch (
    Statics.slice(~ctx=base_ctx(), ~focus=Some(focus), ~direction, e, query)
  ) {
  | exception (
                S.Focus_not_found(_) | S.Wrong_focus_sort |
                S.Incompatible_query(_)
              ) =>
    Skipped
  | exception ex => Crash("slice raised " ++ Printexc.to_string(ex))
  | result =>
    let sliced = reconstruct(result.omitted, e);
    let ctx = Ctx.concat(result.gamma, base_ctx());
    let header = () =>
      Printf.sprintf(
        "program: %s\nfocus:   %s\nquery:   %s\npsi:     %s\nslice:   %s\ngamma:   %s",
        show_exp_src(e),
        render_id(focus, e),
        render_any(Typ(query)),
        render_any(Typ(result.psi)),
        show_exp_src(sliced),
        String.concat(
          ", ",
          List.filter_map(
            (entry: Ctx.entry) =>
              switch (entry) {
              | VarEntry({name, typ, _}) =>
                Some(name ++ " : " ++ render_any(Typ(typ)))
              | _ => None
              },
            result.gamma.entries,
          ),
        ),
      );
    let holds = (candidate: Exp.t): option(bool) =>
      switch (statics_map(ctx, candidate)) {
      | None => None
      | Some(m) =>
        switch (ty_at(~direction, m, focus)) {
        | None => Some(false)
        | Some(ty) => Some(precision_geq(ctx, ty, query))
        }
      };
    switch (holds(sliced)) {
    | None => Crash(header() ++ "\nstatics raised on slice")
    | Some(false) => Invalid(header())
    | Some(true) =>
      let present = substantive(sliced);
      let leaves =
        Id.Set.elements(present)
        |> List.filter(id => {
             let removed = reconstruct(Id.Set.add(id, result.omitted), e);
             Id.Set.cardinal(Id.Set.diff(present, substantive(removed)))
             == 1;
           });
      let witnesses =
        List.filter_map(
          id => {
            let removed = reconstruct(Id.Set.add(id, result.omitted), e);
            if (holds(removed) == Some(true)) {
              let parent = parent_of(id, sliced);
              let kind =
                (
                  switch (find_any(id, sliced)) {
                  | Some(a) => kind_of_any(a)
                  | None => "?"
                  }
                )
                ++ " under "
                ++ (
                  switch (parent) {
                  | Some(p) => kind_of_any(p)
                  | None => "root"
                  }
                );
              Some((
                kind,
                Printf.sprintf(
                  "  removable [%s]: %s   in   %s",
                  kind,
                  render_id(id, sliced),
                  switch (parent) {
                  | Some(p) => render_any(p)
                  | None => "root"
                  },
                ),
              ));
            } else {
              None;
            };
          },
          leaves,
        );
      witnesses == []
        ? Minimal
        : NonMinimal(
            List.sort_uniq(compare, List.map(fst, witnesses)),
            header() ++ "\n" ++ String.concat("\n", List.map(snd, witnesses)),
          );
    };
  };

let rewrite_joins = (e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        switch (Exp.term_of(e)) {
        | If(_)
        | Match(_)
        | ListLit(_)
        | Cons(_)
        | ListConcat(_) => exp_hole(e)
        | _ => continue(e)
        },
    ~f_pat=
      (continue, p) =>
        switch (Pat.term_of(p)) {
        | ListLit(_)
        | Cons(_) => pat_hole(p)
        | _ => continue(p)
        },
    e,
  );

type foci =
  | Root
  | Sample(int);

let check_program =
    (~direction: direction, ~foci: foci, rand, e: Exp.t): list(outcome) =>
  switch (statics_map(base_ctx(), e)) {
  | None => [Skipped]
  | Some(m) when marks(m) != [] =>
    if (debug) {
      Printf.printf(
        "skip ill-typed: %s\n  marks: %s\n",
        show_exp_src(e),
        String.concat("; ", List.map(Mark.show, marks(m))),
      );
    };
    [Skipped];
  | Some(m) =>
    let ids =
      switch (foci) {
      | Root => [whole(e)]
      | Sample(k) =>
        let all = Array.of_list(collect_exp_ids(_ => true, e));
        List.init(k, _ => all[Random.State.int(rand, Array.length(all))]);
      };
    List.concat_map(
      focus =>
        switch (ty_at(~direction, m, focus)) {
        | None => [Skipped]
        | Some(ty) =>
          switch (query_chain(ty)) {
          | [] => [Skipped]
          | queries =>
            List.map(q => check_one(~direction, e, focus, q), queries)
          }
        },
      ids,
    );
  };

type tally = {
  mutable programs: int,
  mutable checked: int,
  mutable minimal: int,
  mutable skipped: int,
  mutable invalid: list(string),
  mutable nonminimal: list(string),
  mutable crashes: list(string),
  kinds: Hashtbl.t(string, (int, string)),
};

let run_property =
    (
      ~name,
      ~count,
      ~seed,
      ~direction,
      ~foci,
      ~program: Random.State.t => Exp.t,
    )
    : tally => {
  let rand = Random.State.make([|seed|]);
  let t = {
    programs: 0,
    checked: 0,
    minimal: 0,
    skipped: 0,
    invalid: [],
    nonminimal: [],
    crashes: [],
    kinds: Hashtbl.create(16),
  };
  for (_ in 1 to count) {
    switch (program(rand)) {
    | exception _ => t.skipped = t.skipped + 1
    | e =>
      let outcomes = check_program(~direction, ~foci, rand, e);
      if (List.exists(o => o != Skipped, outcomes)) {
        t.programs = t.programs + 1;
      };
      List.iter(
        o =>
          switch (o) {
          | Skipped => t.skipped = t.skipped + 1
          | Minimal =>
            t.checked = t.checked + 1;
            t.minimal = t.minimal + 1;
          | Invalid(s) =>
            t.checked = t.checked + 1;
            t.invalid = [s, ...t.invalid];
          | NonMinimal(kinds, s) =>
            t.checked = t.checked + 1;
            t.nonminimal = [s, ...t.nonminimal];
            List.iter(
              k =>
                switch (Hashtbl.find_opt(t.kinds, k)) {
                | Some((n, ex)) => Hashtbl.replace(t.kinds, k, (n + 1, ex))
                | None => Hashtbl.replace(t.kinds, k, (1, s))
                },
              kinds,
            );
          | Crash(s) => t.crashes = [s, ...t.crashes]
          },
        outcomes,
      );
    };
  };
  let show = (label, xs) => {
    let xs = List.rev(xs);
    Printf.printf("  %s: %d\n", label, List.length(xs));
    List.iteri(
      (i, s) =>
        if (i < 6) {
          Printf.printf("---- %s %d\n%s\n", label, i + 1, s);
        },
      xs,
    );
  };
  Printf.printf(
    "\n==== %s\n  programs exercised: %d, slices checked: %d, minimal: %d, skipped: %d\n",
    name,
    t.programs,
    t.checked,
    t.minimal,
    t.skipped,
  );
  Printf.printf("  non-minimal: %d\n", List.length(t.nonminimal));
  Hashtbl.fold((k, v, acc) => [(k, v), ...acc], t.kinds, [])
  |> List.sort(((_, (a, _)), (_, (b, _))) => compare(b, a))
  |> List.iter(((k, (n, ex))) =>
       Printf.printf("---- [%s] x%d, first example:\n%s\n", k, n, ex)
     );
  show("invalid", t.invalid);
  show("crash", t.crashes);
  flush(stdout);
  t;
};

let typed = (~flavour, ~depth, rand) => {
  let src = G.generate1(~rand, gen_program(~flavour, depth));
  switch (Haz3lcore.Parser.to_term(src, ~root=Exp)) {
  | Some(e) => e
  | None =>
    if (debug) {
      Printf.printf("skip unparsable: %s\n", src);
    };
    failwith("unparsable");
  };
};

let hazel_random = rand =>
  rewrite_joins(
    G.generate1(
      ~rand,
      QCheck.get_gen(QCheck_Util.arb_exp(~minimal_idents=true, 30)),
    ),
  );

let count =
  switch (Sys.getenv_opt("MINIMALITY_COUNT")) {
  | Some(n) => int_of_string(n)
  | None => 200
  };

let property = (~name, ~direction, ~foci, ~program, ~expect_minimal) =>
  Alcotest.test_case(
    name,
    `Slow,
    () => {
      let t =
        run_property(~name, ~count, ~seed=7, ~direction, ~foci, ~program);
      if (expect_minimal && t.nonminimal != []) {
        Alcotest.failf(
          "%d of %d slices are not minimal",
          List.length(t.nonminimal),
          t.checked,
        );
      };
    },
  );

let tests = (
  "Statics.Slicing.Minimality",
  [
    property(
      ~name="core: root synthesis slices are minimal",
      ~direction=`Syn,
      ~foci=Root,
      ~program=typed(~flavour=Core, ~depth=4),
      ~expect_minimal=true,
    ),
    property(
      ~name="core: focused synthesis slices are minimal",
      ~direction=`Syn,
      ~foci=Sample(3),
      ~program=typed(~flavour=Core, ~depth=4),
      ~expect_minimal=true,
    ),
    property(
      ~name="core: analysis slices are minimal",
      ~direction=`Ana,
      ~foci=Sample(3),
      ~program=typed(~flavour=Core, ~depth=4),
      ~expect_minimal=true,
    ),
    property(
      ~name="random hazel without joins: root synthesis minimal",
      ~direction=`Syn,
      ~foci=Root,
      ~program=hazel_random,
      ~expect_minimal=true,
    ),
    property(
      ~name="random hazel without joins: analysis minimal",
      ~direction=`Ana,
      ~foci=Sample(3),
      ~program=hazel_random,
      ~expect_minimal=true,
    ),
    property(
      ~name="control, with if and lists: root synthesis",
      ~direction=`Syn,
      ~foci=Root,
      ~program=typed(~flavour=WithJoins, ~depth=4),
      ~expect_minimal=false,
    ),
  ],
);
