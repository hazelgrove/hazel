// Type slicing testing framework
open Alcotest;
open Language;
open Util;

module S = Statics.Slice;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);
let testable_exp = testable(Fmt.using(Exp.show, Fmt.string), Exp.fast_equal);

let parse_exp = (s: string): Exp.t =>
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };

let strip_parens = (t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_typ=
      (continue, t) =>
        switch (Typ.term_of(t)) {
        | Parens(inner) => continue(inner)
        | _ => continue(t)
        },
    t,
  );

let parse_typ = (s: string): Typ.t => {
  let e = parse_exp("? : (" ++ s ++ ")");
  let found = ref(None);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          switch (Exp.term_of(e)) {
          | Asc(_, ty) =>
            if (found^ == None) {
              found := Some(ty);
            }
          | _ => ()
          };
          continue(e);
        },
      e,
    );
  switch (found^) {
  | Some(ty) => strip_parens(ty)
  | None => Alcotest.fail("Failed to parse type query: " ++ s)
  };
};

let collect_exp_ids = (pred: Exp.t => bool, e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          if (pred(e)) {
            acc := acc^ @ [Exp.rep_id(e)];
          };
          continue(e);
        },
      e,
    );
  acc^;
};

let collect_pat_ids = (pred: Pat.t => bool, e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let _ =
    Exp.map_term(
      ~f_pat=
        (continue, p) => {
          if (pred(p)) {
            acc := acc^ @ [Pat.rep_id(p)];
          };
          continue(p);
        },
      e,
    );
  acc^;
};

let first = (what: string, ids: list(Id.t)): Id.t =>
  switch (ids) {
  | [id, ..._] => id
  | [] => Alcotest.fail("focus not found: " ++ what)
  };

let whole = (e: Exp.t): Id.t => Exp.rep_id(e);

let exp_var = (e: Exp.t, name: string): Id.t =>
  first(
    "exp var " ++ name,
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | Var(v) => v == name
        | _ => false
        },
      e,
    ),
  );

let pat_var = (e: Exp.t, name: string): Id.t =>
  first(
    "pat var " ++ name,
    collect_pat_ids(
      p =>
        switch (Pat.term_of(p)) {
        | Var(v) => v == name
        | _ => false
        },
      e,
    ),
  );

let pat_wild = (e: Exp.t): Id.t =>
  first(
    "wildcard pattern",
    collect_pat_ids(
      p =>
        switch (Pat.term_of(p)) {
        | Wild => true
        | _ => false
        },
      e,
    ),
  );

let int_lits = (e: Exp.t): list(Id.t) =>
  collect_exp_ids(
    x =>
      switch (Exp.term_of(x)) {
      | Atom(Int(_)) => true
      | _ => false
      },
    e,
  );

let nth_int = (e: Exp.t, k: int): Id.t =>
  switch (List.nth_opt(int_lits(e), k)) {
  | Some(id) => id
  | None =>
    Alcotest.fail("int literal #" ++ string_of_int(k) ++ " not found")
  };

let first_int = (e: Exp.t): Id.t => nth_int(e, 0);

let first_bool = (e: Exp.t): Id.t =>
  first(
    "bool literal",
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | Atom(Bool(_)) => true
        | _ => false
        },
      e,
    ),
  );

let first_binop = (e: Exp.t): Id.t =>
  first(
    "binary operator",
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | BinOp(_) => true
        | _ => false
        },
      e,
    ),
  );

let exp_hole = (e: Exp.t): Exp.t => {
  ...e,
  term: EmptyHole,
};
let pat_hole = (p: Pat.t): Pat.t => {
  ...p,
  term: EmptyHole,
};
let typ_hole = (t: Typ.t): Typ.t => {
  ...t,
  term: Unknown(Hole(EmptyHole)),
};

let reconstruct = (omitted: Id.Set.t, e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        Id.Set.mem(Exp.rep_id(e), omitted) ? exp_hole(e) : continue(e),
    ~f_pat=
      (continue, p) =>
        Id.Set.mem(Pat.rep_id(p), omitted) ? pat_hole(p) : continue(p),
    ~f_typ=
      (continue, t) =>
        Id.Set.mem(Typ.rep_id(t), omitted) ? typ_hole(t) : continue(t),
    e,
  );

let base_ctx = () => Builtins.ctx_init(Some(Int));

let var_entry = (name: string, typ: Typ.t): Ctx.entry =>
  VarEntry({
    name,
    id: Id.mk(),
    typ,
    custom_statics: None,
  });

let ctx_var = (~ctx=?, name: string, ty_src: string): Ctx.t => {
  let ctx =
    switch (ctx) {
    | Some(c) => c
    | None => base_ctx()
    };
  Ctx.extend(ctx, var_entry(name, parse_typ(ty_src)));
};

let prelude_ctx = (prelude_src: string): Ctx.t => {
  let e = parse_exp(prelude_src ++ " ?");
  let (m, _) = Statics.mk(CoreSettings.on, base_ctx(), e);
  let hole =
    first(
      "prelude trailing hole",
      collect_exp_ids(
        x =>
          switch (Exp.term_of(x)) {
          | EmptyHole => true
          | _ => false
          },
        e,
      ),
    );
  switch (Statics.Map.lookup_exp(hole, m)) {
  | Some({ctx, _}) => ctx
  | None => base_ctx()
  };
};

let run =
    (~ctx=?, ~focus, ~direction, src: string, query_src: string): S.result => {
  let ctx =
    switch (ctx) {
    | Some(c) => c
    | None => base_ctx()
    };
  let e = parse_exp(src);
  let query = parse_typ(query_src);
  Statics.slice(~ctx, ~focus=Some(focus(e)), ~direction, e, query);
};

let check_reconstruct =
    (~result: S.result, ~src: string, ~expected: string): unit => {
  let recon = reconstruct(result.omitted, parse_exp(src));
  check(
    testable_exp,
    "reconstructed slice = " ++ expected,
    parse_exp(expected),
    recon,
  );
};

let check_assumptions =
    (~result: S.result, expected: list((string, string))): unit =>
  List.iter(
    ((name, ty_src)) =>
      check(
        option(testable_typ),
        "minimal assumption " ++ name,
        Some(parse_typ(ty_src)),
        VarMap.lookup(result.gamma, name),
      ),
    expected,
  );

let check_context =
    (
      ~result: S.result,
      ~aliases: list((string, string)),
      ~constructors: list((string, string)),
    )
    : unit => {
  List.iter(
    ((name, ty_src)) =>
      check(
        option(testable_typ),
        "minimal alias " ++ name,
        Some(parse_typ(ty_src)),
        Ctx.lookup_alias(result.context, name),
      ),
    aliases,
  );
  List.iter(
    ((name, ty_src)) =>
      check(
        option(testable_typ),
        "minimal constructor " ++ name,
        Some(parse_typ(ty_src)),
        Option.map(
          (v: Ctx.var_entry) => v.typ,
          Ctx.lookup_ctr(result.context, name),
        ),
      ),
    constructors,
  );
};

let slicing_case =
    (
      ~ctx=?,
      ~direction=`Syn,
      ~focus=whole,
      ~assumptions=[],
      ~aliases=[],
      ~constructors=[],
      name: string,
      src: string,
      query_src: string,
      expected: string,
    )
    : test_case(unit) =>
  test_case(
    name,
    `Quick,
    _ => {
      let result = run(~ctx?, ~focus, ~direction, src, query_src);
      check_reconstruct(~result, ~src, ~expected);
      check_assumptions(~result, assumptions);
      check_context(~result, ~aliases, ~constructors);
    },
  );

let synthesis_case = slicing_case(~direction=`Syn);
let analysis_case = slicing_case(~direction=`Ana);
