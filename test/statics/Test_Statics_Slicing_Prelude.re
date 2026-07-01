// Type slicing testing framework
open Alcotest;
open Language;
open Util;

module S = Statics.Slice;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

/* Render an expression back to Hazel source so failing reconstruction checks
   read as `let x = 1 in x` rather than an sexp */
let show_exp_src = (e: Exp.t): string =>
  e
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
       _,
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", _);
let testable_exp =
  testable(Fmt.using(show_exp_src, Fmt.string), Exp.fast_equal);

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

let render_any = (a: Any.t): string =>
  a
  |> Haz3lcore.ExpToSegment.any_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
       _,
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", _);

// The subterm (any sort) carrying rep_id `id`, if present.
let find_any = (id: Id.t, e: Exp.t): option(Any.t) => {
  let found = ref(None);
  let note = (a: Any.t) =>
    if (found^ == None && Any.rep_id(a) == id) {
      found := Some(a);
    };
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          note(Exp(e));
          continue(e);
        },
      ~f_pat=
        (continue, p) => {
          note(Pat(p));
          continue(p);
        },
      ~f_typ=
        (continue, t) => {
          note(Typ(t));
          continue(t);
        },
      ~f_tpat=
        (continue, tp) => {
          note(TPat(tp));
          continue(tp);
        },
      e,
    );
  found^;
};

// Ids of every exp/pat/typ/tpat node in a term.
let all_term_ids = (e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let note = (id: Id.t) => acc := [id, ...acc^];
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          note(Exp.rep_id(e));
          continue(e);
        },
      ~f_pat=
        (continue, p) => {
          note(Pat.rep_id(p));
          continue(p);
        },
      ~f_typ=
        (continue, t) => {
          note(Typ.rep_id(t));
          continue(t);
        },
      ~f_tpat=
        (continue, tp) => {
          note(TPat.rep_id(tp));
          continue(tp);
        },
      e,
    );
  acc^;
};

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

let first_fun = (e: Exp.t): Id.t =>
  first(
    "function",
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | Fun(_, _, _, _) => true
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
let tpat_hole = (tp: TPat.t): TPat.t => {
  ...tp,
  term: EmptyHole,
};
let mod_hole = (m: Mod.t): Mod.t => {
  ...m,
  term: EmptyHole,
};
let sig_hole = (s: Sig.t): Sig.t => {
  ...s,
  term: EmptyHole,
};

let rec reconstruct = (omitted: Id.Set.t, e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.Set.mem(Exp.rep_id(e), omitted)) {
          exp_hole(e);
        } else {
          switch (Exp.term_of(e)) {
          | Module(items) => {
              ...e,
              term: Module(List.map(reconstruct_mod(omitted), items)),
            }
          | _ => continue(e)
          };
        },
    ~f_pat=
      (continue, p) =>
        Id.Set.mem(Pat.rep_id(p), omitted) ? pat_hole(p) : continue(p),
    ~f_typ=
      (continue, t) =>
        if (Id.Set.mem(Typ.rep_id(t), omitted)) {
          typ_hole(t);
        } else {
          switch (Typ.term_of(t)) {
          | Sig(items) => {
              ...t,
              term: Sig(List.map(reconstruct_sig(omitted), items)),
            }
          | _ => continue(t)
          };
        },
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    e,
  )
and reconstruct_pat = (omitted: Id.Set.t, p: Pat.t): Pat.t =>
  Pat.map_term(
    ~f_pat=
      (continue, p) =>
        Id.Set.mem(Pat.rep_id(p), omitted) ? pat_hole(p) : continue(p),
    ~f_typ=(_, t) => reconstruct_typ(omitted, t),
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    p,
  )
and reconstruct_typ = (omitted: Id.Set.t, t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_exp=(_, e) => reconstruct(omitted, e),
    ~f_typ=
      (continue, t) =>
        if (Id.Set.mem(Typ.rep_id(t), omitted)) {
          typ_hole(t);
        } else {
          switch (Typ.term_of(t)) {
          | Sig(items) => {
              ...t,
              term: Sig(List.map(reconstruct_sig(omitted), items)),
            }
          | _ => continue(t)
          };
        },
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    t,
  )
and reconstruct_tpat = (omitted: Id.Set.t, tp: TPat.t): TPat.t =>
  TPat.map_term(
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    tp,
  )
and reconstruct_mod = (omitted: Id.Set.t, m: Mod.t): Mod.t =>
  if (Id.Set.mem(IdTagged.rep_id(m), omitted)) {
    mod_hole(m);
  } else {
    Mod.map_term(
      ~f_exp=(_, e) => reconstruct(omitted, e),
      ~f_pat=(_, p) => reconstruct_pat(omitted, p),
      ~f_typ=(_, t) => reconstruct_typ(omitted, t),
      ~f_tpat=(_, tp) => reconstruct_tpat(omitted, tp),
      m,
    );
  }
and reconstruct_sig = (omitted: Id.Set.t, s: Sig.t): Sig.t =>
  if (Id.Set.mem(IdTagged.rep_id(s), omitted)) {
    sig_hole(s);
  } else {
    Sig.map_term(
      ~f_pat=(_, p) => reconstruct_pat(omitted, p),
      ~f_typ=(_, t) => reconstruct_typ(omitted, t),
      ~f_tpat=(_, tp) => reconstruct_tpat(omitted, tp),
      s,
    );
  };

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

let run_exp =
    (~ctx=?, ~focus, ~direction, e: Exp.t, query_src: string): S.result => {
  let ctx =
    switch (ctx) {
    | Some(c) => c
    | None => base_ctx()
    };
  let query = parse_typ(query_src);
  Statics.slice(~ctx, ~focus=Some(focus(e)), ~direction, e, query);
};

let exn_text = (e: exn): string =>
  switch (e) {
  | S.Focus_not_found(_) => "Focus_not_found"
  | S.Wrong_focus_sort => "Wrong_focus_sort"
  | S.Incompatible_query(ty) =>
    "Incompatible_query(" ++ render_any(Typ(ty)) ++ ")"
  | Failure(s) => "Failure(" ++ s ++ ")"
  | Invalid_argument(s) => "Invalid_argument(" ++ s ++ ")"
  | _ => Printexc.to_string(e)
  };

let failures: ref(list(string)) = ref([]);

let soft_check = (testable, label, expected, actual): unit =>
  if (!Alcotest.equal(testable, expected, actual)) {
    failures :=
      failures^
      @ [
        Format.asprintf(
          "%s\n    expected: %a\n    actual:   %a",
          label,
          Alcotest.pp(testable),
          expected,
          Alcotest.pp(testable),
          actual,
        ),
      ];
  };

let label =
    (~src_str: string, ~query_src: string, ~focus: option(string)=None, what)
    : string => {
  let focus_str =
    switch (focus) {
    | None
    | Some("") => ""
    | Some(s) => Printf.sprintf(" focus %s", s)
    };
  Printf.sprintf("[slice %s @ %s%s] %s", src_str, query_src, focus_str, what);
};

// Source of the focused subterm, unless the focus is the whole term.
let focus_str = (e: Exp.t, focus_id: Id.t): option(string) =>
  Exp.rep_id(e) == focus_id
    ? None : Option.map(render_any, find_any(focus_id, e));

let check_reconstruct =
    (
      ~result: S.result,
      ~src: Exp.t,
      ~lab: string => string,
      ~expected: string,
    )
    : unit =>
  soft_check(
    testable_exp,
    lab("reconstructed slice = " ++ expected),
    parse_exp(expected),
    reconstruct(result.omitted, src),
  );

let check_assumptions =
    (
      ~result: S.result,
      ~lab: string => string,
      expected: list((string, string)),
    )
    : unit =>
  List.iter(
    ((name, ty_src)) =>
      soft_check(
        option(testable_typ),
        lab("minimal assumption " ++ name),
        Some(parse_typ(ty_src)),
        VarMap.lookup(result.gamma, name),
      ),
    expected,
  );

let check_context =
    (
      ~result: S.result,
      ~lab: string => string,
      ~aliases: list((string, string)),
      ~constructors: list((string, string)),
    )
    : unit => {
  List.iter(
    ((name, ty_src)) =>
      soft_check(
        option(testable_typ),
        lab("minimal alias " ++ name),
        Some(parse_typ(ty_src)),
        Ctx.lookup_alias(result.context, name),
      ),
    aliases,
  );
  List.iter(
    ((name, ty_src)) =>
      soft_check(
        option(testable_typ),
        lab("minimal constructor " ++ name),
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
      failures := [];
      let src_exp = parse_exp(src);
      let lab =
        label(
          ~src_str=src,
          ~query_src,
          ~focus=focus_str(src_exp, focus(src_exp)),
        );
      let result =
        switch (run_exp(~ctx?, ~focus, ~direction, src_exp, query_src)) {
        | result => result
        | exception e =>
          Alcotest.failf(
            "%s\n    raised: %s",
            lab("slice raised"),
            exn_text(e),
          )
        };
      check_reconstruct(~result, ~src=src_exp, ~lab, ~expected);
      check_assumptions(~result, ~lab, assumptions);
      check_context(~result, ~lab, ~aliases, ~constructors);
      if (failures^ != []) {
        Alcotest.failf(
          "%d slice check(s) failed:\n%s",
          List.length(failures^),
          String.concat("\n", failures^),
        );
      };
    },
  );

let synthesis_case = slicing_case(~direction=`Syn);
let analysis_case = slicing_case(~direction=`Ana);
