open Language;
open Test_Statics_Slicing_Prelude;

let show_typ = (ty: Typ.t): string => render_any(Typ(ty));

let show_gamma = (gamma: Ctx.t): string =>
  String.concat(
    ", ",
    List.filter_map(
      (entry: Ctx.entry) =>
        switch (entry) {
        | VarEntry({name, typ, _}) => Some(name ++ " : " ++ show_typ(typ))
        | ConstructorEntry({name, typ, _}) =>
          Some(name ++ " :: " ++ show_typ(typ))
        | TVarEntry({name, kind: Singleton(ty), _}) =>
          Some("type " ++ name ++ " = " ++ show_typ(ty))
        | TVarEntry({name, _}) => Some("type " ++ name)
        | LivelitEntry(_) => None
        },
      gamma.entries,
    ),
  );

let probe =
    (
      ~ctx=?,
      ~direction=`Ana,
      ~focus=whole,
      name: string,
      src: string,
      queries: list(string),
    )
    : Alcotest.test_case(unit) =>
  Alcotest.test_case(
    name,
    `Quick,
    _ => {
      let e = parse_exp(src);
      let ctx =
        switch (ctx) {
        | Some(c) => c
        | None => base_ctx()
        };
      let id = focus(e);
      let (m, _) = Statics.mk(CoreSettings.on, ctx, e);
      print_endline("### " ++ name ++ ":  " ++ src);
      switch (Statics.Map.lookup_exp(id, m)) {
      | Some(info) =>
        print_endline(
          "  focus exp = "
          ++ (
            switch (focus_str(e, id)) {
            | Some(s) => s
            | None => "<whole>"
            }
          ),
        );
        print_endline("  ana = " ++ show_typ(info.Info.ana));
        print_endline("  ty  = " ++ show_typ(info.Info.ty));
        print_endline("  syn = " ++ show_typ(info.Info.elab_syn_ty));
      | None =>
        switch (Statics.Map.lookup_pat(id, m)) {
        | Some(info) =>
          print_endline("  focus pat");
          print_endline("  ana = " ++ show_typ(info.Info.ana));
          print_endline("  ty  = " ++ show_typ(info.Info.ty));
        | None => print_endline("  no info for focus")
        }
      };
      List.iter(
        q =>
          switch (run_exp(~ctx, ~focus, ~direction, e, q)) {
          | result =>
            print_endline(
              "  @ "
              ++ q
              ++ "  ->  "
              ++ show_exp_src(reconstruct(result.omitted, e))
              ++ "   gamma{"
              ++ show_gamma(result.gamma)
              ++ "}",
            )
          | exception exn => print_endline("  @ " ++ q ++ "  ->  " ++ exn_text(exn))
          },
        queries,
      );
    },
  );

let fun_binding = (name: string, e: Exp.t): Id.t =>
  first(
    "function binding " ++ name,
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | Fun(p, _, _, _) => List.mem(name, Pat.bound_vars(p))
        | _ => false
        },
      e,
    ),
  );

let ex2_probe_src = "type Option(A) = None + Some(A) in type Digit = Zero + One in type Pin = (Digit, Digit) in let seq = abs A -> abs B -> fun (p : String -> Option((String, A))) -> fun (f : A -> Option((String, B))) -> fun (s : String) -> case p(s) | None => None | Some((s2, a)) => f(a) end in let digit_parser = fun (s : String) -> Some((s, Zero)) in let parse_pin = fun (s : String) -> seq@<Digit>@<Pin>(digit_parser)(fun (d1 : Digit) -> seq@<Digit>@<Pin>(digit_parser)(fun (d2 : Digit) -> fun (s2 : String) -> Some((s2, (d1, d2))))(s))(s) in parse_pin(\"12\")";

let validate = (name: string, src: string): Alcotest.test_case(unit) =>
  Alcotest.test_case(
    name,
    `Quick,
    _ => {
      let e = parse_exp(src);
      let (m, _) = Statics.mk(CoreSettings.on, base_ctx(), e);
      print_endline("### " ++ name ++ ":  " ++ src);
      switch (Statics.Map.lookup_exp(Exp.rep_id(e), m)) {
      | Some(info) =>
        print_endline("  syn = " ++ show_typ(info.Info.elab_syn_ty));
        print_endline("  ty  = " ++ show_typ(info.Info.ty));
      | None => print_endline("  no root info")
      };
      let marks =
        Id.Map.fold(
          (_, info, acc) => acc @ Info.marks_of(info),
          m,
          [],
        );
      print_endline(
        "  marks = "
        ++ String.concat(
             "; ",
             List.map(mark => Mark.show(mark), marks),
           ),
      );
    },
  );

let examples = [
  probe(
    ~focus=e => exp_ctor(e, "Z"),
    "probe-fix-sum-body",
    "fix Z -> Z",
    ["+ Z + ?", "?"],
  ),
  probe(
    ~focus=e => fix_body_int(e, 0),
    "probe-fix-atom-body",
    "fix 0 -> 0",
    ["Int", "?"],
  ),
  probe(
    ~focus=e => exp_var(e, "x"),
    "probe-fix-ann-body",
    "fix (x : Int) -> x",
    ["Int", "?"],
  ),
  probe(
    ~focus=first_int,
    "probe-fix-ann-body-const",
    "fix (x : Int) -> 0",
    ["Int", "?"],
  ),
  probe(
    ~focus=first_exp_hole,
    "probe-fix-annotated-hole",
    "fix(Z:(? -> ?)) -> ?",
    ["? -> ?", "?"],
  ),
  probe(
    ~focus=first_empty_list,
    "probe-cons-tail-head",
    "0:: [[]]",
    ["Int", "?"],
  ),
  probe(
    ~ctx=ctx_var("x", "(A=())"),
    ~focus=e => exp_var(e, "x"),
    "probe-projection-base",
    "x.A",
    ["(A=?)", "?"],
  ),
  probe(
    ~focus=e => cons_head_ctor(e, "Z"),
    "probe-sum-list-cons",
    "Z:: Z",
    ["[+ Z + ?]", "+ Z + ?", "?"],
  ),
  probe(
    ~ctx=ctx_var("f", "Int -> Bool"),
    ~focus=e => exp_var(e, "f"),
    "probe-app-function",
    "(f(1) : Bool)",
    ["Bool", "? -> Bool", "? -> ?", "Int -> Bool", "?"],
  ),
  probe(
    ~focus=e => exp_var(e, "a"),
    "probe-let-list-pattern",
    "let [] = a in (())",
    ["[?]", "?"],
  ),
  probe(
    ~ctx=ctx_var("f", "Int -> Bool"),
    ~focus=first_int,
    "probe-app-arg",
    "f(1)",
    ["Int", "?"],
  ),
  probe(
    ~direction=`Syn,
    ~focus=e => exp_ctor(e, "M"),
    "probe-module-keyword-receiver",
    "module M = { let x = 1; let y = true } in M.x",
    ["(x=Int, y=?)", "Int"],
  ),
  probe(
    ~direction=`Syn,
    "probe-module-keyword-whole",
    "module M = { let x = 1; let y = true } in M.x",
    ["Int"],
  ),
  probe(
    ~direction=`Syn,
    ~focus=e => exp_var(e, "m"),
    "probe-signature-receiver",
    "let m : { let x : Int; let y : Bool } = { let x = 1; let y = true } in m.x",
    ["(x=Int, y=?)", "Int"],
  ),
  probe(
    ~direction=`Ana,
    ~focus=fun_binding("d2"),
    "probe-ex2-errfn",
    ex2_probe_src,
    ["Digit -> String -> Option((String, Pin))", "Digit -> ?", "?"],
  ),
  probe(
    ~direction=`Syn,
    ~focus=e => exp_var(e, "seq"),
    "probe-ex2-seq",
    ex2_probe_src,
    ["?"],
  ),
  validate(
    "validate-sig-type-member-full",
    "let m : { type T = Int; let x : T } = { type T = Int; let x = (1 : T) } in m.x",
  ),
  validate(
    "validate-sig-type-member-engine-slice",
    "let m : { type T = Int; let x : T } = ({ type T = ?; let x = (? : T) }) in m.x",
  ),
  validate(
    "validate-sig-type-member-expected-slice",
    "let m : { type T = Int; let x : T } = ({ type T = Int; let x = (? : T) }) in m.x",
  ),
  validate(
    "validate-ctor-shadow-engine-slice",
    "type T1 = +A(Int) in type ? = ? in let A(?) : T1 = ? in ?",
  ),
  probe(
    ~ctx=prelude_ctx("type Option(A) = None + Some(A) in"),
    ~direction=`Syn,
    ~focus=e => exp_ctor(e, "Some"),
    "probe-ctor-node",
    "Some(1)",
    ["Option(Int)"],
  ),
  probe(
    ~ctx=prelude_ctx("type Option(A) = None + Some(A) in"),
    ~direction=`Syn,
    "probe-ctor-ap-node",
    "Some(1)",
    ["Option(Int)"],
  ),
  probe(
    ~ctx=prelude_ctx("type Option(A) = None + Some(A) in"),
    ~direction=`Syn,
    "probe-ctor-fun-node",
    "fun (x : Int) -> Some(x)",
    ["Int -> Option(Int)"],
  ),
  probe(
    ~direction=`Ana,
    ~focus=e => List.nth(collect_exp_ids(_ => true, e), 1),
    "probe-cons-fix-focus1",
    "Z:: (fix Z -> [])",
    ["+ Z + ?", "[+ Z + ?]", "?"],
  ),
  probe(
    ~direction=`Ana,
    ~focus=e => List.nth(collect_exp_ids(_ => true, e), 2),
    "probe-cons-fix-focus2",
    "Z:: (fix Z -> [])",
    ["[+ Z + ?]", "?"],
  ),
  probe(
    ~direction=`Ana,
    ~focus=e => List.nth(collect_exp_ids(_ => true, e), 3),
    "probe-cons-fix-focus3",
    "Z:: (fix Z -> [])",
    ["[+ Z + ?]", "+ Z + ?", "?"],
  ),
  probe(
    ~direction=`Ana,
    ~focus=e => List.nth(collect_exp_ids(_ => true, e), 2),
    "probe-cons-module-tail",
    "\"\":: (module B = false in (()))",
    ["[String]", "?"],
  ),
  probe(
    ~direction=`Syn,
    "probe-module-gap-chain",
    "module B = ? in (())",
    ["()", "?"],
  ),
  probe(
    ~direction=`Syn,
    "probe-typabs-tuple",
    "abs y, x -> []",
    ["poly y, x -> [?]", "?"],
  ),
  validate(
    "validate-ctor-implicit-inline",
    "type Option(A) = None + Some(A) in Some(1)",
  ),
  validate(
    "validate-ctor-implicit-case",
    "type Option(A) = None + Some(A) in type Digit = Zero + One in let f = fun c : String -> case c | \"0\" => Some(Zero) | _ => None end in f(\"0\")",
  ),
  validate(
    "validate-ctor-explicit-case",
    "type Option(A) = None + Some(A) in type Digit = Zero + One in let f = fun c : String -> case c | \"0\" => Some@<Digit>(Zero) | _ => None end in f(\"0\")",
  ),
  probe(
    ~direction=`Syn,
    ~focus=e => exp_ctor(e, "Some"),
    "probe-ctor-inline-alias",
    "type Option(A) = None + Some(A) in Some(1)",
    ["Option(Int)", "?"],
  ),
  probe(
    ~ctx=prelude_ctx("type Option(A) = None + Some(A) in"),
    ~direction=`Syn,
    "probe-option-alias-query-poly",
    "abs A -> fun (x : A) -> Some(x)",
    ["poly A -> A -> Option(A)", "poly A -> A -> Option(?)", "?"],
  ),
  probe(
    ~ctx=prelude_ctx("type Option(A) = None + Some(A) in"),
    ~direction=`Syn,
    ~focus=e => exp_ctor(e, "Some"),
    "probe-option-alias-query-ctor",
    "abs A -> fun (x : A) -> Some(x)",
    ["A -> Option(A)", "Option(A)", "?"],
  ),
  probe(
    ~direction=`Syn,
    ~focus=e => List.nth(collect_exp_ids(_ => true, e), 0),
    "probe-if-unit-branches",
    "if false then (()) else ?",
    ["()", "?"],
  ),
  probe(
    ~direction=`Syn,
    ~focus=e => List.nth(collect_exp_ids(_ => true, e), 5),
    "probe-module-item-focus",
    "{ module A = false; type y(x) = (()); let x = 0.044323; module A = false; type x = (+ A); type x(x) = String; 0.000077; A; let y = A; module A = ? }",
    ["Bool", "?"],
  ),
  probe(
    ~direction=`Syn,
    "probe-fix-list-pat-syn",
    "fix [] -> (type x = Bool in abs ? -> []; (()))",
    ["[?]", "?"],
  ),
  probe(
    ~direction=`Syn,
    "probe-fix-ann-pat-syn",
    "fix (x : Int) -> x",
    ["Int", "?"],
  ),
];

let tests = ("Statics.Slicing.Probe", examples);
