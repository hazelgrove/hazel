// Extended worked-example and branch-join slicing tests
open Test_Statics_Slicing_Prelude;
open Language;

let str_lit = (s: string, e: Exp.t): Id.t =>
  first(
    "string literal " ++ s,
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | Atom(String(v)) => v == s
        | _ => false
        },
      e,
    ),
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

let exp_ctor = (name: string, e: Exp.t): Id.t =>
  first(
    "constructor " ++ name,
    collect_exp_ids(
      x =>
        switch (Exp.term_of(x)) {
        | Constructor(v, _) => v == name
        | _ => false
        },
      e,
    ),
  );

let ex1_src = "type Option = typfun A -> None + Some(A) in type Digit = Zero + One + Two + Three + Four + Five + Six + Seven + Eight + Nine in let parse_digit = fun c : String -> case c | \"0\" => Some(Zero) | \"1\" => Some(One) | \"2\" => Some(Two) | \"3\" => Some(Three) | \"4\" => Some(Four) | \"5\" => Some(Five) | \"6\" => Some(Six) | \"7\" => Some(Seven) | \"8\" => Some(Eight) | \"9\" => Some(Nine) | _ => None end in parse_digit(\"5\")";

let parse_digit_examples = [
  synthesis_case(
    ~focus=e => exp_var(e, "parse_digit"),
    "ex1-parse-digit-syn",
    ex1_src,
    "String -> Option(Digit)",
    "type Option = typfun ? -> ? + Some(?) in type ? = ? in let parse_digit = fun ? : String -> case ? | ? => Some(?) | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? end in parse_digit(?)",
  ),
  analysis_case(
    ~focus=str_lit("5"),
    "ex1-parse-digit-ana",
    ex1_src,
    "String",
    "type ? = ? in type ? = ? in let parse_digit = fun ? : String -> ? in parse_digit(?)",
  ),
];

let ex2_src = "type Option = typfun A -> None + Some(A) in type Digit = Zero + One in type Pin = (Digit, Digit) in let seq = abs A -> abs B -> fun (p : String -> Option((String, A))) -> fun (f : A -> Option((String, B))) -> fun (s : String) -> case p(s) | None => None | Some((s2, a)) => f(a) end in let digit_parser = fun (s : String) -> Some((s, Zero)) in let parse_pin = fun (s : String) -> seq@<Digit>@<Pin>(digit_parser)(fun (d1 : Digit) -> seq@<Digit>@<Pin>(digit_parser)(fun (d2 : Digit) -> fun (s2 : String) -> Some((s2, (d1, d2))))(s))(s) in parse_pin(\"12\")";

let seq_pin_examples = [
  synthesis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-syn",
    ex2_src,
    "Digit -> String -> Option((String, Pin))",
    "type Option = typfun ? -> ? + Some(?) in type Digit = ? in type ? = ? in let ? = ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun (? : Digit) -> fun (? : String) -> Some(?))(?))(?) in ?",
  ),
  synthesis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-syn-refined",
    ex2_src,
    "? -> ? -> ?",
    "type ? = ? in type ? = ? in type ? = ? in let ? = ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> fun ? -> ?)(?))(?) in ?",
  ),
  analysis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-ana",
    ex2_src,
    "Digit -> Option((String, Pin))",
    "type Option = ? in type Digit = ? in type Pin = ? in let seq = abs A -> abs B -> fun ? -> fun (? : A -> Option((String, B))) -> ? in let ? = ? in let ? = fun ? -> ?(fun ? -> seq@<Digit, Pin>(?)(?)(?))(?) in ?",
  ),
  analysis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-ana-refined",
    ex2_src,
    "? -> Option(?)",
    "type Option = ? in type ? = ? in type ? = ? in let seq = abs ? -> abs ? -> fun ? -> fun (? : ? -> Option(?)) -> ? in let ? = ? in let ? = fun ? -> ?(fun ? -> seq@<?, ?>(?)(?)(?))(?) in ?",
  ),
];

let demo_src = "type Option = typfun A -> None + Some(A) in type Digit = Zero + One + Two + Three + Four + Five + Six + Seven + Eight + Nine in type Pin = (Digit, Digit, Digit, Digit) in let parse_digit = fun c : String -> case c | \"0\" => Some@<Digit>(Zero) | \"1\" => Some(One) | \"2\" => Some(Two) | \"3\" => Some(Three) | \"4\" => Some(Four) | \"5\" => Some(Five) | \"6\" => Some(Six) | \"7\" => Some(Seven) | \"8\" => Some(Eight) | \"9\" => Some(Nine) | _ => None end in let seq = abs A -> abs B -> fun (p : String -> Option((String, A))) -> fun (f : A -> Option((String, B))) -> fun (s : String) -> case p(s) | None => None | Some((s2, a)) => f(a) end in let digit_parser = fun (s : String) -> case parse_digit(s) | Some(d) => Some((s, d)) | None => None end in let parse_pin = fun (s : String) -> seq@<Digit>@<Pin>(digit_parser)(fun (d1 : Digit) -> seq@<Digit>@<Pin>(digit_parser)(fun (d2 : Digit) -> seq@<Digit>@<Pin>(digit_parser)(fun (d3 : Digit) -> seq@<Digit>@<Pin>(digit_parser)(fun (d4 : Digit) -> fun (s2 : String) -> Some((s2, (d1, d2, d3, d4))))(s))(s))(s))(s) in parse_pin(\"1234\")";

let demo_slice_case =
    (
      ~direction: [
         | `Syn
         | `Ana
       ],
      ~focus: Exp.t => Id.t,
      ~query: option(string),
      name: string,
      expected: string,
    )
    : Alcotest.test_case(unit) =>
  Alcotest.test_case(
    name,
    `Quick,
    () => {
      let e = parse_exp(demo_src);
      let (m, _) = Statics.mk(CoreSettings.on, base_ctx(), e);
      let focus_id = focus(e);
      let query =
        switch (query) {
        | Some(q) => parse_typ(q)
        | None =>
          switch (Statics.Map.lookup_exp(focus_id, m)) {
          | Some(info) => info.Info.elab_syn_ty
          | None => Alcotest.fail("focus has no synthesised type: " ++ name)
          }
        };
      let result =
        Statics.slice(
          ~ctx=base_ctx(),
          ~focus=Some(focus_id),
          ~direction,
          e,
          query,
        );
      Alcotest.check(
        testable_exp,
        name,
        parse_exp(expected),
        reconstruct(result.omitted, e),
      );
    },
  );

let demo_syn_case = demo_slice_case(~direction=`Syn, ~query=None);
let demo_ana_case = (~focus, ~query, name, expected) =>
  demo_slice_case(
    ~direction=`Ana,
    ~focus,
    ~query=Some(query),
    name,
    expected,
  );

let errfn = e => fun_binding("d4", e);

let demo_examples = [
  synthesis_case("demo-scratchpad", demo_src, "?", "?"),
  demo_syn_case(
    ~focus=ctor_arg_ap("Five"),
    "demo-focus-some-five",
    "type Option = typfun ? -> ? + Some(?) in type ? = ? in type ? = ? in let ? = fun ? -> case ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => Some(?) | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? end in ?",
  ),
  demo_syn_case(
    ~focus=e => exp_var(e, "seq"),
    "demo-focus-seq-use",
    "type Option = typfun ? -> None + ? in type ? = ? in type ? = ? in let ? = ? in let seq = abs A -> abs B -> fun (? : String -> Option((String, A))) -> fun (? : A -> Option((String, B))) -> fun (? : String) -> case ? | ? => None | ? => ? end in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> ?(fun ? -> seq@<?>@<?>(?)(?)(?))(?))(?))(?) in ?",
  ),
  demo_syn_case(
    ~focus=e => exp_var(e, "d1"),
    "demo-focus-d1",
    "type ? = ? in type Digit = ? in type ? = ? in let ? = ? in let ? = ? in let ? = ? in let ? = fun ? -> ?(fun (d1 : Digit) -> ?(fun ? -> ?(fun ? -> ?(fun ? -> fun ? -> ?((?, (d1, ?, ?, ?))))(?))(?))(?))(?) in ?",
  ),
  demo_syn_case(
    ~focus=e => exp_var(e, "f"),
    "demo-focus-f-use",
    "type Option = ? in type ? = ? in type ? = ? in let ? = ? in let ? = abs A -> abs B -> fun ? -> fun (f : A -> Option((String, B))) -> fun ? -> case ? | ? => ? | ? => f(?) end in ?",
  ),
  demo_syn_case(
    ~focus=errfn,
    "demo-focus-errfn-syn",
    "type Option = typfun ? -> ? + Some(?) in type Digit = ? in type ? = ? in let ? = ? in let ? = ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> ?(fun ? -> ?(fun (? : Digit) -> fun (? : String) -> Some(?))(?))(?))(?))(?) in ?",
  ),
  demo_ana_case(
    ~focus=errfn,
    ~query="Digit -> Option((String, Pin))",
    "demo-focus-errfn-ana-full",
    "type Option = ? in type Digit = ? in type Pin = ? in let ? = ? in let seq = abs A -> abs B -> fun ? -> fun (? : A -> Option((String, B))) -> ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> ?(fun ? -> seq@<Digit>@<Pin>(?)(?)(?))(?))(?))(?) in ?",
  ),
  demo_ana_case(
    ~focus=errfn,
    ~query="? -> Option((String, Pin))",
    "demo-focus-errfn-ana-fold-digit",
    "type Option = ? in type ? = ? in type Pin = ? in let ? = ? in let seq = abs ? -> abs B -> fun ? -> fun (? : ? -> Option((String, B))) -> ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> ?(fun ? -> seq@<?>@<Pin>(?)(?)(?))(?))(?))(?) in ?",
  ),
  demo_ana_case(
    ~focus=errfn,
    ~query="Digit -> Option((?, Pin))",
    "demo-focus-errfn-ana-fold-string",
    "type Option = ? in type Digit = ? in type Pin = ? in let ? = ? in let seq = abs A -> abs B -> fun ? -> fun (? : A -> Option((?, B))) -> ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> ?(fun ? -> seq@<Digit>@<Pin>(?)(?)(?))(?))(?))(?) in ?",
  ),
  demo_ana_case(
    ~focus=errfn,
    ~query="Digit -> Option((String, ?))",
    "demo-focus-errfn-ana-fold-pin",
    "type Option = ? in type Digit = ? in type ? = ? in let ? = ? in let seq = abs A -> abs ? -> fun ? -> fun (? : A -> Option((String, ?))) -> ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> ?(fun ? -> seq@<Digit>@<?>(?)(?)(?))(?))(?))(?) in ?",
  ),
  demo_ana_case(
    ~focus=errfn,
    ~query="?",
    "demo-focus-errfn-ana-gap",
    "type ? = ? in type ? = ? in type ? = ? in let ? = ? in let seq = abs ? -> abs ? -> fun ? -> fun (? : ?) -> ? in let ? = ? in let ? = fun ? -> ?(fun ? -> ?(fun ? -> ?(fun ? -> seq@<?>@<?>(?)(?)(?))(?))(?))(?) in ?",
  ),
  demo_ana_case(
    ~focus=e => pat_var(e, "d1"),
    ~query="Digit",
    "demo-focus-d1-binding-ana",
    "type ? = ? in type Digit = ? in type ? = ? in let ? = ? in let ? = ? in let ? = ? in let ? = fun ? -> ?(fun (? : Digit) -> ?)(?) in ?",
  ),
];

let ctor_alias_examples = [
  synthesis_case(
    ~focus=e => exp_var(e, "x"),
    "sum-name-only-folds",
    "type T = A + B + C in let x : T = ? in x",
    "T",
    "type T = ? in let x : T = ? in x",
  ),
  synthesis_case(
    ~focus=exp_ctor("A"),
    "ctor-alias-left",
    "type T = A + B in A",
    "T",
    "type T = A + ? in A",
  ),
  synthesis_case(
    ~focus=exp_ctor("B"),
    "ctor-alias-right",
    "type T = A + B in B",
    "T",
    "type T = ? + B in B",
  ),
];

let either_prelude = "type Either = typfun A -> typfun B -> Left(A) + Right(B) in";

let if_joins = [
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "if-join-product-full",
    "if c then (1, ?) else (?, 2)",
    "(Int, Int)",
    "if ? then (1, ?) else (?, 2)",
  ),
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "if-join-product-left",
    "if c then (1, ?) else (?, 2)",
    "(Int, ?)",
    "if ? then (1, ?) else ?",
  ),
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "if-join-product-right",
    "if c then (1, ?) else (?, 2)",
    "(?, Int)",
    "if ? then ? else (?, 2)",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    ~aliases=[("Either", "typfun A -> typfun B -> Left(A) + Right(B)")],
    "if-join-either-full",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "Either(Int, Bool)",
    "if ? then Left@<Int, ?>(?) else Right@<?, Bool>(?)",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    "if-join-either-explicit-sum-full",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "+ Left(Int) + Right(Bool)",
    "if ? then Left@<Int, ?>(?) else Right@<?, Bool>(?)",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    "if-join-either-explicit-sum-left-gap",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "+ Left(Int) + ?",
    "if ? then Left@<Int, ?>(?) else ?",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    "if-join-either-explicit-sum-left-shaped-gap",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "+ Left(Int) + Right(?)",
    "if ? then Left@<Int, ?>(?) else ?",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    ~aliases=[("Either", "typfun A -> typfun B -> Left(A) + ?")],
    "if-join-either-left",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "Either(Int, ?)",
    "if ? then Left@<Int, ?>(?) else ?",
  ),
];

let case_joins = [
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "case-join-product-full",
    "case c | true => (1, ?) | false => (?, 2) end",
    "(Int, Int)",
    "case ? | ? => (1, ?) | ? => (?, 2) end",
  ),
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "case-join-product-left",
    "case c | true => (1, ?) | false => (?, 2) end",
    "(Int, ?)",
    "case ? | ? => (1, ?) | ? => ? end",
  ),
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "case-join-product-right",
    "case c | true => (1, ?) | false => (?, 2) end",
    "(?, Int)",
    "case ? | ? => ? | ? => (?, 2) end",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    "case-join-either-full",
    "case c | true => Left@<Int, ?>(1) | false => Right@<?, Bool>(true) end",
    "Either(Int, Bool)",
    "case ? | ? => Left@<Int, ?>(?) | ? => Right@<?, Bool>(?) end",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    "case-join-either-left",
    "case c | true => Left@<Int, ?>(1) | false => Right@<?, Bool>(true) end",
    "Either(Int, ?)",
    "case ? | ? => Left@<Int, ?>(?) | ? => ? end",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    "case-join-either-right",
    "case c | true => Left@<Int, ?>(1) | false => Right@<?, Bool>(true) end",
    "Either(?, Bool)",
    "case ? | ? => ? | ? => Right@<?, Bool>(?) end",
  ),
];

let tests = (
  "Statics.Slicing.ExtendedExamples",
  parse_digit_examples
  @ seq_pin_examples
  @ demo_examples
  @ ctor_alias_examples
  @ if_joins
  @ case_joins,
);
