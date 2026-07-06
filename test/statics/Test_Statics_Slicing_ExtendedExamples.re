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
    "type Option = typfun A -> ? + Some(A) in type ? = ? in let parse_digit = fun c : String -> case ? | ? => Some(?) | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? | ? => ? end in parse_digit(?)",
  ),
  analysis_case(
    ~focus=str_lit("5"),
    "ex1-parse-digit-ana",
    ex1_src,
    "String",
    "type ? = ? in type ? = ? in let parse_digit = fun c : String -> ? in parse_digit(?)",
  ),
];

let ex2_src = "type Option = typfun A -> None + Some(A) in type Digit = Zero + One in type Pin = (Digit, Digit) in let seq = abs A -> abs B -> fun (p : String -> Option((String, A))) -> fun (f : A -> Option((String, B))) -> fun (s : String) -> case p(s) | None => None | Some((s2, a)) => f(a) end in let digit_parser = fun (s : String) -> Some((s, Zero)) in let parse_pin = fun (s : String) -> seq@<Digit, Pin>(digit_parser)(fun (d1 : Digit) -> seq@<Digit, Pin>(digit_parser)(fun (d2 : Digit) -> fun (s2 : String) -> Some((s2, (d1, d2))))(s))(s) in parse_pin(\"12\")";

let seq_pin_examples = [
  synthesis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-syn",
    ex2_src,
    "Digit -> String -> Option((String, Pin))",
    "type Option = typfun A -> ? + Some(A) in type Digit = ? + ? in type Pin = (Digit, Digit) in let ? = ? in let ? = ? in let parse_pin = fun ? -> seq@<Digit, Pin>(?)(fun ? -> ?(fun (d2 : Digit) -> fun (s2 : String) -> Some(?))(?))(?) in parse_pin(?)",
  ),
  synthesis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-syn-refined",
    ex2_src,
    "? -> ? -> ?",
    "type ? = ? in type ? = ? in type ? = ? in let ? = ? in let ? = ? in let parse_pin = fun ? -> seq@<?, ?>(?)(fun ? -> ?(fun ? -> fun ? -> ?)(?))(?) in parse_pin(?)",
  ),
  analysis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-ana",
    ex2_src,
    "Digit -> Option((String, Pin))",
    "type Option = typfun ? -> None + ? in type Digit = ? + ? in type Pin = (Digit, Digit) in let seq = abs A -> abs B -> fun (p : String -> Option((String, A))) -> fun (f : A -> Option((String, B))) -> fun (s : String) -> case ? | ? => None | ? => ? end in let ? = ? in let ? = fun ? -> ?(fun ? -> seq@<Digit, Pin>(?)(?)(?))(?) in ?",
  ),
  analysis_case(
    ~focus=fun_binding("d2"),
    "ex2-seq-error-ana-refined",
    ex2_src,
    "? -> Option(?)",
    "type Option = typfun ? -> None + ? in type ? = ? in type ? = ? in let seq = abs A -> abs B -> fun (p : String -> Option((String, ?))) -> fun (f : ? -> Option((String, ?))) -> fun (s : String) -> case ? | ? => None | ? => ? end in let ? = ? in let ? = fun ? -> ?(fun ? -> seq@<?, ?>(?)(?)(?))(?) in ?",
  ),
];

let demo_src = "type Option = typfun A -> None + Some(A) in type Digit = Zero + One + Two + Three + Four + Five + Six + Seven + Eight + Nine in type Pin = (Digit, Digit, Digit, Digit) in let parse_digit = fun c : String -> case c | \"0\" => Some(Zero) | \"1\" => Some(One) | \"2\" => Some(Two) | \"3\" => Some(Three) | \"4\" => Some(Four) | \"5\" => Some(Five) | \"6\" => Some(Six) | \"7\" => Some(Seven) | \"8\" => Some(Eight) | \"9\" => Some(Nine) | _ => None end in let seq = abs A -> abs B -> fun (p : String -> Option((String, A))) -> fun (f : A -> Option((String, B))) -> fun (s : String) -> case p(s) | None => None | Some((s2, a)) => f(a) end in let digit_parser = fun (s : String) -> case parse_digit(s) | Some(d) => Some((s, d)) | None => None end in let parse_pin = fun (s : String) -> seq@<Digit, Pin>(digit_parser)(fun (d1 : Digit) -> seq@<Digit, Pin>(digit_parser)(fun (d2 : Digit) -> seq@<Digit, Pin>(digit_parser)(fun (d3 : Digit) -> seq@<Digit, Pin>(digit_parser)(fun (d4 : Digit) -> fun (s2 : String) -> Some((s2, (d1, d2, d3, d4))))(s))(s))(s))(s) in parse_pin(\"1234\")";

let demo_examples = [synthesis_case("demo-scratchpad", demo_src, "?", "?")];

let ctor_alias_examples = [
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
    "if c then (1, ?) else (?, 2)",
  ),
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "if-join-product-left",
    "if c then (1, ?) else (?, 2)",
    "(Int, ?)",
    "if c then (1, ?) else ?",
  ),
  synthesis_case(
    ~ctx=ctx_var("c", "Bool"),
    "if-join-product-right",
    "if c then (1, ?) else (?, 2)",
    "(?, Int)",
    "if c then ? else (?, 2)",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    ~aliases=[("Either", "typfun A -> typfun B -> Left(A) + Right(B)")],
    "if-join-either-full",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "Either(Int, Bool)",
    "if c then Left@<Int, ?>(?) else Right@<?, Bool>(?)",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    ~aliases=[("Either", "typfun A -> typfun B -> Left(A) + ?")],
    "if-join-either-left",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "Either(Int, ?)",
    "if c then Left@<Int, ?>(?) else ?",
  ),
  synthesis_case(
    ~ctx=ctx_var(~ctx=prelude_ctx(either_prelude), "c", "Bool"),
    ~aliases=[("Either", "typfun A -> typfun B -> ? + Right(B)")],
    "if-join-either-right",
    "if c then Left@<Int, ?>(1) else Right@<?, Bool>(true)",
    "Either(?, Bool)",
    "if c then ? else Right@<?, Bool>(?)",
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
