open Alcotest;
open Language;

/* Quotation covers every Exp form, and keeps covering them as Hazel grows.

   A quotation holds ordinary syntax: `quote e end` is the parsed term of
   `e`, whatever form `e` is, and everything quotation does to it -- check
   it closed, keep it unevaluated, fill its antiquotes, decode it back into
   code -- goes through Exp.map_term, which has no default arm. So a new
   form needs no quotation code of its own. This test is what makes that
   claim checkable rather than hoped for, in two ways:

   - At compile time: `coverage` below is an exhaustive switch over
     Exp.cls, with no default arm, so adding a form to Exp.cls (which
     Exp.cls_of_term, also exhaustive, forces for every new constructor in
     Grammar) fails to build until someone writes a sample for it here, or
     says why it has none.
   - At run time: every tag in Exp.all_of_cls (derived by ppx_enumerate) is
     checked with its sample: `quote <sample> end` parses to a quotation
     whose body is that form; the program evaluates to the same quotation,
     unchanged; decoding gives back the body; and, where the sample marks
     a child position with `%%`, an antiquote there is filled exactly as
     the code written in place would be. */

type coverage =
  /* Closed concrete syntax containing the form -- at its root, or inside
     another where the form occurs only there, as a tuple's labeled item
     does -- with at most one `%%` marking a position where a child
     expression goes. A leaf has none. */
  | Sample(string)
  /* No concrete syntax produces the form on its own; the reason says why,
     and where it is covered instead, if anywhere. */
  | Exempt(string);

let int_ops = (op: Operators.op_bin_num) =>
  Sample("%% " ++ Operators.int_op_to_string(op) ++ " 2");

let coverage: Exp.cls => coverage =
  fun
  | Invalid => Exempt("an unparsable token: there is no term to quote")
  | EmptyHole => Sample("?")
  | MultiHole =>
    Exempt("malformed input, terms with no operator between them")
  | DynamicErrorHole => Exempt("made by evaluation, when a cast fails")
  /* Occurs only as an argument of a partial application. */
  | Deferral => Sample("(fun (a, b) -> a)(%%, _)")
  | Undefined => Sample("undefined")
  | Atom(Int) => Sample("1")
  | Atom(Float) => Sample("1.5")
  | Atom(Bool) => Sample("true")
  | Atom(String) => Sample("\"a\"")
  /* Nat and SInt have no literal syntax: elaboration makes them from an
     Int literal, under `use` or against a Nat or SInt expected type. A
     quotation holds syntax, so what it holds is the Int form. */
  | Atom(Nat)
  | Atom(SInt) => Exempt("made by elaboration from an Int literal")
  /* Its body is in a derivation sort, not Exp, so there is no Exp child
     for an antiquote: a leaf here, as Exp.map_term treats it. */
  | DrvQuote => Sample("of_alfa_exp 1 end")
  | ListLit => Sample("[%%, 2]")
  | Constructor => Sample("Some")
  | Fun => Sample("fun x -> %%")
  | TypFun => Sample("typfun A -> %%")
  /* The `_` of `_ = e`, which occurs only inside a tuple. */
  | ExplicitNonlabel => Sample("(_ = %%, b = 2)")
  | Label => Sample("`a`")
  /* A labeled item occurs only inside a tuple. */
  | TupLabel => Sample("(a = %%, b = 2)")
  | TupleExtension => Sample("(a = 1) ... %%")
  | Tuple => Sample("(%%, 2)")
  | Dot => Sample("(a = %%).a")
  | Var => Sample("string_of_int")
  | Let => Sample("let y = %% in y")
  | Bind => Sample("do x <- %% in x")
  | Theorem => Sample("theorem p = %% in 1")
  | ProofObject => Sample("proof_object %% end")
  | Forall => Sample("forall x -> %%")
  | FixF => Sample("fix f -> %%")
  | TyAlias => Sample("type T = Int in %%")
  | Use => Sample("use Int in %%")
  | Ap => Sample("string_of_int(%%)")
  | TypAp => Sample("(typfun A -> %%)@<Int>")
  | DeferredAp => Sample("(fun (a, b) -> a)(%%, _)")
  | If => Sample("if true then %% else 2")
  | Seq => Sample("%%; 2")
  | Test => Sample("test %% end")
  /* A leaf here: an antiquote inside a nested quotation is that
     quotation's own, filled when IT is evaluated, which as data it is
     not. */
  | Quote => Sample("quote 1 end")
  | Unquote =>
    Exempt(
      "an antiquote, the quotation mechanism itself; covered by the Quote group",
    )
  | HintedTest => Sample("hint %% test true end")
  | Filter => Sample("hide 1 in %%")
  | Closure => Exempt("made by evaluation")
  /* Fumola's embedded code is not quoted: a Macro expansion on this branch
     builds Hazel code only. */
  | FumolaQuote => Exempt("Fumola code: quotation builds Hazel code only")
  | FumolaPeek => Exempt("made by evaluation: a Fumola value on display")
  | Parens =>
    Exempt("never a class: Exp.cls_of_term looks through parentheses")
  /* A projector is transparent to semantics: the term built from the
     editor (MakeTerm.from_zip_for_sem) holds the projected term, so that
     is what a quotation of `^^fold(e)` holds. */
  | Projector =>
    Exempt("transparent to semantics: a quotation holds the projected term")
  | Cons => Sample("%% :: []")
  | UnOp(Int(Minus)) => Sample("-(%%)")
  /* Likewise made from `-`, the Int form, by elaboration. */
  | UnOp(Nat(Minus))
  | UnOp(SInt(Minus))
  | UnOp(Float(Minus)) => Exempt("made by elaboration from the Int `-`")
  | UnOp(Bool(Not)) => Sample("!(%%)")
  | BinOp(Int(op)) => int_ops(op)
  /* Written with the Int operators; elaboration picks the class. */
  | BinOp(SInt(_))
  | BinOp(Nat(_)) => Exempt("made by elaboration from the Int operator")
  | BinOp(Float(op)) =>
    Sample("%% " ++ Operators.float_op_to_string(op) ++ " 2.")
  | BinOp(Bool(op)) =>
    Sample("%% " ++ Operators.bool_op_to_string(op) ++ " false")
  | BinOp(String(op)) =>
    Sample("%% " ++ Operators.string_op_to_string(op) ++ " \"b\"")
  | BinOp(Poly(op)) =>
    Sample("%% " ++ Operators.poly_op_to_string(op) ++ " 2")
  | BuiltinFun => Exempt("made by elaboration: a builtin's implementation")
  | Match => Sample("case %% | _ => 1 end")
  | Asc => Sample("(%% : Int)")
  | LivelitName => Sample("^pct")
  | LivelitAp => Sample("^pct(%%)")
  | ListConcat => Sample("[%%] @ [2]")
  | Module => Sample("{ let a = %% }")
  | ModuleExp => Sample("module M = %% in 1");

let parse = (~source, text): Exp.t =>
  switch (Haz3lcore.PersistentZipper.parse_text(~source, ~root=Exp, text)) {
  | None => fail(source ++ " did not parse: " ++ text)
  | Some(z) =>
    let Haz3lcore.MakeTerm.{term, _} =
      Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    term;
  };

let rec strip = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(e) => strip(e)
  | _ => e
  };

/* The body of the quotation a program is, as parsed or as a value. */
let quoted_body = (e: Exp.t): option(Exp.t) =>
  switch (strip(e).term) {
  | Quote(body) => Some(body)
  | _ => None
  };

let evaluate = (e: Exp.t): Exp.t => {
  let (_, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), e);
  Evaluator.evaluate(~env=Builtins.env_init, elab) |> fst;
};

/* The class of every expression in a term, the root included. */
let classes = (e: Exp.t): list(Exp.cls) => {
  let found = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e) => {
          found := [Exp.cls_of_term(e.term), ...found^];
          continue(e);
        },
      e,
    );
  found^;
};

let fill = (sample, child) =>
  Str.global_replace(Str.regexp_string("%%"), child, sample);

let check_form = (cls: Exp.cls, sample: string) => {
  let name = Exp.show_cls(cls);
  let plain = fill(sample, "1");
  let program = parse(~source=name, "quote " ++ plain ++ " end");
  let body =
    switch (quoted_body(program)) {
    | Some(b) => b
    | None => fail(name ++ ": `quote " ++ plain ++ " end` is not a quotation")
    };
  /* The sample really contains the form it stands for. */
  check(
    bool,
    name ++ ": the sample `" ++ plain ++ "` contains that form",
    true,
    List.mem(cls, classes(body)),
  );
  /* Evaluation leaves a quotation alone. */
  let value = evaluate(program);
  let value_body =
    switch (quoted_body(value)) {
    | Some(b) => b
    | None => fail(name ++ ": did not evaluate to a quotation")
    };
  check(
    bool,
    name ++ ": evaluation keeps the body",
    true,
    Exp.fast_equal(body, value_body),
  );
  /* Decoding gives back the code. */
  check(
    bool,
    name ++ ": decoding gives back the body",
    true,
    switch (BuiltinsADT.code_of_exp_value(value)) {
    | Some(code) => Exp.fast_equal(body, code)
    | None => false
    },
  );
  /* An antiquote at a child position is filled as the code in place. */
  if (sample != plain) {
    let anti =
      parse(
        ~source=name ++ "-anti",
        "quote " ++ fill(sample, "unquote quote 1 end end") ++ " end",
      );
    switch (quoted_body(evaluate(anti))) {
    | Some(filled) =>
      check(
        bool,
        name ++ ": an antiquote is filled as the code in place",
        true,
        Exp.fast_equal(body, filled),
      )
    | None => fail(name ++ ": the antiquoted sample did not evaluate")
    };
  };
};

let tests = (
  "QuoteCoverage",
  Exp.all_of_cls
  |> List.map(cls =>
       test_case(Exp.show_cls(cls), `Quick, () =>
         switch (coverage(cls)) {
         | Sample(s) => check_form(cls, s)
         | Exempt(_) => ()
         }
       )
     ),
);
