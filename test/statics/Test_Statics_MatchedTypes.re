open Alcotest;
open Language;
open Test_Statics_Prelude;
module T = FTemp.Typ;

let ctx = Builtins.ctx_init(Some(Int));
let diagnostic = ({ana, syn}: MatchedTyp.failure) => (ana, syn);
let testable_diagnostic = pair(testable_typ, testable_typ);

let check_synthesis = (map, exp) =>
  check(
    list(string),
    "recorded synthesis agrees with fresh synthesis of every elaborated subexpression",
    [],
    Test_Statics_Properties.differing_subs(map, exp)
    |> List.map(Test_Statics_Properties.show_syn_disagreement),
  );

/* Punctuation ids can share an Info. Count diagnostics at their owning node,
   exactly as the editor's error list does. */
let local_marks = map =>
  Statics.Map.error_ids(map)
  |> List.concat_map(id =>
       Statics.Map.lookup(id, map) |> Option.get |> Info.marks_of
     );

let marked_once = (name, program) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(program);
      let map = statics(exp);
      check(int, "one local mark", 1, List.length(local_marks(map)));
      check_synthesis(map, exp);
    },
  );

let find_info = (predicate, map) =>
  Id.Map.fold(
    (_, info, found) =>
      switch (info) {
      | Info.InfoExp(info) when predicate(info.user_term.term) => Some(info)
      | _ => found
      },
    map,
    None,
  )
  |> Option.get;

let tuple_message = (name, program, expected) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(program);
      let map = statics(exp);
      let info =
        find_info(
          fun
          | Tuple(_) => true
          | _ => false,
          map,
        );
      check(
        string,
        "tuple diagnostic",
        expected,
        Haz3lcore.ErrorPrint.string_of_marks(InfoExp(info), info.marks),
      );
      check_synthesis(map, exp);
    },
  );

let qcheck_tuple_synthesis =
  QCheck.Test.make(
    ~name="Tuple diagnostics preserve fresh elaboration synthesis",
    ~count=500,
    QCheck.make(
      QCheck.Gen.(
        list_size(
          map((+)(2), int_bound(5)),
          oneofl(["1", "true", "\"s\""]),
        )
      ),
    ),
    values => {
      let program =
        "("
        ++ String.concat(",", values)
        ++ ") : ("
        ++ String.concat(",", List.map(_ => "Int", values))
        ++ ")";
      let exp = parse_exp(program);
      let map = statics(exp);
      check(
        int,
        "each wrong element owns exactly one mark",
        List.length(List.filter((!=)("1"), values)),
        List.length(local_marks(map)),
      );
      check_synthesis(map, exp);
      true;
    },
  );

let tests = (
  "Statics.MatchedTypes",
  [
    test_case("Optional shape failure returns None", `Quick, () =>
      check(
        bool,
        "no arrow",
        true,
        MatchedTyp.match_type(ctx, Arrow, Optional, T.int()) == None,
      )
    ),
    test_case(
      "Caller selects the shape diagnostic and receives recovery",
      `Quick,
      () => {
        let ((input, output), marks) =
          MatchedTyp.match_type(ctx, Arrow, Report(diagnostic), T.int());
        check(
          list(testable_diagnostic),
          "caller-selected diagnostic",
          [(T.int(), T.arrow(T.unknown(Internal), T.unknown(Internal)))],
          marks,
        );
        check(testable_typ, "input recovers", T.unknown(Internal), input);
        check(testable_typ, "output recovers", T.unknown(Internal), output);
      },
    ),
    test_case(
      "Success does not invoke the diagnostic constructor",
      `Quick,
      () => {
        let (element, marks) =
          MatchedTyp.match_type(
            ctx,
            List,
            Report(_ => failwith("successful match reported an error")),
            T.list(T.int()),
          );
        check(testable_typ, "element", T.int(), element);
        check(int, "no diagnostics", 0, List.length(marks));
      },
    ),
    test_case(
      "Unknown recovery is distinct from optional success",
      `Quick,
      () => {
        let unknown = T.unknown(Internal);
        check(
          bool,
          "ordinary unknown is not an observed list",
          true,
          MatchedTyp.match_type(ctx, List, Optional, unknown) == None,
        );
        let (element, marks) =
          MatchedTyp.match_type(ctx, List, Report(diagnostic), unknown);
        check(testable_typ, "unknown element", unknown, element);
        check(
          list(testable_diagnostic),
          "unknown is not an error",
          [],
          marks,
        );
        let synswitch = T.unknown(SynSwitch);
        let (element, marks) =
          MatchedTyp.match_type(ctx, List, Report(diagnostic), synswitch);
        check(testable_typ, "synthesis mode propagates", synswitch, element);
        check(
          list(testable_diagnostic),
          "synthesis mode is not an error",
          [],
          marks,
        );
      },
    ),
    test_case(
      "Matching normalizes aliases and parentheses",
      `Quick,
      () => {
        let ctx =
          Ctx.extend_alias(ctx, "Numbers", Id.invalid, T.list(T.int()));
        let element =
          MatchedTyp.match_type(
            ctx,
            List,
            Optional,
            T.parens(T.var("Numbers")),
          )
          |> Option.get;
        check(testable_typ, "aliased list element", T.int(), element);
      },
    ),
    test_case(
      "Product alignment records reordering and introduced labels",
      `Quick,
      () => {
        let typ =
          T.prod([
            T.tup_label(T.label("a"), T.int()),
            T.tup_label(T.label("b"), T.string()),
          ]);
        let product =
          MatchedTyp.match_type(
            ctx,
            Product([Some("b"), None]),
            Optional,
            typ,
          )
          |> Option.get;
        check(
          list(pair(int, option(string))),
          "alignment",
          [(1, Some("a")), (0, None)],
          product.alignment,
        );
        check(
          list(testable_typ),
          "component expectations",
          [
            T.tup_label(T.label("a"), T.int()),
            T.tup_label(T.label("b"), T.string()),
          ],
          product.types,
        );
      },
    ),
    test_case(
      "Arity recovery has the expression's length",
      `Quick,
      () => {
        let (product, marks) =
          MatchedTyp.match_type(
            ctx,
            Product([None, None, None]),
            Report(diagnostic),
            T.prod([T.int(), T.int()]),
          );
        check(
          int,
          "three child expectations",
          3,
          List.length(product.types),
        );
        check(
          list(testable_diagnostic),
          "arity diagnostic",
          [
            (
              T.prod([T.int(), T.int()]),
              T.prod(List.init(3, _ => T.unknown(Internal))),
            ),
          ],
          marks,
        );
      },
    ),
    tuple_message(
      "Tuple shape diagnostics name a missing label",
      {|(name="s") : (name=String, age=Int)|},
      "Expecting a tuple of 2 elements with labels name, age but got 1 element with labels name; missing label age",
    ),
    tuple_message(
      "Same-arity tuple diagnostics name missing and unexpected labels",
      {|(a=1, c=2) : (a=Int, b=Int)|},
      "Expecting a tuple of 2 elements with labels a, b but got 2 elements with labels a, c; missing label b; unexpected label c",
    ),
    tuple_message(
      "Tuple label diagnostics normalize aliases and parentheses",
      {|type Person = (name=String, age=Int) in (name="s") : ((Person))|},
      "Expecting a tuple of 2 elements with labels name, age but got 1 element with labels name; missing label age",
    ),
    tuple_message(
      "Tuple label diagnostics pluralize multiple missing labels",
      {|(name="s") : (name=String, age=Int, active=Bool)|},
      "Expecting a tuple of 3 elements with labels name, age, active but got 1 element with labels name; missing labels age, active",
    ),
    tuple_message(
      "Unlabeled arity diagnostics retain both counts",
      {|(1, 2, 3) : (Int, Int)|},
      "Expecting a tuple of 2 elements but got 3 elements",
    ),
    test_case(
      "Same-shape tuple types keep their component mismatch message",
      `Quick,
      () => {
        let ana = T.prod([T.int(), T.int()]);
        let syn = T.prod([T.int(), T.string()]);
        check(
          string,
          "ordinary expectation mismatch",
          "Expecting type Int, Int but got inconsistent type Int, String",
          Haz3lcore.ErrorPrint.core_mark_string(
            ctx,
            ana,
            ExpectationMismatch({
              ana,
              syn,
            }),
          ),
        );
      },
    ),
    test_case(
      "A nonliteral tuple's shape mismatch names differing labels",
      `Quick,
      () => {
        let ana = T.prod([T.tup_label(T.label("a"), T.int())]);
        let syn = T.prod([T.tup_label(T.label("b"), T.int())]);
        check(
          string,
          "tuple type shape",
          "Expecting a tuple of 1 element with labels a but got 1 element with labels b; missing label a; unexpected label b",
          Haz3lcore.ErrorPrint.core_mark_string(
            ctx,
            ana,
            ExpectationMismatch({
              ana,
              syn,
            }),
          ),
        );
      },
    ),
    marked_once(
      "A tuple component owns its mismatch",
      {|(1, "s") : (Int, Int)|},
    ),
    marked_once(
      "Parentheses delegate the component check",
      {|((1, (("s")))) : (Int, Int)|},
    ),
    marked_once(
      "A labeled value owns its mismatch",
      {|(a=1, b="s") : (a=Int, b=Int)|},
    ),
    marked_once(
      "An explicitly unlabeled value owns its mismatch",
      {|(_=1) : (_=String)|},
    ),
    marked_once(
      "A call's tuple argument delegates component checks",
      {|let f : (Int -> Int, Int) -> Int = fun (g, x) -> g(x) in f(true, 3)|},
    ),
    marked_once(
      "A function body owns its mismatch",
      {|let f : Int -> Bool = fun x -> x + 1 in f(1)|},
    ),
    marked_once(
      "A function argument delegates its result check",
      {|let g = fun (f : Int -> Bool) -> f(1) in g(fun x -> x + 1)|},
    ),
    marked_once("A list element owns its mismatch", {|[1, "s"] : [Int]|}),
    marked_once(
      "An explicit nonlabel delegates a nested tuple expectation",
      {|(_=(1, "s")) : (_=(Int, Int))|},
    ),
    marked_once(
      "An explicit parameter type is checked against the expected domain",
      {|(fun x : String -> 1) : (Int -> Int)|},
    ),
    marked_once(
      "A polymorphic function delegates the renamed body expectation",
      {|(typfun A -> 1) : (poly B -> Bool)|},
    ),
    marked_once(
      "A conditional branch owns its mismatch",
      {|(if true then 1 else "s") : Int|},
    ),
    marked_once(
      "A case branch owns its mismatch",
      {|(case true | true => 1 | false => "s" end) : Int|},
    ),
    marked_once(
      "A tuple shape mismatch stays on the tuple",
      {|(1, 2, 3) : (Int, Int)|},
    ),
    marked_once(
      "A function shape mismatch stays on the function",
      {|(fun x -> x) : Int|},
    ),
    marked_once(
      "A variable's independently synthesized type is checked",
      {|let p = (1, "s") in p : (Int, Int)|},
    ),
    marked_once(
      "A literal explicitly checks its expectation",
      {|true : Int|},
    ),
    marked_once(
      "An operator explicitly checks its result expectation",
      {|(1 + 2) : Bool|},
    ),
    marked_once(
      "An application explicitly checks its result expectation",
      {|let f = fun (x : Int) -> x in f(1) : Bool|},
    ),
    marked_once(
      "A projection explicitly checks its result expectation",
      {|let p = (a=1, b=2) in p.a : Bool|},
    ),
    marked_once(
      "Custom statics explicitly checks its result expectation",
      {|omit_all_labels((a=1, b=2)) : Bool|},
    ),
    marked_once(
      "An ascription's independently synthesized type is checked",
      {|(1 : Int) : Bool|},
    ),
    test_case(
      "A tuple retains its actual elaborated synthesized type",
      `Quick,
      () => {
        let exp = parse_exp({|(1, "s") : (Int, Int)|});
        let map = statics(exp);
        let info =
          find_info(
            fun
            | Tuple(_) => true
            | _ => false,
            map,
          );
        check(
          testable_typ,
          "raw synthesis",
          T.prod([T.int(), T.string()]),
          info.elab_syn_ty,
        );
        check(
          testable_typ,
          "type after fixing",
          T.prod([T.int(), T.int()]),
          Typ.normalize(info.ctx, info.ty) |> Typ.all_ids_temp,
        );
        check(int, "tuple has no local mark", 0, List.length(info.marks));
        check(
          bool,
          "Info retains the disagreement independently of its message",
          false,
          Info.meets_expectation(InfoExp(info)),
        );
        check(
          option(testable_typ),
          "expected type is queried from Info",
          Some(T.prod([T.int(), T.int()])),
          Info.expected_type(InfoExp(info)) |> Option.map(Typ.all_ids_temp),
        );
        check(
          bool,
          "the query reflects a changed type without changing the message",
          true,
          Info.meets_expectation(
            InfoExp({
              ...info,
              elab_syn_ty: T.prod([T.int(), T.int()]),
            }),
          ),
        );
      },
    ),
    test_case(
      "A function retains its actual elaborated return type",
      `Quick,
      () => {
        let exp = parse_exp({|let f : Int -> Bool = fun x -> x + 1 in f|});
        let map = statics(exp);
        let info =
          find_info(
            fun
            | Fun(_) => true
            | _ => false,
            map,
          );
        check(
          testable_typ,
          "raw synthesis",
          T.arrow(T.int(), T.int()),
          info.elab_syn_ty,
        );
        check(int, "function has no local mark", 0, List.length(info.marks));
      },
    ),
    QCheck_alcotest.to_alcotest(qcheck_tuple_synthesis),
  ],
);
