open Alcotest;
open Language;
open Test_Statics_Prelude;
module T = IdTagged.FreshGrammar.Typ;

let infos = (map: Statics.Map.t): list(Info.exp) =>
  Statics.Map.error_ids(map)
  |> List.filter_map(~f=id => Statics.Map.lookup_exp(id, map));

let messages = (map: Statics.Map.t): list(string) =>
  infos(map)
  |> List.map(~f=(info: Info.exp) =>
       Haz3lcore.ErrorPrint.string_of_marks(InfoExp(info), info.marks)
     )
  |> List.sort(~compare=String.compare);

/* The marked nodes' messages are `expected`, in any order. */
let marked_with = (name, program, expected) =>
  test_case(name, `Quick, () =>
    check(
      list(string),
      "messages",
      List.sort(~compare=String.compare, expected),
      messages(statics(parse_exp(program))),
    )
  );

/* Exactly one node is marked, and its message is `message`. */
let marked_once = (name, program, message) =>
  marked_with(name, program, [message]);

/* No node is marked. */
let unmarked = (name, program) =>
  test_case(name, `Quick, () =>
    check(
      int,
      "marked expressions",
      0,
      List.length(infos(statics(parse_exp(program)))),
    )
  );

let info_where = (map: Statics.Map.t, matches: Exp.term => bool): Info.exp =>
  switch (
    Id.Map.fold(
      (_, info: Info.t, found) =>
        switch (info) {
        | InfoExp(info) when matches(info.user_term.term) => Some(info)
        | _ => found
        },
      map,
      None,
    )
  ) {
  | Some(info) => info
  | None => fail("no expression of the expected form")
  };

/* The syn type keeps the error the elab syn type has settled on ana. */
let types_of = (name, program, matches, ~syn, ~elab_syn) =>
  test_case(
    name,
    `Quick,
    () => {
      let map = statics(parse_exp(program));
      let info = info_where(map, matches);
      check(testable_typ, "syn_ty", syn, info.syn_ty);
      check(testable_typ, "elab_syn_ty", elab_syn, info.elab_syn_ty);
    },
  );

/* Each sub-expression's elaboration synthesizes the type recorded for it,
   with no expectation. */
let self_typed = (name, program) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(program);
      check(
        list(string),
        "sub-expressions whose elaboration synthesizes another type",
        [],
        Test_Statics_Properties.differing_subs(statics(exp), exp)
        |> List.map(~f=Test_Statics_Properties.show_syn_disagreement),
      );
    },
  );

let elab_ids = (map: Statics.Map.t): list(Id.t) =>
  Id.Map.fold(
    (_, info: Info.t, acc) =>
      switch (info) {
      | InfoExp({elab_term, _}) =>
        let ids = ref([]);
        let _ =
          Exp.map_term(
            ~f_exp=
              (cont, e) => {
                ids := IdTagged.ids(e) @ ids^;
                cont(e);
              },
            elab_term,
          );
        ids^ @ acc;
      | _ => acc
      },
    map,
    [],
  )
  |> List.dedup_and_sort(~compare=Id.compare);

/* The incremental evaluator caches by id, so syntax elaboration inserts
   must have the same ids on every run. */
let ids_stable = (name, program) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(program);
      check(
        list(string),
        "elaboration ids",
        elab_ids(statics(exp)) |> List.map(~f=Id.to_string),
        elab_ids(statics(exp)) |> List.map(~f=Id.to_string),
      );
    },
  );

/* `var`'s elaboration in `program` is ascribed with what it was expected to
   be. */
let ascribes_var = (name, program, var) =>
  test_case(
    name,
    `Quick,
    () => {
      let (_, elab) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp(program),
        );
      let ascribed = ref(false);
      let _ =
        Exp.map_term(
          ~f_exp=
            (cont, e) => {
              switch (e.term) {
              | Asc({term: Var(v), _}, _) when String.equal(v, var) =>
                ascribed := true
              | _ => ()
              };
              cont(e);
            },
          elab,
        );
      check(bool, "ascribed", true, ascribed^);
    },
  );

let programs = [
  "let f : Int -> Bool = fun x -> x + 1 in f(1)",
  {|let r : Int = if true then 1 else "s" in r|},
  "(a=1, b=1) : Int",
  "((a=1, b=1)) : Int",
  "[1, 2] : Int",
  {|[1, "s"] : [Int]|},
  {|[1, "s", 3]|},
  "let p : (a=Int, b=Int) = (a=1, b=2, c=3) in p",
  {|let p : (a=Int, b=Int) = (a=1, b="s", c=3) in p|},
  "let p : (a=Int, b=Int) = (a=1, c=3) in p",
  "let x : String = 4 in x",
  {|if false then 1 else ("" : ?)|},
  "module M = { let x = 1 } in M + 1",
];

let tests = (
  "Statics.Settling",
  [
    marked_once(
      "A function body that fails the return type is marked, not the function",
      "let f : Int -> Bool = fun x -> x + 1 in f(1)",
      "Expecting type Bool but got inconsistent type Int",
    ),
    marked_once(
      "A branch that fails the expected type is marked, not the if",
      {|let r : Int = if true then 1 else "s" in r|},
      "Expecting type Int but got inconsistent type String",
    ),
    marked_once(
      "A tuple that is not of the expected type is marked with its type",
      "(a=1, b=1) : Int",
      "Expecting type Int but got inconsistent type a=Int, b=Int",
    ),
    marked_once(
      "Parentheses do not repeat the mark of what they enclose",
      "((a=1, b=1)) : Int",
      "Expecting type Int but got inconsistent type a=Int, b=Int",
    ),
    marked_once(
      "A list that is not of the expected type is marked with its type",
      "[1, 2] : Int",
      "Expecting type Int but got inconsistent type [Int]",
    ),
    marked_once(
      "A list in synthesis whose elements do not meet is marked",
      {|[1, "s", 3]|},
      "Expecting elements to have consistent types but got types: Int, String, Int",
    ),
    marked_once(
      "Lists appended in synthesis whose elements do not meet are marked",
      {|[1] @ ["s"]|},
      "Expecting operands to have consistent types but got types: [Int], [String]",
    ),
    marked_once(
      "Against a list type, only the element that does not fit is marked",
      {|let xs : [Int] = [1, "s"] in xs|},
      "Expecting type Int but got inconsistent type String",
    ),
    unmarked(
      "Against an unknown type, elements need not meet",
      {|let f : ? -> ? = fun x -> x in f([1, "s"])|},
    ),
    marked_once(
      "An unexpected tuple label is marked on its entry only",
      "let p : (a=Int, b=Int) = (a=1, b=2, c=3) in p",
      "Unexpected label c; expecting labels a, b",
    ),
    marked_with(
      "The other entries are still checked against their labels' types",
      {|let p : (a=Int, b=Int) = (a=1, b="s", c=3) in p|},
      [
        "Expecting type Int but got inconsistent type String",
        "Unexpected label c; expecting labels a, b",
      ],
    ),
    marked_with(
      "A tuple also missing a label is marked for its shape",
      "let p : (a=Int, b=Int) = (a=1, c=3) in p",
      [
        "Expecting a tuple of 2 elements with labels a, b but got 2 elements with labels a, c; missing label b; unexpected label c",
        "Unexpected label c; expecting labels a, b",
      ],
    ),
    test_case(
      "A mismatch is kept beside a node's other marks",
      `Quick,
      () => {
        let map = statics(parse_exp("[fun 1 -> 2] : [Int]"));
        switch (infos(map)) {
        | [{user_term: {term: Fun(_), _}, marks, _}] =>
          check(
            list(string),
            "marks",
            ["InexhaustiveMatch", "ExpectationMismatch"],
            List.map(marks, ~f=m =>
              switch (m) {
              | InexhaustiveMatch(_) => "InexhaustiveMatch"
              | ExpectationMismatch(_) => "ExpectationMismatch"
              | _ => "other"
              }
            ),
          )
        | infos =>
          fail(
            Printf.sprintf(
              "expected only the function marked, got %d",
              List.length(infos),
            ),
          )
        };
      },
    ),
    marked_once(
      "A free constructor is marked only for being free",
      "let xs : [Int] = A in xs",
      "Constructor is not defined",
    ),
    test_case(
      "The type projector reads what is written, not the settled type",
      `Quick,
      () => {
        let map = statics(parse_exp("let x : String = 4 in x"));
        let four =
          info_where(
            map,
            fun
            | Atom(Int(_)) => true
            | _ => false,
          );
        check(
          option(testable_typ),
          "self type",
          Some(T.int()),
          Haz3lcore.TypeProj.self_ty(InfoExp(four)),
        );
      },
    ),
    types_of(
      "A constructor's syn type is its own type, not the one it settled on",
      "type T = A + B in let x : Int = A in x",
      fun
      | Constructor(_) => true
      | _ => false,
      ~syn=T.var("T"),
      ~elab_syn=T.int(),
    ),
    types_of(
      "A function's syn type keeps its body's error",
      "let f : Int -> Bool = fun x -> x + 1 in f",
      fun
      | Fun(_) => true
      | _ => false,
      ~syn=T.(arrow(int(), int())),
      ~elab_syn=T.(arrow(int(), bool())),
    ),
    types_of(
      "An if whose branches disagree has the type its branches were settled to",
      {|let r : Int = if true then 1 else "s" in r|},
      fun
      | If(_) => true
      | _ => false,
      ~syn=T.int(),
      ~elab_syn=T.int(),
    ),
    types_of(
      "An if whose branches agree on the wrong type keeps that type",
      {|let r : Int = if true then "a" else "b" in r|},
      fun
      | If(_) => true
      | _ => false,
      ~syn=T.string(),
      ~elab_syn=T.int(),
    ),
    types_of(
      "A list whose elements disagree has the type its elements were settled to",
      {|let xs : [Int] = [1, "s"] in xs|},
      fun
      | ListLit(_) => true
      | _ => false,
      ~syn=T.(list(int())),
      ~elab_syn=T.(list(int())),
    ),
    types_of(
      "Parentheses pass up the syn type of what they enclose",
      "((a=1, b=1)) : Int",
      fun
      | Parens({term: Parens(_), _}) => true
      | _ => false,
      ~syn=
        T.(
          prod([
            tup_label(label("a"), int()),
            tup_label(label("b"), int()),
          ])
        ),
      ~elab_syn=T.int(),
    ),
    ascribes_var(
      "A module used where a value is expected keeps its ascription",
      "module M = { let x = 1 } in M + 1",
      "M",
    ),
    ...List.map(
         ~f=p => self_typed("Elaboration synthesizes: " ++ p, p),
         programs,
       ),
  ]
  @ List.map(
      ~f=p => ids_stable("Elaboration ids persist: " ++ p, p),
      programs,
    ),
);
