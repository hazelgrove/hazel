open Alcotest;
open Language;
open Test_Statics_Prelude;
module T = IdTagged.FreshGrammar.Typ;

let infos = (map: Statics.Map.t): list(Info.exp) =>
  Statics.Map.error_ids(map)
  |> List.filter_map(~f=id => Statics.Map.lookup_exp(id, map));

/* Exactly one node is marked, and its message is `message`. */
let marked_once = (name, program, message) =>
  test_case(
    name,
    `Quick,
    () => {
      let map = statics(parse_exp(program));
      switch (infos(map)) {
      | [info] =>
        check(
          string,
          "message",
          message,
          Haz3lcore.ErrorPrint.string_of_marks(InfoExp(info), info.marks),
        )
      | infos =>
        fail(
          Printf.sprintf(
            "expected one marked expression, got %d",
            List.length(infos),
          ),
        )
      };
    },
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
