open Alcotest;
open Haz3lcore;
let parse = Test_StackFocus.parse;
let check_plan = (before, after) => {
  let steps = DefinitionSteps.plan(before, after);
  let final =
    List.fold_left(
      (seg, step) => {
        let next = DefinitionSteps.apply(step, seg);
        /* Each intermediate is real syntax, not an overlay on the final term. */
        ignore(MakeTerm.go(DefinitionSteps.materialize(next)));
        next;
      },
      before,
      steps,
    );
  check(
    bool,
    "operations reconstruct accepted syntax and ids",
    true,
    Segment.equiv_mod_grout(final, after),
  );
  steps;
};
let tests = (
  "Definition steps",
  [
    test_case(
      "block replacement never duplicates declarations",
      `Quick,
      () => {
        let before =
          parse(
            "type A = Int in type B = [A] in let f : A -> B = fun a -> [a] in f(1)",
          );
        let after =
          parse(
            "type A = Int in type B = [A] in let f : A -> B = fun a -> [a, a] in f(2)",
          );
        let steps = check_plan(before, after);
        ignore(
          List.fold_left(
            (seg, op) => {
              let next = DefinitionSteps.apply(op, seg);
              let names =
                DefinitionSteps.items(next)
                |> List.filter_map((it: DefinitionSteps.item) => it.name);
              check(
                int,
                "each named definition appears once",
                List.length(List.sort_uniq(compare, names)),
                List.length(names),
              );
              check(
                list(string),
                "types stay present throughout",
                ["A", "B"],
                names
                |> List.filter_map(((kind, name)) =>
                     kind == "type" ? Some(name) : None
                   ),
              );
              next;
            },
            before,
            steps,
          ),
        );
      },
    ),
    test_case(
      "existing function body changes as one definition",
      `Quick,
      () => {
        let before =
          parse("let f : Int -> Int = fun x -> let old = x in old in f(1)");
        let (term, _) = Test_StackFocus.statics_of(before);
        let id = Test_StackFocus.outline_id(term, "f");
        let after =
          Web.ScratchFocus.splice_def(
            id,
            parse("fun x -> let a = x + 1 in let b = a * 2 in b"),
            before,
          );
        let steps = check_plan(before, after);
        check(int, "one function revision", 1, List.length(steps));
      },
    ),
    test_case(
      "multi-definition insertion",
      `Quick,
      () => {
        let after =
          parse(
            "type A = Int in type B = Int in let f : A -> B = fun x -> x in 0",
          );
        let steps = check_plan(parse("0"), after);
        check(
          bool,
          "definitions are separate operations",
          true,
          List.length(steps) >= 3,
        );
        let states =
          List.fold_left(
            ((seg, states), op) => {
              let next = DefinitionSteps.apply(op, seg);
              (next, states @ [next]);
            },
            (parse("0"), []),
            steps,
          )
          |> snd;
        check(
          bool,
          "a real state contains A before B",
          true,
          List.exists(
            seg => {
              let text = Test_StackFocus.text_of(seg);
              String.contains(text, 'A') && !String.contains(text, 'B');
            },
            states,
          ),
        );
      },
    ),
    test_case(
      "reparsed module retains existing members during replacement",
      `Quick,
      () => {
        let before =
          parse(
            "let m = {type A = Int; type B = Int; let f : A -> B = fun x -> x} in 0",
          );
        let after =
          parse(
            "let m = {type A = Int; type B = Int; let f : A -> B = fun x -> x + 1} in 0",
          );
        let steps = check_plan(before, after);
        ignore(
          List.fold_left(
            (seg, op) => {
              let next = DefinitionSteps.apply(op, seg);
              let text = Test_StackFocus.text_of(next);
              check(
                bool,
                "A remains in the module",
                true,
                String.contains(text, 'A'),
              );
              check(
                bool,
                "B remains in the module",
                true,
                String.contains(text, 'B'),
              );
              next;
            },
            before,
            steps,
          ),
        );
      },
    ),
    test_case(
      "nested module grows by member",
      `Quick,
      () => {
        let after =
          parse(
            "let m = {type A = Int; type B = Int; let f : A -> B = fun x -> x} in 0",
          );
        let steps = check_plan(parse("0"), after);
        check(
          bool,
          "module shell and members have distinct operations",
          true,
          List.length(
            List.filter(
              (op: DefinitionSteps.operation) => op.path != [],
              steps,
            ),
          )
          >= 3,
        );
      },
    ),
    test_case(
      "reorder preserves ids and subsequent deletion",
      `Quick,
      () => {
        let original = parse("type A = Int in type B = Int in 0");
        let spans = DefinitionSpans.item_spans(original);
        let parts =
          List.map(
            (s: DefinitionSpans.item_span) =>
              DefinitionSpans.slice(s.sp_start, s.sp_stop, original),
            spans,
          );
        let reordered =
          List.nth(parts, 1) @ List.nth(parts, 0) @ List.nth(parts, 2);
        ignore(check_plan(original, reordered));
        ignore(
          check_plan(reordered, List.nth(parts, 0) @ List.nth(parts, 2)),
        );
      },
    ),
    test_case("padding and nested function blocks", `Quick, () => {
      ignore(
        check_plan(
          parse(" 0 "),
          parse(
            "\n type A = Int in\n let f = fun x -> let y = x in let z = y in z in\n 0 ",
          ),
        ),
      )
    }),
    test_case(
      "emptying a module retains its identity",
      `Quick,
      () => {
        let before = parse("let m = {type A = Int; type B = Int} in 0");
        let (term, info_map) = Test_StackFocus.statics_of(before);
        ignore(info_map);
        let id = Test_StackFocus.outline_id(term, "m");
        let after = Web.ScratchFocus.splice_def(id, parse("{}"), before);
        ignore(check_plan(before, after));
      },
    ),
    test_case(
      "unchanged program produces no edits",
      `Quick,
      () => {
        let seg = parse("type A = Int in 0");
        check(int, "no-op", 0, List.length(check_plan(seg, seg)));
      },
    ),
  ],
);
