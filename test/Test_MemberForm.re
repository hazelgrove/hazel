open Alcotest;
open Haz3lcore;
open Language;

module P = Test_DefStaticsParity;

/* an expression-rooted program as module members (MemberForm, world-tree
   C2): same scoping, same ids */

let convert = (src: string): option(Segment.t) =>
  MemberForm.of_exp_root(CorpusUtil.seg_of(~root=Exp, src));

let text = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", seg) |> String.trim;

let shape = (name, src, expected) =>
  test_case(name, `Quick, () =>
    check(option(string), name, expected, Option.map(text, convert(src)))
  );

/* the type errors and the tests' results a program has at [root] */
let checked = (~root: Sort.t, seg: Segment.t) => {
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let (map, elab) =
    Statics.mk_unmemoized(P.settings, DefStatics.ctx0(), term);
  let errors = List.length(Statics.Map.error_ids(map));
  let tests =
    switch (P.evaluate(map, elab)) {
    | (_, state) => P.tests_of(state)
    | exception _ => [("evaluation", "raised")]
    };
  (errors, tests);
};

let corpus = () => {
  let converted = ref(0);
  let kept = ref([]);
  List.iter(
    ((path, _, src)) => {
      let seg = CorpusUtil.seg_of(~root=Exp, src);
      switch (MemberForm.of_exp_root(seg)) {
      | None => kept := [path, ...kept^]
      | Some(members) =>
        incr(converted);
        let (e0, t0) = checked(~root=Exp, seg);
        let (e1, t1) = checked(~root=Mod, members);
        check(int, path ++ ": type errors", e0, e1);
        check(list(pair(string, string)), path ++ ": tests", t0, t1);
      };
    },
    /* the expression programs: the world's slides are members already */
    List.filter(
      ((_, root, _)) => root == Sort.Exp,
      CorpusUtil.slide_corpus(),
    ),
  );
  print_endline(
    Printf.sprintf(
      "converted %d slides; kept %d: %s",
      converted^,
      List.length(kept^),
      String.concat(", ", List.rev(kept^)),
    ),
  );
};

let tests = (
  "MemberForm",
  [
    shape(
      "lets, a test, the last expression",
      "let x = 1 in\ntest x == 1 end;\nlet y = x + 1 in\ny",
      Some("let x = 1;\ntest x == 1 end;\nlet y = x + 1;\ny"),
    ),
    shape(
      "a type and a module",
      "type T = Int in\nmodule M = {\n  let a = 1\n} in\nlet b : T = M.a in\nb",
      Some(
        "type T = Int;\nmodule M = {\n  let a = 1\n};\nlet b : T = M.a;\nb",
      ),
    ),
    shape(
      "a let inside a definition stays",
      "let f = fun x -> let y = x in y in\nf(1)",
      Some("let f = fun x -> let y = x in y;\nf(1)"),
    ),
    shape("an unfinished definition", "let x = 1\nx", None),
    /* a hole member is passed over, as `?; e` is: the test after it runs */
    test_case("members after a hole still run", `Quick, () =>
      check(
        list(string),
        "test results",
        ["Pass"],
        List.map(
          snd,
          snd(
            checked(
              ~root=Mod,
              CorpusUtil.seg_of(
                ~root=Mod,
                "let a = 1;\n?;\ntest a == 1 end;\na",
              ),
            ),
          ),
        ),
      )
    ),
    test_case("the expression corpus keeps errors and tests", `Quick, corpus),
  ],
);
