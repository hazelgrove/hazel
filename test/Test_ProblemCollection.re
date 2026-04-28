open Alcotest;

module ProblemCollection = Haz3lcore.ProblemCollection;

let from_string =
    (s: string)
    : option(
        (ProblemCollection.problem_context, list(ProblemCollection.problem)),
      ) =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, s)) {
  | None => None
  | Some(z) =>
    let editor = Haz3lcore.Editor.Model.mk(z, ~root=Exp);
    let statics =
      Haz3lcore.CachedStatics.init(
        ~settings=Language.CoreSettings.on,
        ~is_dynamic_term=false,
        ~stitch=Fun.id,
        ~root=Exp,
        editor.state.zipper,
      );
    let ctx =
      ProblemCollection.make_problem_context(
        ~display_warnings=true,
        ~statics,
        ~syntax=editor.syntax,
      );
    let problems = ProblemCollection.collect_all_problems(ctx);
    Some((ctx, problems));
  };

let from_string_exn = (s: string) =>
  switch (from_string(s)) {
  | Some(result) => result
  | None => fail("Failed to parse: " ++ s)
  };

let count_by_category = (cat: ProblemCollection.problem_category, problems) =>
  List.length(
    List.filter(
      (p: ProblemCollection.problem) => p.category == cat,
      problems,
    ),
  );

let has_structural =
    (desc: string, problems: list(ProblemCollection.problem)) =>
  List.exists(
    (p: ProblemCollection.problem) =>
      switch (p.source) {
      | Structural(d) => d == desc
      | FromInfo(_)
      | FromProjector(_) => false
      },
    problems,
  );

let has_multihole_error = (problems: list(ProblemCollection.problem)) =>
  List.exists(
    (p: ProblemCollection.problem) =>
      switch (p.source) {
      | FromInfo(ci) =>
        List.exists(
          m =>
            switch (m) {
            | Language.Mark.IsMulti => true
            | _ => false
            },
          Language.Info.marks_of(ci),
        )
      | _ => false
      },
    problems,
  );

let clean_program = () => {
  let (_, problems) = from_string_exn("let x = 1 in x + 2");
  check(int, "no problems", 0, List.length(problems));
};

let juxtaposed_literals = () => {
  let (_, problems) = from_string_exn("1 2");
  check(
    bool,
    "has missing operator",
    true,
    has_structural("Missing operator", problems),
  );
  check(
    bool,
    "syntax category count > 0",
    true,
    count_by_category(Syntax, problems) > 0,
  );
};

let type_mismatch = () => {
  let (_, problems) = from_string_exn("1 + true");
  check(
    bool,
    "has static errors",
    true,
    count_by_category(Static, problems) > 0,
  );
};

let incomplete_tile = () => {
  let (_, problems) = from_string_exn("if true then 1");
  check(
    bool,
    "has incomplete syntax error",
    true,
    count_by_category(Syntax, problems) > 0,
  );
};

let trailing_unbound_var = () => {
  let (_, problems) = from_string_exn("1\nf");
  check(
    bool,
    "has syntax errors",
    true,
    count_by_category(Syntax, problems) > 0,
  );
  check(
    bool,
    "has multihole error for broken expression",
    true,
    has_multihole_error(problems),
  );
};

let trailing_var_after_let = () => {
  let (_, problems) = from_string_exn("let x = 1 in x\nf");
  check(bool, "has errors for trailing f", true, List.length(problems) > 0);
  check(bool, "has multihole error", true, has_multihole_error(problems));
};

let tests = (
  "ProblemCollection",
  [
    test_case("Clean program has no errors", `Quick, clean_program),
    test_case("Juxtaposed literals", `Quick, juxtaposed_literals),
    test_case("Type mismatch", `Quick, type_mismatch),
    test_case("Incomplete tile", `Quick, incomplete_tile),
    test_case("Trailing unbound var", `Quick, trailing_unbound_var),
    test_case("Trailing var after let", `Quick, trailing_var_after_let),
  ],
);
