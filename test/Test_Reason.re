/* The ReasonML surface: programs typed (character by character, through
   the editor's insertion path) under the Reason surface, translated by
   MakeTerm onto Hazel terms, then checked and run by the shared back end.
   Each case pins the program's value printed in Hazel syntax, and that
   it has no static errors. */
open Alcotest;
open Haz3lcore;
open Language;

let hazel_text = (e: Exp.t): string =>
  Surface.with_(Hazel, () =>
    ExpToSegment.exp_to_segment(
      ~settings=ExpToSegment.Settings.editable(~inline=true),
      e,
    )
    |> Printer.of_segment
  );

let run = (src: string): (int, string) =>
  Surface.with_(Reason, () =>
    switch (Parser.to_segment(src, ~root=Exp)) {
    | None => failf("did not parse: %s", src)
    | Some(seg) =>
      let term =
        MakeTerm.from_zip_for_sem(Zipper.unzip(seg), ~root=Exp).term;
      let (info_map, elab) =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(None), term);
      let errors = List.length(Statics.Map.error_ids(info_map));
      let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, elab);
      (errors, hazel_text(result));
    }
  );

let case = (name: string, src: string, expected: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let (errors, value) = run(src);
      check(int, "static errors", 0, errors);
      check(string, "value", expected, value);
    },
  );

/* Reason text must survive the typing parser verbatim */
let reprint = (name: string, src: string) =>
  test_case("reprint: " ++ name, `Quick, () =>
    Surface.with_(Reason, () =>
      switch (Parser.to_segment(src, ~root=Exp)) {
      | None => failf("did not parse: %s", src)
      | Some(seg) => check(string, "reprint", src, Printer.of_segment(seg))
      }
    )
  );

let switch_len = {|let rec len = (xs) =>
  switch (xs) {
  | [] => 0
  | [_, ...rest] => 1 + len(rest)
  };
len([1, 2, 3])|};

let shapes = {|type shape =
  | Circle(float)
  | Rect(float, float);
let area = (s: shape): float =>
  switch (s) {
  | Circle(r) => 3.14 *. r *. r
  | Rect(w, h) => w *. h
  };
area(Rect(2., 3.))|};

let tests = (
  "Reason",
  [
    case("let", "let x = 1;\nx + 1", "2"),
    case("arrow", "let f = (x) => x + 1;\nf(41)", "42"),
    case("curried arrow", "let add = (a, b) => a + b;\nadd(1, 2)", "3"),
    case("bare param", "let double = x => x * 2;\ndouble(4)", "8"),
    case(
      "partial application",
      "let add = (a, b) => a + b;\nlet inc = add(1);\ninc(9)",
      "10",
    ),
    case("switch + list spread", switch_len, "3"),
    case("variants + annotations", shapes, "6.000000"),
    case(
      "if/else chain",
      {|let sign = (x) =>
  if (x < 0) {
    "neg"
  } else if (x == 0) {
    "zero"
  } else {
    "pos"
  };
(sign(-3), sign(0), sign(5))|},
      "(\"neg\", \"zero\", \"pos\")",
    ),
    case(
      "records",
      "let p = {x: 1, y: 2};\nlet x = 3;\nlet q = {x, y: 4};\np.x + q.x + q.y",
      "8",
    ),
    case(
      "list building",
      {|let rec map = (f, xs) =>
  switch (xs) {
  | [] => []
  | [x, ...rest] => [f(x), ...map(f, rest)]
  };
map(x => x * 2, [1, 2, 3])|},
      "[2, 4, 6]",
    ),
    case(
      "fun matching",
      {|let describe = fun
  | 0 => "zero"
  | _ => "many";
(describe(0), describe(7))|},
      "(\"zero\", \"many\")",
    ),
    case(
      "type variables are unknown",
      "let id = (x: 'a): 'a => x;\n(id(1), id(true))",
      "(1, true)",
    ),
    case(
      "modules",
      "module M = {\n  let x = 1;\n  let f = (y) => y + x;\n};\nM.f(M.x)",
      "2",
    ),
    case("trailing semicolon in a block", "{ let a = 1; a + 1; }", "2"),
    case(
      "comments",
      "/* a block\n   comment */\nlet x = 1; // trailing\n/* inline */ let s = \"not // one\";\n(x, s)",
      "(1, \"not // one\")",
    ),
    reprint("switch", switch_len),
    reprint("variants", shapes),
  ],
);
