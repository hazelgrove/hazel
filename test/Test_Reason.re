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
        MakeTerm.from_zip_for_sem(Zipper.unzip(seg), ~root=Exp).term
        |> ReasonPrelude.wrap;
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

/* Type `typed` at the start of a loaded program, then run it */
let edit_case = (name: string, program: string, typed: string, expected) =>
  test_case("edit: " ++ name, `Quick, () =>
    Surface.with_(
      Reason,
      () => {
        let seg = ReasonLoad.to_segment(~root=Exp, program);
        let z =
          Token.to_list(typed)
          |> List.fold_left(
               (z, c) =>
                 Insert.go(c == "\n" ? Token.linebreak : c, z, ~root=Exp)
                 |> Option.get,
               Zipper.unzip(~direction=Left, seg),
             );
        let term =
          MakeTerm.from_zip_for_sem(z, ~root=Exp).term |> ReasonPrelude.wrap;
        let (info_map, elab) =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(None), term);
        check(
          int,
          "static errors",
          0,
          List.length(Statics.Map.error_ids(info_map)),
        );
        let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, elab);
        check(string, "value", expected, hazel_text(result));
      },
    )
  );

/* Typing a program (the editor's insertion path) and loading it in bulk
   (ReasonLoad) build the same syntax */
let typed_like_loaded = (name: string, src: string) =>
  test_case("typed = loaded: " ++ name, `Quick, () =>
    Surface.with_(
      Reason,
      () => {
        let typed = Parser.typing_to_segment(src, ~root=Exp) |> Option.get;
        let loaded = ReasonLoad.to_segment(~root=Exp, src);
        let text = Printer.of_segment;
        let term = seg =>
          MakeTerm.from_zip_for_sem(Zipper.unzip(seg), ~root=Exp).term
          |> hazel_text;
        check(string, "text", text(typed), text(loaded));
        check(string, "term", term(typed), term(loaded));
      },
    )
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

/* Every tile of a program has statics info for the cursor inspector to
   show (Reason tiles the translation consumes borrow another term's) */
let rec tiles = (seg: Segment.t): list(Tile.t) =>
  List.concat_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) => [t, ...List.concat_map(tiles, t.children)]
      | _ => []
      },
    seg,
  );
let statics_of = (seg: Segment.t) =>
  CachedStatics.init(
    ~settings=CoreSettings.on,
    ~is_dynamic_term=false,
    ~stitch=Fun.id,
    ~root=Exp,
    Zipper.unzip(seg),
  );
let inspected = (name: string, src: string) =>
  test_case("inspector: " ++ name, `Quick, () =>
    Surface.with_(
      Reason,
      () => {
        let seg = ReasonLoad.to_segment(~root=Exp, src);
        let st = statics_of(seg);
        let missing =
          tiles(seg)
          |> List.filter((t: Tile.t) => !Id.Map.mem(t.id, st.info_map))
          |> List.map((t: Tile.t) => String.concat(" ", Tile.label(t)));
        check(list(string), "tiles without info", [], missing);
      },
    )
  );

/* `and p =` is one tile with the pattern as its child, and the cursor on
   it describes that binding */
let and_binding =
  test_case("and: a binding tile", `Quick, () =>
    Surface.with_(
      Reason,
      () => {
        let src = "let rec even = n => n == 0 || odd(n - 1)\nand odd = n => n != 0 && even(n - 1);\neven(4)";
        let seg = ReasonLoad.to_segment(~root=Exp, src);
        let ands =
          tiles(seg)
          |> List.filter((t: Tile.t) => t.form == Compound(ReAnd));
        check(int, "one and tile", 1, List.length(ands));
        let t = List.hd(ands);
        check(list(string), "label", ["and", "="], Tile.label(t));
        check(
          string,
          "child",
          "odd",
          Printer.of_segment(List.hd(t.children)) |> String.trim,
        );
        let st = statics_of(seg);
        let cls =
          Id.Map.find_opt(t.id, st.info_map) |> Option.map(Info.cls_label);
        check(option(string), "describes", Some("Variable binding"), cls);
        check(int, "static errors", 0, List.length(st.error_ids));
      },
    )
  );

let forms = {|// One of each Reason form the surface supports
[@ocaml.warning "-27"]
open List;

exception Not_here(string);

type color =
  | Red
  | Green
  | Rgb(int, int, int);

type point = {x: int, y: int};

type tree('a) =
  | Leaf
  | Node(tree('a), 'a, tree('a));

type expr =
  | Num(int)
  | Add(expr, expr)
and stmt =
  | Print(expr)
  | Seq(list(stmt));

module Geo = {
  let origin = {x: 0, y: 0};
  let shift = (p: point, dx: int): point => {...p, x: p.x + dx};
};

let rec even = n => if (n == 0) {true} else {odd(n - 1)}
and odd = n => if (n == 0) {false} else {even(n - 1)};

let name = (c: color): string =>
  switch (c) {
  | Red => "red"
  | Green as g => "green"
  | Rgb(r, _, _) when r > 200 => "reddish"
  | Rgb(_) => "mixed"
  };

let first =
  fun
  | [] => None
  | [x, ..._] => Some(x);

let sum3 = (a, b, c) => a + b + c;
let labeled = (~root, ~depth) => root * depth;
let block = {
  let a = 1;
  let b = 2;
  a + b;
};
let x = {x: 1, y: 2};
let y = x.y;
let tup = (1, "two", 3.);
let lst = [1, 2, ...[3]];
let piped = [1, 2, 3] |> List.map(n => n * 2);
let cmp = if (sum3(1, 2, 3) > 5) {"big"} else if (y < 0) {"neg"} else {"small"};
let shifted = Geo.shift(Geo.origin, 3);
let fact = labeled(~root=2, ~depth=3);
let opt: option(int) = first(lst);
let strs = "a" ++ "b";
let flt = 1.5 *. 2.;
let neg = - 3;
let bools = true && !false || even(4);
let unit_ = ();
let hole = ?;
(name(Rgb(250, 0, 0)), shifted, block, fact, opt)|};

let guard_call = {|let f = (a, b) => a < b;
let g = (x) =>
  switch (x) {
  | (p, q) when f(p, q) => 1
  | _ => 0
  };
g((1, 2))|};

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
    case(
      "prelude",
      {|let xs = [3, 1, 2];
(List.length(xs), List.fold_left((a, b) => a + b, 0, xs), List.sort((a, b) => a - b, xs), String.concat(", ", List.map(string_of_int, xs)), Option.map((x) => x + 1, Some(1)))|},
      "(3, 6, [1, 2, 3], \"3, 1, 2\", Some(2))",
    ),
    edit_case(
      "a definition typed above an expression",
      "(1, 2)",
      "let d = List.map((x) => x * 2, [1]);\n",
      "(1, 2)",
    ),
    case(
      "or-patterns",
      {|type shape =
  | Convex
  | Concave(int);
let fits = (l, r) =>
  switch (l, r) {
  | (Convex, Concave(_))
  | (Concave(_), Convex) => true
  | (Convex, Convex)
  | (Concave(_), Concave(_)) => false
  };
(fits(Convex, Concave(3)), fits(Convex, Convex))|},
      "(true, false)",
    ),
    case(
      "guards",
      {|let classify = (n) =>
  switch (n) {
  | 0 => "zero"
  | n when n < 0 => "negative"
  | n when n > 100 => "big"
  | _ => "small"
  };
(classify(-5), classify(500), classify(7))|},
      "(\"negative\", \"big\", \"small\")",
    ),
    case(
      "a failed guard falls through in the switch's scope",
      {|let f = (x, y) =>
  switch (x, y) {
  | (Some(x), _) when x > 10 => "big"
  | (_, y) => y
  };
(f(Some(1), "fell through"), f(Some(11), "no"))|},
      "(\"fell through\", \"big\")",
    ),
    case(
      "as-patterns",
      {|let swap = ((a, b) as pair) => ((b, a), pair);
let tail_or = (d, xs) =>
  switch (xs) {
  | [_, ..._] as l => List.tl(l)
  | [] => d
  };
(swap((1, 2)), tail_or([], [1, 2]))|},
      "(((2, 1), (1, 2)), [2])",
    ),
    case(
      "record update, trailing commas, attributes, open",
      {|open Util;
[@deriving show]
type point = {x: int, y: int,};
let p = {x: 1, y: 2,};
let q = {...p, y: 20};
(q.x + q.y, [1, 2,])|},
      "(21, [1, 2])",
    ),
    case(
      "and: mutual recursion, recursive types; exceptions; labeled args",
      {|type t =
  | Op(root)
  | Pre(root, t)
and root = (list(int), list(t));
exception Nonconvex(int);
let rec even = (n) => if (n == 0) {true} else {odd(n - 1)}
and odd = (n) => if (n == 0) {false} else {even(n - 1)};
let scale = (~factor, ~offset=0, x) => x * factor + offset;
(even(10), odd(7), scale(~factor=3, ~offset=1, 5), Op(([1], [])))|},
      "(true, true, 16, Op([1], []))",
    ),
    typed_like_loaded("switch", switch_len),
    typed_like_loaded("variants", shapes),
    typed_like_loaded(
      "fun matching",
      "let describe = fun\n  | 0 => \"zero\"\n  | _ => \"many\";\ndescribe(1)",
    ),
    typed_like_loaded(
      "modules and comments",
      "/* doc\n   comment */\nmodule M = {\n  let x = 1; // one\n  let f = (y) => y + x;\n};\nM.f(M.x)",
    ),
    typed_like_loaded("guard with a call", guard_call),
    typed_like_loaded("every form", forms),
    reprint("switch", switch_len),
    reprint("variants", shapes),
    inspected("every form", forms),
    inspected("guards", guard_call),
    and_binding,
  ],
);
