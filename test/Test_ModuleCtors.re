open Alcotest;
open Language;

/* A module exports the constructors of its sum type members: `M.A` and
   `M.A(e)` outside the module, in expressions and patterns (#2243). */

let ctx = () => Builtins.ctx_init(Some(Operators.default_mode));

let statics = (src: string): (Statics.Map.t, Exp.t, Exp.t) => {
  let term = Test_Evaluator_Prelude.parse_exp(src);
  let (map, elab) = Statics.mk(CoreSettings.on, ctx(), term);
  (map, term, elab);
};

/* each mark's constructor name, e.g. "FreeConstructor" */
let mark_name = (m: Mark.t): string => {
  let s = Mark.show(m);
  let s =
    String.starts_with(~prefix="(", s)
      ? String.sub(s, 1, String.length(s) - 1) : s;
  let stop =
    switch (String.index_opt(s, '('), String.index_opt(s, ' ')) {
    | (Some(i), Some(j)) => min(i, j)
    | (Some(i), None)
    | (None, Some(i)) => i
    | (None, None) => String.length(s)
    };
  String.sub(s, 0, stop);
};

let marks = (src: string): list(string) => {
  let (map, _, _) = statics(src);
  Id.Map.fold(
    (_, info, acc) => List.map(mark_name, Info.marks_of(info)) @ acc,
    map,
    [],
  )
  |> List.sort_uniq(compare);
};

let no_errors = src =>
  check(list(string), "marks in: " ++ src, [], marks(src));

let has_mark = (name, src) => {
  let ms = marks(src);
  check(
    bool,
    name ++ " among [" ++ String.concat(", ", ms) ++ "] in: " ++ src,
    true,
    List.mem(name, ms),
  );
};

let eval = (src: string): Exp.t => {
  let (map, _, elab) = statics(src);
  if (Statics.Map.error_ids(map) != []) {
    check(list(string), "static errors in: " ++ src, [], marks(src));
  };
  Test_Evaluator_Prelude.evaluate(elab);
};

let same = (src, expected) =>
  check(
    Test_Evaluator_Prelude.testable_exp(~ignore_constructor_types=true, ()),
    src,
    eval(expected),
    eval(src),
  );

/* the root's synthesized type, normalized */
let typ_of = (src: string): Typ.t => {
  let (map, term, _) = statics(src);
  switch (Statics.Map.lookup_exp(Exp.rep_id(term), map)) {
  | Some(info) => Typ.normalize(info.ctx, info.ty)
  | None => fail("no info for the root of: " ++ src)
  };
};

let same_typ = (src, expected_src) =>
  check(
    bool,
    "types of " ++ src ++ " and " ++ expected_src,
    true,
    Typ.fast_equal(
      typ_of(src) |> Typ.all_ids_temp,
      typ_of(expected_src) |> Typ.all_ids_temp,
    ),
  );

let m = "module M = {\n  type t = A(Int) + B;\n  let v = A(3)\n} in\n";

let expressions = [
  test_case(
    "a qualified constructor builds a value",
    `Quick,
    () => {
      no_errors(m ++ "M.A(1)");
      same(m ++ "M.A(1)", "type t = A(Int) + B in A(1)");
      same(m ++ "M.B", "type t = A(Int) + B in B");
    },
  ),
  test_case(
    "its type is the module's sum",
    `Quick,
    () => {
      same_typ(m ++ "M.B", "type t = A(Int) + B in B");
      same_typ(m ++ "M.A", "type t = A(Int) + B in A");
      no_errors(m ++ "let x : M.t = M.A(1) in x");
      has_mark("ExpectationMismatch", m ++ "let x : Int = M.B in x");
    },
  ),
  test_case("a value made inside matches outside", `Quick, () =>
    same(m ++ "case M.v\n| A(x) => x\n| B => 0\nend", "3")
  ),
  test_case("a value made outside matches inside", `Quick, () =>
    same(
      "module M = {\n  type t = A(Int) + B;\n  let get = fun v -> case v\n  | A(x) => x\n  | B => 0\n  end\n} in\nM.get(M.A(4))",
      "4",
    )
  ),
  test_case(
    "constructors are not bare outside",
    `Quick,
    () => {
      has_mark("FreeConstructor", m ++ "B");
      has_mark("FreeConstructor", m ++ "A");
    },
  ),
  test_case("a missing constructor is not a member", `Quick, () =>
    has_mark("ModuleMemberNotFound", m ++ "M.C")
  ),
  test_case("a lowercase module exports them too", `Quick, () =>
    same("let m = {\n  type t = A + B\n} in\nm.B", "type t = A + B in B")
  ),
  test_case("a sub-module's constructors", `Quick, () =>
    same(
      "module M = {\n  module N = {\n    type u = C(Int) + D\n  }\n} in\nM.N.C(2)",
      "type u = C(Int) + D in C(2)",
    )
  ),
  test_case(
    "a recursive type's constructors",
    `Quick,
    () => {
      let l = "module L = {\n  type t = Nil + Cons((Int, t))\n} in\n";
      no_errors(l ++ "L.Cons(1, L.Cons(2, L.Nil))");
      same(
        l ++ "L.Cons(1, L.Nil)",
        "type t = Nil + Cons((Int, t)) in Cons(1, Nil)",
      );
    },
  ),
  test_case(
    "a sub-module beats a constructor only before a dot",
    `Quick,
    () => {
      let src = "module M = {\n  type t = N + O;\n  module N = {\n    let x = 1\n  }\n} in\n";
      same(src ++ "M.N.x", "1");
      same(src ++ "M.N", "type t = N + O in N");
    },
  ),
  test_case("a signature with the sum exports it", `Quick, () =>
    same(
      "module M : { type t = A + B } = {\n  type t = A + B\n} in\nM.A",
      "type t = A + B in A",
    )
  ),
  test_case("a signature hiding the definition hides them", `Quick, () =>
    has_mark(
      "ModuleMemberNotFound",
      "module M : { type t = ? } = {\n  type t = A + B\n} in\nM.A",
    )
  ),
  test_case("a later type's constructor shadows", `Quick, () =>
    same_typ(
      "module M = {\n  type t = A + B;\n  type u = A + C\n} in\nM.A",
      "type u = A + C in A",
    )
  ),
];

let unused = (src: string): list(string) => {
  let (map, _, _) = statics(src);
  Id.Map.fold(
    (_, info, acc) =>
      List.map(
        (Warning.Pat(UnusedVar(name)): Warning.list_item) => name,
        Info.warnings_of(info),
      )
      @ acc,
    map,
    [],
  )
  |> List.sort_uniq(compare);
};

let patterns = [
  test_case(
    "a qualified constructor matches",
    `Quick,
    () => {
      same(m ++ "case M.v\n| M.A(x) => x\n| M.B => 0\nend", "3");
      same(m ++ "case M.B\n| M.A(x) => x\n| M.B => 0\nend", "0");
    },
  ),
  test_case("against a value made inside, either way", `Quick, () => {
    same(
      "module M = {\n  type t = A(Int) + B;\n  let get = fun v -> case v\n  | A(x) => x\n  | B => 0\n  end\n} in\ncase M.A(6)\n| M.A(y) => M.get(M.A(y))\n| M.B => 0\nend",
      "6",
    )
  }),
  test_case(
    "coverage sees the module's constructors",
    `Quick,
    () => {
      no_errors(m ++ "case M.v\n| M.A(x) => x\n| M.B => 0\nend");
      has_mark("InexhaustiveMatch", m ++ "case M.v\n| M.A(x) => x\nend");
    },
  ),
  test_case(
    "in let and fun patterns",
    `Quick,
    () => {
      let w = "module W = {\n  type t = + Wrap(Int)\n} in\n";
      same(w ++ "let W.Wrap(x) = W.Wrap(1) in x", "1");
      same(w ++ "(fun W.Wrap(x) -> x)(W.Wrap(2))", "2");
    },
  ),
  test_case(
    "a sub-module's and a lowercase module's",
    `Quick,
    () => {
      same(
        "module M = {\n  module N = {\n    type u = C(Int) + D\n  }\n} in\ncase M.N.C(7)\n| M.N.C(x) => x\n| M.N.D => 0\nend",
        "7",
      );
      same(
        "let m = {\n  type t = A + B\n} in\ncase m.B\n| m.A => 1\n| m.B => 2\nend",
        "2",
      );
    },
  ),
  test_case(
    "a wrong path or member is reported",
    `Quick,
    () => {
      has_mark(
        "ModuleMemberNotFound",
        m ++ "case M.v\n| M.C => 0\n| _ => 1\nend",
      );
      has_mark("Free", "case 1\n| Q.A => 0\n| _ => 1\nend");
      has_mark(
        "TypWantModule",
        "let q = 1 in\ncase 1\n| q.A => 0\n| _ => 1\nend",
      );
    },
  ),
  test_case("a pattern's module is a use of it", `Quick, () => {
    check(
      list(string),
      "unused",
      ["x"],
      unused(m ++ "fun v -> case v\n| M.A(x) => 0\n| M.B => 1\nend"),
    )
  }),
  test_case("the path binds nothing", `Quick, () =>
    same(
      "let m = {\n  type t = A + B\n} in\ncase m.A\n| m.A => m.B\n| _ => m.A\nend",
      "type t = A + B in B",
    )
  ),
];

let tests = ("ModuleCtors", expressions @ patterns);
