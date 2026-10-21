open Util;
open Alcotest;
open Haz3lcore;
open Language;

/* `M.x` and `M.T` as references to the member (Info.binding_site): F12,
   the inspector's reading and highlights, on the editor's compositional
   statics and the assistant's monolithic ones */

let m_src = "module M = {\n  let a = 1;\n  let b = a + 1;\n  type T = Int;\n  let c : T = b\n};\nlet a = 5;\nlet d : M.T = M.a + a;\nd";
let e_src = "module M = {\n  let a = 1;\n  let b = a + 1;\n  type T = Int;\n  let c : T = b\n} in\nlet a = 5 in\nlet d : M.T = M.a + a in\nd";
let nested_src = "module Outer = {\n  module Inner = {\n    type T = Bool;\n    let x = true\n  };\n  let w = 1\n};\nlet y : Outer.Inner.T = Outer.Inner.x;\ny";

let load = (~root, src) => Zipper.unzip(CorpusUtil.seg_of(~root, src));

/* the editor's statics */
let compositional = (~root, z: Zipper.t): Statics.Map.t =>
  CachedStatics.init_compositional(
    ~settings=CoreSettings.on,
    ~stitch=Fun.id,
    ~root,
    ~probe_ids=Id.Map.empty,
    z,
  ).
    info_map;

/* the assistant's */
let monolithic = (~root, z: Zipper.t): Statics.Map.t =>
  CompositionGo.Public.mk_statics(~root, z);

let both = [("compositional", compositional), ("monolithic", monolithic)];

/* the row and column of the first [needle] in [src], [off] columns in */
let point = (~off=0, src: string, needle: string): Point.t => {
  let rec find = (row, lines) =>
    switch (lines) {
    | [] => fail("no " ++ needle)
    | [l, ...rest] =>
      switch (Str.search_forward(Str.regexp_string(needle), l, 0)) {
      | c => (
          {
            row,
            col: c + off,
          }: Point.t
        )
      | exception Not_found => find(row + 1, rest)
      }
    };
  find(0, String.split_on_char('\n', src));
};

/* [src] with the caret at [needle], [off] columns in */
let at = (~off=0, ~root, src: string, needle: string): Zipper.t => {
  let z = load(~root, src);
  switch (
    Perform.go(
      ~settings=CoreSettings.on,
      ~statics=CachedStatics.empty,
      ~syntax=CachedSyntax.init(~root, z),
      ~root,
      Move(Point(point(~off, src, needle), None)),
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => z
  | Error(f) =>
    fail("no caret at " ++ needle ++ ": " ++ Action.Failure.show(f))
  };
};

let indicated = (~info_map, z: Zipper.t): Info.t =>
  switch (Indicated.ci_of(z, info_map)) {
  | Some(ci) => ci
  | None => fail("no info at the caret")
  };

/* a token of [z]'s, as `row:col line` */
let place = (~root, src, z: Zipper.t, id: Id.t): string => {
  let measured = CachedSyntax.measured(CachedSyntax.init(~root, z));
  switch (Measured.find_by_id(id, measured)) {
  | Some(m) =>
    let line = List.nth(String.split_on_char('\n', src), m.origin.row);
    Printf.sprintf(
      "%d:%d %s",
      m.origin.row,
      m.origin.col,
      String.trim(line),
    );
  | None => "unmeasured"
  };
};

/* where F12 lands, from the editor's statics */
let f12 = (~off=0, ~root, src, needle): string => {
  let z = at(~off, ~root, src, needle);
  let info_map = compositional(~root, z);
  switch (
    Perform.go(
      ~settings=CoreSettings.on,
      ~statics={
        ...CachedStatics.empty,
        info_map,
      },
      ~syntax=CachedSyntax.init(~root, z),
      ~root,
      Move(Goal(BindingSiteOfIndicatedVar)),
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) =>
    let pt =
      Zipper.Caret.point(
        CachedSyntax.measured(CachedSyntax.init(~root, z)),
        z,
      );
    let line = List.nth(String.split_on_char('\n', src), pt.row);
    Printf.sprintf("%d:%d %s", pt.row, pt.col, String.trim(line));
  | Error(f) => Action.Failure.show(f)
  };
};

/* the binder the token at the caret names */
let names = (~info_map, ~root, src, z): string =>
  switch (Info.binding_site(~info_map, indicated(~info_map, z))) {
  | None => "nothing"
  | Some(b) => place(~root, src, z, b)
  };

/* the cursor's reading, as the inspector shows it: class and type */
let inspect = (~info_map, z): string =>
  switch (Info.as_member_reference(~info_map, indicated(~info_map, z))) {
  | InfoExp(e) =>
    Info.cls_label(InfoExp(e)) ++ " : " ++ Typ.pretty_print(e.ty)
  | InfoTyp(
      {message: Some(TypOk(WHNormalizedTo({whnormalized, _}))), _} as t,
    ) =>
    Info.cls_label(InfoTyp(t)) ++ " = " ++ Typ.pretty_print(whnormalized)
  | info => Info.cls_label(info)
  };

let roots = [(Sort.Mod, m_src), (Sort.Exp, e_src)];

let go_to_definition = () =>
  List.iter(
    ((root, src)) => {
      let r = " [" ++ Sort.show(root) ++ "]";
      check(
        string,
        "M.a's a" ++ r,
        "1:6 let a = 1;",
        f12(~off=2, ~root, src, "M.a"),
      );
      check(
        string,
        "M.T's T" ++ r,
        "3:7 type T = Int;",
        f12(~off=2, ~root, src, "M.T"),
      );
      check(
        string,
        "M.a's M" ++ r,
        "0:7 module M = {",
        f12(~off=0, ~root, src, "M.a"),
      );
    },
    roots,
  );

let nested = () =>
  List.iter(
    ((what, mk)) => {
      let root = Sort.Mod;
      let names_at = (needle, off, expected) => {
        let z = at(~off, ~root, nested_src, needle);
        check(
          string,
          what ++ ": " ++ needle ++ " +" ++ string_of_int(off),
          expected,
          names(~info_map=mk(~root, z), ~root, nested_src, z),
        );
      };
      names_at("Outer.Inner.x", 12, "3:8 let x = true");
      names_at("Outer.Inner.x", 6, "1:9 module Inner = {");
      names_at("Outer.Inner.T", 12, "2:9 type T = Bool;");
      names_at("Outer.Inner.T", 6, "1:9 module Inner = {");
      check(
        string,
        "F12 on Outer.Inner.x's x",
        "3:8 let x = true",
        f12(~off=12, ~root, nested_src, "Outer.Inner.x"),
      );
    },
    both,
  );

/* what `.x` names: through aliases, value-bound modules, the last member
   so named, a function member; not a labeled tuple's label */
let resolution = () => {
  let cases = [
    (
      "an alias",
      "module Base = {\n  let x = 1\n};\nmodule Db = Base;\nlet y = Db.x;\ny",
      "Db.x",
      "1:6 let x = 1",
    ),
    (
      "a let-bound module",
      "let m = {\n  let x = 1\n};\nlet y = m.x;\ny",
      "m.x",
      "1:6 let x = 1",
    ),
    (
      "the last member so named",
      "module M = {\n  let x = 1;\n  let x = 2\n};\nM.x",
      "M.x",
      "2:6 let x = 2",
    ),
    (
      "a function member",
      "module M = {\n  let f(n) = n + 1\n};\nM.f(2)",
      "M.f",
      "1:6 let f(n) = n + 1",
    ),
    ("a labeled tuple's label", "let t = (x = 1);\nt.x", "t.x", "nothing"),
  ];
  List.iter(
    ((what, mk)) =>
      List.iter(
        ((case, src, needle, expected)) => {
          /* the caret on the label */
          let z =
            at(~off=String.index(needle, '.') + 1, ~root=Mod, src, needle);
          check(
            string,
            what ++ ": " ++ case,
            expected,
            names(~info_map=mk(~root=Sort.Mod, z), ~root=Mod, src, z),
          );
        },
        cases,
      ),
    both,
  );
};

let inspector = () =>
  List.iter(
    ((what, mk)) => {
      List.iter(
        ((root, src)) => {
          let r = " " ++ what ++ " [" ++ Sort.show(root) ++ "]";
          let z = at(~off=2, ~root, src, "M.a");
          check(
            string,
            "M.a's a" ++ r,
            "Variable reference : Int",
            inspect(~info_map=mk(~root, z), z),
          );
          let z = at(~off=2, ~root, src, "M.T");
          check(
            string,
            "M.T's T" ++ r,
            "Type variable = Int",
            inspect(~info_map=mk(~root, z), z),
          );
        },
        roots,
      );
      /* through a nested module */
      let z = at(~off=12, ~root=Mod, nested_src, "Outer.Inner.x");
      check(
        string,
        "Outer.Inner.x's x " ++ what,
        "Variable reference : Bool",
        inspect(~info_map=mk(~root=Sort.Mod, z), z),
      );
      let z = at(~off=12, ~root=Mod, nested_src, "Outer.Inner.T");
      check(
        string,
        "Outer.Inner.T's T " ++ what,
        "Type variable = Bool",
        inspect(~info_map=mk(~root=Sort.Mod, z), z),
      );
      /* a tuple's label stays a label */
      let src = "let t = (x = 1) in\nt.x";
      let z = at(~off=2, ~root=Exp, src, "t.x");
      check(
        string,
        "t.x's x " ++ what,
        "Dot operator : x",
        inspect(~info_map=mk(~root=Sort.Exp, z), z),
      );
    },
    both,
  );

/* the caret on the member highlights its use and its `M.a` label, and so
   does the label; the top-level `a` stays out */
let highlights = () =>
  List.iter(
    ((root, src)) => {
      let hl = (needle, off) => {
        let z = at(~off, ~root, src, needle);
        let info_map = compositional(~root, z);
        Statics.Map.var_highlight_ids(info_map, indicated(~info_map, z))
        |> List.map(id =>
             switch (
               Measured.find_by_id(
                 id,
                 CachedSyntax.measured(CachedSyntax.init(~root, z)),
               )
             ) {
             | Some(m) => [
                 Printf.sprintf("%d:%d", m.origin.row, m.origin.col),
               ]
             /* the module's export tuple, which no token shows */
             | None => []
             }
           )
        |> List.concat
        |> List.sort(compare)
        |> String.concat(" ");
      };
      let r = " [" ++ Sort.show(root) ++ "]";
      check(string, "from the member" ++ r, "1:6 2:10 7:16", hl("a = 1", 0));
      check(string, "from M.a's a" ++ r, "1:6 2:10 7:16", hl("M.a", 2));
    },
    roots,
  );

let tests = (
  "MemberRefs",
  [
    test_case("F12 on M.a and M.T", `Quick, go_to_definition),
    test_case("nested modules", `Quick, nested),
    test_case("what a dot's label names", `Quick, resolution),
    test_case("the inspector's reading", `Quick, inspector),
    test_case("highlights", `Quick, highlights),
  ],
);
