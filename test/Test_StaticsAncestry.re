open Util;
open Alcotest;
open Haz3lcore;
open Language;

/* the readers of a term's ancestors and parent give the same answers on
   the editor's compositional statics as on a whole-program analysis of
   the same term: Cmd+D, the assistant's node map, autoprobe, outline
   rows and the evaluation worklist. DefStaticsCheck compares the
   ancestors themselves, armed for every test here */

let settings = CoreSettings.on;

let chain_src = "let a = 1 in\nlet b = a + 1 in\ntype T = Int in\nlet c : T = b in\nc";
let e_src = "module M = {\n  let a = 1;\n  let b = a + 1;\n  type T = Int;\n  let c : T = b\n} in\nlet a = 5 in\nlet d : M.T = M.a + a in\nd";
let m_src = "module M = {\n  let a = 1;\n  let b = a + 1;\n  type T = Int;\n  let c : T = b\n};\nlet a = 5;\nlet d : M.T = M.a + a;\nd";
/* a member that is a let chain, at a Mod root */
let mx_src = "let x = 1;\nlet y = x + 1;\nlet a = y in let b = a + 2 in b + x";
let st_src = "let a = 1 in\na + 1;\nlet b = a in\ntest b == 1 end;\nb";
let mt_src = "let a = 1;\ntest a == 1 end;\nlet b = a;\nb";
let nested_src = "module Outer = {\n  module Inner = {\n    type T = Bool;\n    let x = true\n  };\n  let w = 1\n};\nlet y : Outer.Inner.T = Outer.Inner.x;\ny";
let let_mod_src = "let m = {\n  let a = 1;\n  let b = a + 10\n} in\nlet k = 2 in\nm.b + k";
/* a member sealed with a signature: the module's own signature holds it */
let sealed_src = "module M = {\n  let a = 1;\n  module S : { let e : Int } = { let e = 5 };\n  let h = S.e + a\n} in\nM.h";

let programs = [
  ("a let chain", Sort.Exp, chain_src),
  ("a module, then a chain", Sort.Exp, e_src),
  ("a module at a Mod root", Sort.Mod, m_src),
  ("a let-chain member", Sort.Mod, mx_src),
  ("statements", Sort.Exp, st_src),
  ("a test member", Sort.Mod, mt_src),
  ("nested modules", Sort.Mod, nested_src),
  ("a let-bound module", Sort.Exp, let_mod_src),
  ("a sealed sub-module", Sort.Exp, sealed_src),
];

/* the editor's statics, and a whole-program analysis of its term */
let statics = (~root, z: Zipper.t): (CachedStatics.t, CachedStatics.t) => {
  let st =
    CachedStatics.init_compositional(
      ~settings,
      ~stitch=Fun.id,
      ~root,
      ~probe_ids=Id.Map.empty,
      z,
    );
  let (mono, _) = Statics.mk_unmemoized(settings, DefStatics.ctx0, st.term);
  (
    st,
    {
      ...st,
      info_map: mono,
    },
  );
};

/* [src]'s first [needle], [off] columns in */
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

let perform = (~root, ~statics, z: Zipper.t, a: Action.t): option(Zipper.t) =>
  switch (
    Perform.go(
      ~settings,
      ~statics,
      ~syntax=CachedSyntax.init(~root, z),
      ~root,
      a,
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(z) => Some(z)
  | Error(_) => None
  };

let at = (~root, ~statics, src, needle, z) =>
  perform(~root, ~statics, z, Move(Point(point(src, needle), None)));

/* the selection after Cmd+D [n] times from [needle], lines joined */
let grow = (~root, ~statics, ~n, src, needle, z) => {
  let rec go = (n, z) =>
    n == 0
      ? Some(z)
      : Option.bind(
          perform(~root, ~statics, z, Select(Term(Current))),
          go(n - 1),
        );
  switch (Option.bind(at(~root, ~statics, src, needle, z), go(n))) {
  | None => "Cant_select"
  | Some(z) =>
    Printer.of_segment(~holes="?", ~refractors=[], z.selection.content)
    |> String.split_on_char('\n')
    |> List.map(String.trim)
    |> String.concat(" ⏎ ")
  };
};

let cmd_d = (~root, src, needle, expect: list(string), ()) => {
  let z = Zipper.unzip(CorpusUtil.seg_of(~root, src));
  let (editor, whole) = statics(~root, z);
  List.iteri(
    (i, want) =>
      List.iter(
        ((what, statics)) =>
          check(
            string,
            Printf.sprintf("Cmd+D x%d (%s)", i + 1, what),
            want,
            grow(~root, ~statics, ~n=i + 1, src, needle, z),
          ),
        [("compositional", editor), ("monolithic", whole)],
      ),
    expect,
  );
};

let cmd_d_tests = [
  test_case(
    "Cmd+D in a let chain, Exp root",
    `Quick,
    cmd_d(
      ~root=Exp,
      chain_src,
      "a + 1",
      [
        "a",
        "a + 1",
        "let b = a + 1 in",
        "let b = a + 1 in ⏎ type T = Int in ⏎ let c : T = b in ⏎ c",
        "let a = 1 in ⏎ let b = a + 1 in ⏎ type T = Int in ⏎ let c : T = b in ⏎ c",
        "Cant_select",
      ],
    ),
  ),
  test_case(
    "Cmd+D in a let chain after a module, Exp root",
    `Quick,
    cmd_d(
      ~root=Exp,
      e_src,
      "M.a",
      [
        "M",
        "M.a",
        "M.a + a",
        "let d : M.T = M.a + a in",
        "let d : M.T = M.a + a in ⏎ d",
        "let a = 5 in ⏎ let d : M.T = M.a + a in ⏎ d",
        "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ } in ⏎ let a = 5 in ⏎ let d : M.T = M.a + a in ⏎ d",
        "Cant_select",
      ],
    ),
  ),
  test_case(
    "Cmd+D in a let chain, Mod root",
    `Quick,
    cmd_d(
      ~root=Mod,
      mx_src,
      "a + 2",
      [
        "a",
        "a + 2",
        "let b = a + 2 in",
        "let b = a + 2 in b + x",
        "let a = y in let b = a + 2 in b + x",
        "let x = 1; ⏎ let y = x + 1; ⏎ let a = y in let b = a + 2 in b + x",
        "Cant_select",
      ],
    ),
  ),
  /* a member grows to its module, not to the members after it */
  test_case(
    "Cmd+D from a member, Mod root",
    `Quick,
    cmd_d(
      ~root=Mod,
      m_src,
      "a + 1",
      [
        "a",
        "a + 1",
        "let b = a + 1",
        "{ ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }",
        "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }",
        "module M = { ⏎ let a = 1; ⏎ let b = a + 1; ⏎ type T = Int; ⏎ let c : T = b ⏎ }; ⏎ let a = 5; ⏎ let d : M.T = M.a + a; ⏎ d",
        "Cant_select",
      ],
    ),
  ),
];

/* every id of the term's own, as the parse gave them; a Mod root's
   wrapper has no tile (nor info under DefStatics) */
let own_ids = (term: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let grab = (cont, x) => {
    acc := IdTagged.ids(x) @ acc^;
    cont(x);
  };
  ignore(
    Exp.map_term(
      ~f_exp=grab,
      ~f_pat=grab,
      ~f_typ=grab,
      ~f_tpat=grab,
      ~f_rul=grab,
      ~f_mod=grab,
      ~f_sig=grab,
      ~f_mpat=grab,
      term,
    ),
  );
  let root =
    switch (term.term) {
    | Module(_) => [Exp.rep_id(term)]
    | _ => []
    };
  List.sort_uniq(compare, List.filter(id => !List.mem(id, root), acc^));
};

/* an answer per own id, on either statics: the id shown with its class */
let per_id = ((editor: CachedStatics.t, whole: CachedStatics.t), f) => {
  let own = own_ids(editor.term);
  let show = (map, id) =>
    switch (Id.Map.find_opt(id, map)) {
    | Some(i) => Cls.show(Info.cls_of(i)) ++ " " ++ Id.str8(id)
    | None => "no info " ++ Id.str8(id)
    };
  let answers = (st: CachedStatics.t) =>
    List.map(
      id =>
        show(whole.info_map, id)
        ++ ": "
        ++ (
          switch (f(st, id)) {
          | Some(a) => List.mem(a, own) ? show(whole.info_map, a) : "-"
          | None => "-"
          | exception e => "raised " ++ Printexc.to_string(e)
          }
        ),
      own,
    );
  (answers(editor), answers(whole));
};

let same = (name, (editor, whole)) =>
  check(list(string), name, whole, editor);

let consumers = ((name, root, src), ()) => {
  let seg = CorpusUtil.seg_of(~root, src);
  let z = Zipper.unzip(seg);
  let both = statics(~root, z);
  let (editor, whole) = both;
  same(
    name ++ ": parents",
    per_id(both, (st, id) =>
      Option.bind(Statics.Map.lookup(id, st.info_map), Info.parent_id_of)
    ),
  );
  same(
    name ++ ": autoprobe targets",
    per_id(both, (st, id) =>
      AutoProbePerform.toplevel_def_body_id(~statics=st.info_map, ~id)
    ),
  );
  same(
    name ++ ": outline rows",
    per_id(both, (st, id) =>
      Web.OutlineTree.enclosing_row(~info_map=st.info_map, id, st.term)
    ),
  );
  /* the row search climbs no further than it must: the row the item's
     whole ancestry gives, on either statics */
  let row = (st: CachedStatics.t, id) =>
    Web.OutlineTree.enclosing_row(~info_map=st.info_map, id, st.term);
  let row_of_item = (st: CachedStatics.t, id) => {
    let rows = Web.OutlineTree.row_ids(st.term);
    switch (Id.Map.find_opt(id, st.info_map)) {
    | Some(info) =>
      List.find_opt(
        x => Id.Map.mem(x, rows),
        [id, ...Info.within_item(~info_map=st.info_map, info)],
      )
    | None => None
    };
  };
  let ((r_editor, r_whole), (w_editor, w_whole)) = (
    per_id(both, row),
    per_id(both, row_of_item),
  );
  same(name ++ ": the row search", (r_editor, w_editor));
  same(name ++ ": the row search, monolithic", (r_whole, w_whole));
  let pending = (st: CachedStatics.t) =>
    EvalWorklist.pending_ids_uncached(st.info_map)
    |> List.filter(id => List.mem(id, own_ids(st.term)))
    |> List.map(Id.str8)
    |> List.sort(compare);
  check(
    list(string),
    name ++ ": pending evaluation",
    pending(whole),
    pending(editor),
  );
  /* the assistant's node map, from a caret at each line's first token */
  let nodes = (st: CachedStatics.t, z) =>
    switch (HighLevelNodeMap.build(z, st.info_map)) {
    | Some(nm) =>
      Id.Map.bindings(nm)
      |> List.map(((_, n: HighLevelNodeMap.node)) =>
           n.name ++ "@" ++ string_of_int(List.length(n.path))
         )
      |> List.sort(compare)
      |> String.concat(" ")
    | None => "none"
    | exception e => "raised " ++ Printexc.to_string(e)
    };
  List.iteri(
    (row, line) => {
      let rec col = i =>
        i < String.length(line) && line.[i] == ' ' ? col(i + 1) : i;
      let col = col(0);
      switch (
        perform(
          ~root,
          ~statics=editor,
          z,
          Move(
            Point(
              {
                row,
                col,
              }: Point.t,
              None,
            ),
          ),
        )
      ) {
      | Some(z) when String.trim(line) != "" =>
        let at = name ++ ": node map at line " ++ string_of_int(row);
        let got = nodes(editor, z);
        check(
          bool,
          at ++ " builds",
          false,
          String.starts_with(~prefix="raised", got),
        );
        check(string, at, nodes(whole, z), got);
      | _ => ()
      };
    },
    String.split_on_char('\n', src),
  );
};

/* a member's autoprobe target is its definition, as a top-level one is */
let member_autoprobe = () =>
  List.iter(
    ((root, src)) => {
      let seg = CorpusUtil.seg_of(~root, src);
      let (editor, whole) = statics(~root, Zipper.unzip(seg));
      let id = CorpusUtil.tile("+", seg);
      List.iter(
        ((what, st: CachedStatics.t)) =>
          check(
            option(string),
            "the member's definition (" ++ what ++ ")",
            Some(Id.str8(id)),
            Option.map(
              Id.str8,
              AutoProbePerform.toplevel_def_body_id(
                ~statics=st.info_map,
                ~id,
              ),
            ),
          ),
        [("compositional", editor), ("monolithic", whole)],
      );
    },
    [(Sort.Exp, e_src), (Sort.Mod, m_src)],
  );

/* an error badges its row, and rolls up into the rows holding it,
   modules included, but not into the items before it */
let error_rollup = (~root, src, ~own, ~rolls, ~not_rolls, ()) => {
  module O = Web.OutlineTree;
  let z = Zipper.unzip(CorpusUtil.seg_of(~root, src));
  let (editor, whole) = statics(~root, z);
  List.iter(
    ((what, st: CachedStatics.t)) => {
      let nodes = O.flatten(O.of_term(st.term));
      let id_of = label =>
        switch (List.find_opt((n: O.node) => n.o_label == label, nodes)) {
        | Some({o_id: Some(id), _}) => id
        | _ => fail("no row " ++ label)
        };
      let (direct, roll) =
        O.error_rows(
          ~info_map=st.info_map,
          ~rows=O.collect(O.of_term(st.term)),
          st.error_ids,
        );
      check(
        bool,
        what ++ ": " ++ own ++ " owns the error",
        true,
        List.mem(id_of(own), direct),
      );
      List.iter(
        r =>
          check(
            bool,
            what ++ ": " ++ r ++ " rolls it up",
            true,
            List.mem(id_of(r), roll),
          ),
        rolls,
      );
      List.iter(
        r =>
          check(
            bool,
            what ++ ": " ++ r ++ " has no badge",
            false,
            List.mem(id_of(r), roll) || List.mem(id_of(r), direct),
          ),
        not_rolls,
      );
    },
    [("compositional", editor), ("monolithic", whole)],
  );
};

let rollup_tests = [
  test_case(
    "an error rolls up into its module",
    `Quick,
    error_rollup(
      ~root=Mod,
      "module M = {\n  let a = true;\n  let b = a + 1;\n  let c = b\n};\nlet x = 5;\nx",
      ~own="b",
      ~rolls=["M"],
      ~not_rolls=["a", "c", "x"],
    ),
  ),
  test_case(
    "an error rolls up into nested modules",
    `Quick,
    error_rollup(
      ~root=Mod,
      "module Outer = {\n  module Inner = {\n    let v = 1;\n    let x = v + true\n  };\n  let w = 1\n};\nlet y = Outer.Inner.x;\ny",
      ~own="x",
      ~rolls=["Inner", "Outer"],
      ~not_rolls=["v", "w", "y"],
    ),
  ),
  test_case(
    "an error in a let chain stays on its item",
    `Quick,
    error_rollup(
      ~root=Exp,
      "let a = 1 in\nlet b = true + 1 in\nlet c = b in\nc",
      ~own="b",
      ~rolls=[],
      ~not_rolls=["a", "c"],
    ),
  ),
];

let tests = (
  "StaticsAncestry",
  List.map(
    ((name, speed, f)) => (name, speed, DefStaticsCheck.armed(f)),
    cmd_d_tests
    @ rollup_tests
    @ [test_case("a member's autoprobe target", `Quick, member_autoprobe)]
    @ List.map(
        ((name, _, _) as p) =>
          test_case("readers: " ++ name, `Quick, consumers(p)),
        programs,
      ),
  ),
);
