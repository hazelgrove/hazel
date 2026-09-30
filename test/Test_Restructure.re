open Alcotest;
open Haz3lcore;

/* Outline restructure ops (OutlineSidebar.def_op via
   ScratchMode.Restructure.apply): the full op matrix across block
   kinds, asserted on the resulting program TEXT. Every op is
   id-preserving segment surgery; parses use the same FastParse entry
   the app uses. */

module Focus = Web.ScratchMode.Focus;
module R = Web.ScratchMode.Restructure;

let parse = (src: string): Segment.t =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | Some(seg) => seg
  | None =>
    /* the fast path bails on some shapes; recover like persistence */
    switch (MarkerParse.of_text(~root=Exp, src)) {
    | Some(z) => Zipper.unselect_and_zip(z)
    | None => failwith("Test_Restructure: parse failed: " ++ src)
    }
  };

let text_of = (seg: Segment.t): string =>
  MarkerParse.to_text(Zipper.unzip(seg));

let statics_term = (seg: Segment.t): Language.Exp.t => MakeTerm.go(seg).term;

let outline_id = (term, label: string): Id.t => {
  let rec find = (ns: list(Web.OutlineTree.node)) =>
    List.fold_left(
      (acc, n: Web.OutlineTree.node) =>
        switch (acc) {
        | Some(_) => acc
        | None => n.o_label == label ? n.o_id : find(n.o_children)
        },
      None,
      ns,
    );
  switch (find(Web.OutlineTree.of_term(term))) {
  | Some(id) => id
  | None => failwith("no outline row: " ++ label)
  };
};

let contains = (needle, hay) => {
  let nl = String.length(needle)
  and hl = String.length(hay);
  let rec go = i =>
    i + nl <= hl && (String.sub(hay, i, nl) == needle || go(i + 1));
  go(0);
};

let apply_ok = (~src, ~label, ~op, ~desc): string => {
  let seg = parse(src);
  let fid = outline_id(statics_term(seg), label);
  switch (R.apply(op, fid, seg)) {
  | None => failwith("apply returned None: " ++ desc)
  | Some((seg', _)) => text_of(seg')
  };
};

let apply_none = (~src, ~label, ~op, ~desc): unit => {
  let seg = parse(src);
  let fid = outline_id(statics_term(seg), label);
  check(bool, desc, true, R.apply(op, fid, seg) == None);
};

let top_src = "let a = 1 in\nlet b = a + 1 in\ntest b == 2 end;\nb";

let top_level = (): unit => {
  let t =
    apply_ok(
      ~src=top_src,
      ~label="a",
      ~op=Web.OutlineSidebar.NewBelow,
      ~desc="top new-below",
    );
  check(bool, "top new-below inserts", true, contains("new_def", t));
  check(
    bool,
    "top new-below uses an implicit hole",
    true,
    !contains("new_def = ?", t),
  );
  let t =
    apply_ok(
      ~src=top_src,
      ~label="a",
      ~op=Web.OutlineSidebar.NewTypeBelow,
      ~desc="top new-type",
    );
  check(bool, "top new-type inserts", true, contains("type NewType", t));
  let t =
    apply_ok(
      ~src=top_src,
      ~label="a",
      ~op=Web.OutlineSidebar.NewModuleBelow,
      ~desc="top new-module",
    );
  check(
    bool,
    "top new-module inserts an EMPTY module",
    true,
    contains("module NewModule", t) && !contains("member", t),
  );
  let t =
    apply_ok(
      ~src=top_src,
      ~label="b",
      ~op=Web.OutlineSidebar.Delete,
      ~desc="top delete",
    );
  check(bool, "top delete removes", true, !contains("let b", t));
  let t =
    apply_ok(
      ~src=top_src,
      ~label="b",
      ~op=Web.OutlineSidebar.MoveUp,
      ~desc="top move-up",
    );
  check(
    bool,
    "top move-up swaps",
    true,
    String.index(t, 'b') < String.index(t, 'a'),
  );
  let t =
    apply_ok(
      ~src=top_src,
      ~label="a",
      ~op=Web.OutlineSidebar.MoveDown,
      ~desc="top move-down",
    );
  check(
    bool,
    "top move-down swaps",
    true,
    contains("let b = a + 1 in\nlet a = 1 in", t),
  );
  let t =
    apply_ok(
      ~src=top_src,
      ~label="a",
      ~op=Web.OutlineSidebar.Duplicate,
      ~desc="top duplicate",
    );
  check(
    bool,
    "top duplicate doubles",
    true,
    {
      let rec count = (i, acc) =>
        switch (String.index_from_opt(t, i, 'a')) {
        | Some(j) when j + 4 <= String.length(t) => count(j + 1, acc)
        | _ => acc
        };
      ignore(count);
      contains("let a = 1 in\nlet a = 1 in", t);
    },
  );
  /* guards */
  apply_none(
    ~src=top_src,
    ~label="a",
    ~op=Web.OutlineSidebar.MoveUp,
    ~desc="first item can't move up",
  );
};

let statements = (): unit => {
  /* test row (label "1"): delete + move */
  let t =
    apply_ok(
      ~src=top_src,
      ~label="1",
      ~op=Web.OutlineSidebar.Delete,
      ~desc="test delete",
    );
  check(bool, "test delete removes", true, !contains("test b == 2", t));
  let t =
    apply_ok(
      ~src=top_src,
      ~label="1",
      ~op=Web.OutlineSidebar.MoveUp,
      ~desc="test move-up",
    );
  check(
    bool,
    "test moves above b",
    true,
    contains("test b == 2 end;\nlet b", t),
  );
};

let m_src = "module M = {\n  let a = 1;\n  let b = 2;\n} in M.a";

let members = (): unit => {
  let t =
    apply_ok(
      ~src=m_src,
      ~label="a",
      ~op=Web.OutlineSidebar.NewBelow,
      ~desc="member new-below",
    );
  check(
    bool,
    "member new-below is a member with an implicit hole",
    true,
    contains("new_def", t)
    && !contains("new_def = ? in", t)
    && !contains("new_def = ?", t)
    && contains("let b = 2", t),
  );
  let t =
    apply_ok(
      ~src=m_src,
      ~label="M",
      ~op=Web.OutlineSidebar.NewInside,
      ~desc="module new-inside",
    );
  check(
    bool,
    "new-inside lands in the body after b",
    true,
    contains("let b = 2;", t) && contains("new_def", t),
  );
  /* new-inside into an EMPTY module */
  let t2 = {
    let seg = parse("module E = {} in 0");
    let fid = outline_id(statics_term(seg), "E");
    switch (R.apply(Web.OutlineSidebar.NewInside, fid, seg)) {
    | None => failwith("new-inside empty module failed")
    | Some((seg', _)) => text_of(seg')
    };
  };
  check(
    bool,
    "new-inside populates an empty module",
    true,
    contains("module E = {", t2) && contains("new_def", t2),
  );
  /* new-inside after an UNTERMINATED last member (mega style) must
     add a separator first */
  let t3 =
    apply_ok(
      ~src="module U = {\n  let a = 1;\n  let z = fun x -> x\n} in U.a",
      ~label="U",
      ~op=Web.OutlineSidebar.NewInside,
      ~desc="new-inside unterminated tail member",
    );
  check(
    bool,
    "separator added before the appended member",
    true,
    contains("fun x -> x;", t3) && contains("new_def", t3),
  );
};

let fn_body = (): unit => {
  let src = "module N = {\n  let f = fun x ->\n    let y = x + 1 in\n    y * 2;\n} in N.f(1)";
  /* nested let INSIDE a member fn (flattened block): let-in form */
  let t =
    apply_ok(
      ~src,
      ~label="y",
      ~op=Web.OutlineSidebar.NewBelow,
      ~desc="flattened fn-body new-below",
    );
  check(
    bool,
    "flattened insert is a let-in, member intact",
    true,
    contains("new_def", t)
    && contains("y * 2;", t)
    && !contains("new_def = ?", t),
  );
  /* family guard: nested let can't move above its member head */
  apply_none(
    ~src,
    ~label="y",
    ~op=Web.OutlineSidebar.MoveUp,
    ~desc="nested let can't cross its member head",
  );
};

let tests = (
  "Restructure",
  [
    test_case("top-level ops", `Quick, top_level),
    test_case("statement ops", `Quick, statements),
    test_case("member ops", `Quick, members),
    test_case("flattened fn-body ops", `Quick, fn_body),
  ],
);
