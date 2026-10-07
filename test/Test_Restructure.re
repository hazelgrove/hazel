open Alcotest;
open Haz3lcore;

/* outline restructure ops (ItemEdit.apply) across block kinds, checked on
   the resulting program text */

module Focus = Web.ScratchMode.Focus;
module R = Web.ItemEdit;

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
  apply_none(
    ~src=top_src,
    ~label="a",
    ~op=Web.OutlineSidebar.MoveUp,
    ~desc="first item can't move up",
  );
};

let statements = (): unit => {
  /* the test row is labeled "1" */
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

/* a member typed into a module's name row (ItemEdit.InsertInside) */
let run_inside = (src, label, text) => {
  let seg = parse(src);
  let term = statics_term(seg);
  let ctx: R.ctx = {
    mod_root: false,
    term,
    info_map: lazy(Language.Statics.Map.empty),
    is_open: _ => true,
  };
  R.edit(ctx, InsertInside(outline_id(term, label), text), seg);
};

let inside_ok = (~src, ~label, ~desc): string =>
  switch (run_inside(src, label, "new_def")) {
  | Error(why) => failwith(desc ++ ": " ++ why)
  | Ok((seg', _)) => text_of(seg')
  };

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
  let t = inside_ok(~src=m_src, ~label="M", ~desc="module new-inside");
  check(
    bool,
    "new-inside lands in the body after b",
    true,
    contains("let b = 2;", t) && contains("new_def", t),
  );
  let t2 =
    inside_ok(
      ~src="module E = {} in 0",
      ~label="E",
      ~desc="new-inside empty module",
    );
  check(
    bool,
    "new-inside populates an empty module",
    true,
    contains("module E = {", t2) && contains("new_def", t2),
  );
  let t3 =
    inside_ok(
      ~src="module U = {\n  let a = 1;\n  let z = fun x -> x\n} in U.a",
      ~label="U",
      ~desc="new-inside unterminated tail member",
    );
  check(
    bool,
    "separator added before the appended member",
    true,
    contains("fun x -> x;", t3) && contains("new_def", t3),
  );
};

/* InsertInside: the keyword picks the kind; the target is the new member */
let insert_inside = (): unit => {
  switch (run_inside(m_src, "M", "c")) {
  | Error(why) => fail("value inside: " ++ why)
  | Ok((seg', target)) =>
    let t = text_of(seg');
    check(
      bool,
      "c is appended to M after b",
      true,
      contains("let b = 2;", t) && contains("let c", t),
    );
    check(
      bool,
      "the new member is the row it lands on",
      true,
      target == Some(outline_id(statics_term(seg'), "c")),
    );
  };
  switch (run_inside("module E = {} in 0", "E", "type T")) {
  | Error(why) => fail("type inside an empty module: " ++ why)
  | Ok((seg', _)) =>
    check(
      bool,
      "an empty module gets the type",
      true,
      contains("type T", text_of(seg')),
    )
  };
  switch (run_inside(m_src, "M", "module N")) {
  | Error(why) => fail("module inside: " ++ why)
  | Ok((seg', _)) =>
    check(
      bool,
      "a module inside a module",
      true,
      contains("module N", text_of(seg')),
    )
  };
  check(
    bool,
    "a name that can't bind is refused",
    true,
    Result.is_error(run_inside(m_src, "M", "1x")),
  );
};

/* the last member has no `;`: ops that put a member after it add one */
let unterminated_tail = (): unit => {
  let src = "module U = {\n  let a = 1;\n  let z = fun x -> x\n} in U.a";
  /* U's members, as the outline reads them back */
  let rows_of = (t: string): list(string) =>
    switch (
      List.find_opt(
        (n: Web.OutlineTree.node) => n.o_label == "U",
        Web.OutlineTree.of_term(statics_term(parse(t))),
      )
    ) {
    | Some(u) =>
      List.map((n: Web.OutlineTree.node) => n.o_label, u.o_children)
    | None => []
    };
  check(list(string), "members before", ["a", "z"], rows_of(src));
  let ok = (~label, ~op, ~desc, expected) => {
    let t = apply_ok(~src, ~label, ~op, ~desc);
    check(list(string), desc ++ ": members", expected, rows_of(t));
  };
  ok(~label="z", ~op=Web.OutlineSidebar.MoveUp, ~desc="move up", ["z", "a"]);
  ok(
    ~label="a",
    ~op=Web.OutlineSidebar.MoveDown,
    ~desc="move down",
    ["z", "a"],
  );
  ok(
    ~label="z",
    ~op=Web.OutlineSidebar.Duplicate,
    ~desc="duplicate",
    ["a", "z", "z"],
  );
  ok(
    ~label="z",
    ~op=Web.OutlineSidebar.NewBelow,
    ~desc="new below",
    ["a", "z", "new_def"],
  );
  /* the same module at the top of a module-rooted program */
  let mod_src = "module U = {\n  let a = 1;\n  let z = fun x -> x\n};\nU.a";
  let mod_rows = (seg: Segment.t): list(string) =>
    switch (
      List.find_opt(
        (n: Web.OutlineTree.node) => n.o_label == "U",
        Web.OutlineTree.of_term(MakeTerm.Incr.term_of_mod(seg)),
      )
    ) {
    | Some(u) =>
      List.map((n: Web.OutlineTree.node) => n.o_label, u.o_children)
    | None => []
    };
  let mod_parse = (t: string): Segment.t =>
    switch (FastParse.of_text(~root=Mod, t)) {
    | Some(seg) => seg
    | None => failwith("mod parse failed: " ++ t)
    };
  let seg = mod_parse(mod_src);
  check(list(string), "mod: members before", ["a", "z"], mod_rows(seg));
  let z_id = outline_id(MakeTerm.Incr.term_of_mod(seg), "z");
  let mod_ok = (op, desc, expected) =>
    switch (R.apply(op, z_id, seg)) {
    | None => fail(desc ++ " refused")
    | Some((seg', _)) =>
      check(
        list(string),
        desc ++ ": members",
        expected,
        mod_rows(mod_parse(text_of(seg'))),
      )
    };
  mod_ok(Web.OutlineSidebar.MoveUp, "mod move up", ["z", "a"]);
  mod_ok(Web.OutlineSidebar.Duplicate, "mod duplicate", ["a", "z", "z"]);
  mod_ok(
    Web.OutlineSidebar.NewBelow,
    "mod new below",
    ["a", "z", "new_def"],
  );
};

/* Alt↑↓ across module edges: in at the neighbour's end or start, out to
   just above or below; a collapsed module is stepped over */
let across = (): unit => {
  let rows = (~root, t: string): list(string) => {
    let seg =
      switch (FastParse.of_text(~root, t)) {
      | Some(seg) => seg
      | None =>
        switch (MarkerParse.of_text(~root, t)) {
        | Some(z) => Zipper.unselect_and_zip(z)
        | None => failwith("parse: " ++ String.escaped(t))
        }
      };
    let term =
      root == Sort.Mod
        ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);
    let rec labels = (ns: list(Web.OutlineTree.node)) =>
      List.concat_map(
        (n: Web.OutlineTree.node) =>
          n.o_label == ""
            ? []
            : n.o_children == []
                ? [n.o_label]
                : [
                  n.o_label
                  ++ "{"
                  ++ String.concat(",", labels(n.o_children))
                  ++ "}",
                ],
        ns,
      );
    labels(Web.OutlineTree.of_term(term));
  };
  let mv = (~root, ~open_=true, ~up, src, label, expected) => {
    let seg =
      switch (FastParse.of_text(~root, src)) {
      | Some(seg) => seg
      | None =>
        switch (MarkerParse.of_text(~root, src)) {
        | Some(z) => Zipper.unselect_and_zip(z)
        | None => failwith("parse: " ++ src)
        }
      };
    let term =
      root == Sort.Mod
        ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);
    let id = outline_id(term, label);
    let owner =
      switch (Web.OutlineTree.trail_of(id, term)) {
      | Some(trail) =>
        switch (List.rev(trail)) {
        | [_, parent, ..._]
            when Web.OutlineTree.kind_of(parent, term) == Some(KModule) =>
          Some(parent)
        | _ => None
        }
      | None => None
      };
    switch (
      R.move(
        ~mod_root=root == Sort.Mod,
        ~is_open=_ => open_,
        ~owner,
        ~up,
        id,
        seg,
      )
    ) {
    | None => fail("refused: " ++ label)
    | Some((seg', _)) =>
      check(
        list(string),
        (up ? "up " : "down ") ++ label,
        expected,
        rows(~root, text_of(seg')),
      )
    };
  };
  let exp = "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb";
  mv(~root=Exp, ~up=true, exp, "b", ["a", "M{x,y,b}"]);
  mv(~root=Exp, ~up=false, exp, "a", ["M{a,x,y}", "b"]);
  mv(~root=Exp, ~up=true, exp, "x", ["a", "x", "M{y}", "b"]);
  mv(~root=Exp, ~up=false, exp, "y", ["a", "M{x}", "y", "b"]);
  mv(~root=Exp, ~open_=false, ~up=true, exp, "b", ["a", "b", "M{x,y}"]);
  let md = "let a = 1;\nmodule M = {\n  let x = 2;\n  let y = 3\n};\nlet b = 4;\nb";
  mv(~root=Mod, ~up=true, md, "b", ["a", "M{x,y,b}"]);
  mv(~root=Mod, ~up=false, md, "a", ["M{a,x,y}", "b"]);
  mv(~root=Mod, ~up=true, md, "x", ["a", "x", "M{y}", "b"]);
  mv(~root=Mod, ~up=false, md, "y", ["a", "M{x}", "y", "b"]);
  /* the last member out leaves an empty module */
  let md1 = "let a = 1;\nmodule M = {\n  let x = 2\n};\nlet b = 4;\nb";
  mv(~root=Mod, ~up=false, md1, "x", ["a", "M", "x", "b"]);
  mv(~root=Mod, ~up=true, md1, "x", ["a", "x", "M", "b"]);
  /* an empty module takes an item in, or is stepped over when it can't */
  let e = "let a = 1 in module E = {} in let b = 2 in b";
  mv(~root=Exp, ~up=true, e, "b", ["a", "E{b}"]);
  mv(~root=Exp, ~up=false, e, "a", ["E{a}", "b"]);
  let c = "let a = 1 in\nmodule E = {\n  # todo #\n} in\nlet b = 2 in\nb";
  mv(~root=Exp, ~up=true, c, "b", ["a", "b", "E"]);
};

/* a module's last member, moved out, moves back in where it was */
let back_in = (): unit => {
  let src = "let a = 1;\nmodule M = {\n  let x = 2\n};\nlet b = 4;\nb";
  let parse_mod = t =>
    switch (FastParse.of_text(~root=Mod, t)) {
    | Some(seg) => seg
    | None => failwith("parse: " ++ t)
    };
  let step = (seg, up) => {
    let term = MakeTerm.Incr.term_of_mod(seg);
    let id = outline_id(term, "x");
    let owner =
      switch (Option.map(List.rev, Web.OutlineTree.trail_of(id, term))) {
      | Some([_, parent, ..._]) => Some(parent)
      | _ => None
      };
    switch (R.move(~mod_root=true, ~is_open=_ => true, ~owner, ~up, id, seg)) {
    | Some((seg', _)) => seg'
    | None => fail((up ? "in" : "out") ++ " refused")
    };
  };
  let out = step(parse_mod(src), false);
  check(
    string,
    "out leaves {}",
    text_of(
      parse_mod("let a = 1;\nmodule M = {};\nlet x = 2;\nlet b = 4;\nb"),
    ),
    text_of(out),
  );
  check(string, "and back in", src, text_of(step(out, true)));
};

/* in then out again restores the program, and every step parses */
let round_trip = (): unit => {
  let src = "module T = {\n  let a = 1;\n  let s = 2\n};\n\nmodule D = {\n  let x = 1\n};\n0";
  let seg0 =
    switch (FastParse.of_text(~root=Mod, src)) {
    | Some(seg) => seg
    | None => failwith("parse")
    };
  let step = (seg, label, up) => {
    let term = MakeTerm.Incr.term_of_mod(seg);
    let id = outline_id(term, label);
    let owner =
      switch (Option.map(List.rev, Web.OutlineTree.trail_of(id, term))) {
      | Some([_, parent, ..._])
          when Web.OutlineTree.kind_of(parent, term) == Some(KModule) =>
        Some(parent)
      | _ => None
      };
    switch (R.move(~mod_root=true, ~is_open=_ => true, ~owner, ~up, id, seg)) {
    | None => failwith("refused " ++ label)
    | Some((seg', _)) =>
      ignore(MakeTerm.go_mod_root(seg'));
      seg';
    };
  };
  let seg2 = step(step(seg0, "D", true), "D", false);
  check(string, "back where it was", src, text_of(seg2));
  /* deleting a module's last member leaves no bare `;` */
  let term = MakeTerm.Incr.term_of_mod(seg0);
  switch (
    R.apply(
      ~mod_root=true,
      Web.OutlineSidebar.Delete,
      outline_id(term, "s"),
      seg0,
    )
  ) {
  | None => fail("delete refused")
  | Some((seg', _)) => ignore(MakeTerm.go_mod_root(seg'))
  };
};

let fn_body = (): unit => {
  let src = "module N = {\n  let f = fun x ->\n    let y = x + 1 in\n    y * 2;\n} in N.f(1)";
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
  apply_none(
    ~src,
    ~label="y",
    ~op=Web.OutlineSidebar.MoveUp,
    ~desc="nested let can't cross its member head",
  );
};

/* a member whose body opens with `let … in`: the member's ops take all
   of it, the inner let's stay inside the body */
let let_body = (): unit => {
  let src = "module M = {\n  let x =\n    let y = 1 in\n    y + 1;\n  let z = 2\n} in M.x";
  let rows = (t: string): list(string) =>
    switch (
      List.find_opt(
        (n: Web.OutlineTree.node) => n.o_label == "M",
        Web.OutlineTree.of_term(statics_term(parse(t))),
      )
    ) {
    | Some(m) =>
      List.map(
        (n: Web.OutlineTree.node) =>
          n.o_label
          ++ String.concat(
               "",
               List.map(
                 (c: Web.OutlineTree.node) => "." ++ c.o_label,
                 n.o_children,
               ),
             ),
        m.o_children,
      )
    | None => []
    };
  check(list(string), "before", ["x.y", "z"], rows(src));
  let ok = (~label, ~op, ~desc, expected) => {
    let t = apply_ok(~src, ~label, ~op, ~desc);
    check(list(string), desc, expected, rows(t));
    t;
  };
  let t =
    ok(
      ~label="x",
      ~op=Web.OutlineSidebar.MoveDown,
      ~desc="move down",
      ["z", "x.y"],
    );
  check(bool, "x moves whole", true, contains("y + 1\n}", t));
  ignore(
    ok(
      ~label="z",
      ~op=Web.OutlineSidebar.MoveUp,
      ~desc="move up",
      ["z", "x.y"],
    ),
  );
  ignore(
    ok(
      ~label="x",
      ~op=Web.OutlineSidebar.Duplicate,
      ~desc="duplicate",
      ["x.y", "x.y", "z"],
    ),
  );
  ignore(
    ok(
      ~label="x",
      ~op=Web.OutlineSidebar.NewBelow,
      ~desc="new below",
      ["x.y", "new_def", "z"],
    ),
  );
  let t =
    ok(~label="x", ~op=Web.OutlineSidebar.Delete, ~desc="delete", ["z"]);
  check(bool, "delete takes the body", false, contains("y + 1", t));
  ignore(
    ok(
      ~label="y",
      ~op=Web.OutlineSidebar.NewBelow,
      ~desc="inner new below",
      ["x.y.new_def", "z"],
    ),
  );
  ignore(
    ok(
      ~label="y",
      ~op=Web.OutlineSidebar.Delete,
      ~desc="inner delete",
      ["x", "z"],
    ),
  );
  apply_none(
    ~src,
    ~label="y",
    ~op=Web.OutlineSidebar.MoveUp,
    ~desc="the inner let can't leave its member",
  );
  /* a function member's flattened body: the same */
  let fsrc = "module N = {\n  let f = fun x ->\n    let y = x + 1 in\n    y * 2;\n  let g = 0\n} in N.f(1)";
  let t =
    apply_ok(
      ~src=fsrc,
      ~label="f",
      ~op=Web.OutlineSidebar.MoveDown,
      ~desc="fn member move down",
    );
  check(
    bool,
    "f moves whole",
    true,
    contains("let g = 0;", t) && contains("y * 2\n}", t),
  );
};

/* an op a row's own block refuses does nothing: it never falls back to
   the item around the row */
let refused = (): unit => {
  let fsrc = "let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)";
  let seg = parse(fsrc);
  let term = statics_term(seg);
  let result_row =
    switch (
      List.find_opt(
        (n: Web.OutlineTree.node) => n.o_label == "f",
        Web.OutlineTree.of_term(term),
      )
    ) {
    | Some(f) =>
      switch (
        List.find_opt(
          (n: Web.OutlineTree.node) => n.o_kind == KTrail,
          f.o_children,
        )
      ) {
      | Some({o_id: Some(id), _}) => id
      | _ => failwith("no result row in f")
      }
    | None => failwith("no row f")
    };
  List.iter(
    op =>
      check(
        bool,
        "result row: " ++ Web.OutlineSidebar.show_def_op(op),
        true,
        R.apply(op, result_row, seg) == None,
      ),
    Web.OutlineSidebar.[Delete, Duplicate, MoveUp, MoveDown],
  );
  /* a module's last test without its `;` */
  let msrc = "let z = 0 in\nmodule M = {\n  let a = 1;\n  test 1 + 1 == 2 end\n} in\nM.a";
  apply_none(
    ~src=msrc,
    ~label="1",
    ~op=Web.OutlineSidebar.Delete,
    ~desc="last test: delete",
  );
  apply_none(
    ~src=msrc,
    ~label="1",
    ~op=Web.OutlineSidebar.Duplicate,
    ~desc="last test: duplicate",
  );
  let seg = parse(msrc);
  let term = statics_term(seg);
  check(
    bool,
    "last test: Alt+Up leaves the module put",
    true,
    R.move(
      ~mod_root=false,
      ~is_open=_ => true,
      ~owner=Some(outline_id(term, "M")),
      ~up=true,
      outline_id(term, "1"),
      seg,
    )
    == None,
  );
};

/* ops keep the layout: items keep their lines, new ones take their
   neighbours' indentation, and comment lines directly above an item go
   with it */
let layout = (): unit => {
  let same = (desc, expected, got) =>
    check(string, desc, text_of(parse(expected)), got);
  let op = (src, label, op) =>
    apply_ok(~src, ~label, ~op, ~desc=label ++ " " ++ src);
  let c = "# about a #\nlet a = 1 in\n# about b #\nlet b = 2 in\nb";
  same(
    "comments move with their items",
    "# about b #\nlet b = 2 in\n# about a #\nlet a = 1 in\nb",
    op(c, "b", MoveUp),
  );
  same(
    "and go when they do",
    "# about b #\nlet b = 2 in\nb",
    op(c, "a", Delete),
  );
  let d = "let a = 1 in\n\n# section #\n\nlet b = 2 in\nb";
  same(
    "a comment set apart stays",
    "let b = 2 in\n\n# section #\n\nlet a = 1 in\nb",
    op(d, "b", MoveUp),
  );
  let m = "module M = {\n  let a = 1;\n  let b = 2\n} in\nM.a";
  same(
    "member down",
    "module M = {\n  let b = 2;\n  let a = 1\n} in\nM.a",
    op(m, "a", MoveDown),
  );
  same(
    "first member deleted",
    "module M = {\n  let b = 2\n} in\nM.a",
    op(m, "a", Delete),
  );
  same(
    "last member deleted",
    "module M = {\n  let a = 1\n} in\nM.a",
    op(m, "b", Delete),
  );
  same(
    "member duplicated",
    "module M = {\n  let a = 1;\n  let a = 1;\n  let b = 2\n} in\nM.a",
    op(m, "a", Duplicate),
  );
  same(
    "new member below the last",
    {js|module M = {
  let a = 1;
  let b = 2;
  let new_def = ¿
} in
M.a|js},
    op(m, "b", NewBelow),
  );
  let t = "module M = {\n  let a = 1;\n  let b = 2;\n} in\nM.a";
  switch (run_inside(t, "M", "c")) {
  | Ok((seg', _)) =>
    same(
      "new member inside after a last `;`",
      {js|module M = {
  let a = 1;
  let b = 2;
  let c = ¿;
} in
M.a|js},
      text_of(seg'),
    )
  | Error(why) => fail("inside: " ++ why)
  };
  same(
    "a last `;` moves with the last place",
    "module M = {\n  let b = 2;\n  let a = 1;\n} in\nM.a",
    op(t, "b", MoveUp),
  );
  let f = "let f = fun x ->\n  let y = x + 1 in\n  y * 2\nin\nf(1)";
  same(
    "a function body's new let",
    {js|let f = fun x ->
  let y = x + 1 in
  let new_def = ¿ in
  y * 2
in
f(1)|js},
    op(f, "y", NewBelow),
  );
  /* across module edges an item is reindented to where it lands */
  let x = "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3\n} in\nlet b = 4 in\nb";
  let mv = (~up, src, label) => {
    let seg = parse(src);
    let term = statics_term(seg);
    switch (
      R.move(
        ~mod_root=false,
        ~is_open=_ => true,
        ~owner=
          label == "b" || label == "f" ? None : Some(outline_id(term, "M")),
        ~up,
        outline_id(term, label),
        seg,
      )
    ) {
    | Some((seg', _)) => text_of(seg')
    | None => fail("move refused: " ++ label)
    };
  };
  same(
    "into a module",
    "let a = 1 in\nmodule M = {\n  let x = 2;\n  let y = 3;\n  let b = 4\n} in\nb",
    mv(~up=true, x, "b"),
  );
  same(
    "out of a module",
    "let a = 1 in\nlet x = 2 in\nmodule M = {\n  let y = 3\n} in\nlet b = 4 in\nb",
    mv(~up=true, x, "x"),
  );
  same(
    "a function into a module",
    "module M = {\n  let f = fun x ->\n    x + 1;\n  let y = 3\n} in\nM.y",
    mv(
      ~up=false,
      "let f = fun x ->\n  x + 1\nin\nmodule M = {\n  let y = 3\n} in\nM.y",
      "f",
    ),
  );
};

let tests = (
  "Restructure",
  [
    test_case("a refused op does nothing", `Quick, refused),
    test_case("ops keep the layout", `Quick, layout),
    test_case("top-level ops", `Quick, top_level),
    test_case("statement ops", `Quick, statements),
    test_case("member ops", `Quick, members),
    test_case("a named member inside a module", `Quick, insert_inside),
    test_case("flattened fn-body ops", `Quick, fn_body),
    test_case("a member whose body opens with a let", `Quick, let_body),
    test_case("unterminated last member", `Quick, unterminated_tail),
    test_case("moves across module edges", `Quick, across),
    test_case("a last member out and back in", `Quick, back_in),
    test_case("in and out again", `Quick, round_trip),
  ],
);
