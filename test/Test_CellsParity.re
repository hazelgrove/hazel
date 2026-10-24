open Alcotest;
open Haz3lcore;
open Language;

/* ScratchFocus and ItemEdit on ItemSpans against their frozen selves
   (FocusOracle, ItemEditOracle) over the corpora: every outline row's
   cell, splice and refusal, every row's restructure ops, and on small
   programs every tile and piece. Differences by design are named where
   they are allowed */

module F = Web.ScratchFocus;
module F0 = FocusOracle;
module IE = Web.ItemEdit;
module IE0 = ItemEditOracle;
module O = Web.OutlineTree;

let text_of = CorpusUtil.text_of;
let ids = Segment.ids;
let show_ids = (s: Segment.t) =>
  String.concat(",", List.map(Id.show, ids(s)));

/* a segment as both sides must give it: its text and its pieces' ids */
let seg_key = (s: Segment.t) => text_of(s) ++ "\n@" ++ show_ids(s);

/* a program made by an op, with fresh pieces: its text, and the ids it
   kept of [before]'s, in order */
let made_key = (~before: Id.Map.t(unit), s: Segment.t) =>
  text_of(s)
  ++ "\n@"
  ++ String.concat(
       ",",
       List.filter_map(
         id => Id.Map.mem(id, before) ? Some(Id.show(id)) : None,
         ids(s),
       ),
     );
let row_key = (~before: Id.Map.t(unit), row: option(Id.t)) =>
  switch (row) {
  | None => "none"
  | Some(id) when Id.Map.mem(id, before) => Id.show(id)
  | Some(_) => "fresh"
  };

/* the same, whitespace aside: text and kept ids */
let solid_key = (~before: Id.Map.t(unit), s: Segment.t) => {
  let rec solid = (seg: Segment.t) =>
    List.concat_map(
      (p: Piece.t) =>
        switch (p) {
        | Secondary(w) when !Haz3lcore.Secondary.is_comment(w) => []
        | Tile(t) => [t.id, ...List.concat_map(solid, t.children)]
        | p => [Piece.id(p)]
        },
      seg,
    );
  Str.global_replace(Str.regexp("[ \n]+"), "", text_of(s))
  ++ "\n@"
  ++ String.concat(
       ",",
       List.filter_map(
         id => Id.Map.mem(id, before) ? Some(Id.show(id)) : None,
         solid(s),
       ),
     );
};

/* ItemEdit's rules that read differently by design: moves and deletes
   carry spacing by the trivia rule (which gap an item takes along, and
   its indentation's offset), so results may differ in whitespace alone;
   and a statement doesn't go into a member's body, where its `;` would
   end the member */
let trivia_rule = "the trivia rule";
let stmt_rule = "no statement into a member's body";
/* and an item goes into the item beside it (Tab, Alt into a module) only
   with no comment set apart between them: it couldn't come back to its
   side of the comment */
let apart_rule = "nothing goes in across a comment set apart";
let item_rules = [trivia_rule, stmt_rule, apart_rule];

type tally = {
  mutable programs: int,
  mutable checks: int,
  counts: Hashtbl.t(string, int),
  /* (program, the rule that reads it differently by design (none: a
     failure), what differs) */
  mutable diffs: list((string, option(string), string)),
};
let tally = () => {
  programs: 0,
  checks: 0,
  counts: Hashtbl.create(16),
  diffs: [],
};

/* old and new, each run caught: equal or a diff. [what] is counted,
   [at] names the row in a diff, [why] says if it differs by design, as
   does [design] from the two results */
let same =
    (
      t: tally,
      ~name: string,
      ~why: unit => option(string),
      ~design=(_, _) => None,
      ~at="",
      what: string,
      old_f,
      new_f,
      key,
    ) => {
  t.checks = t.checks + 1;
  Hashtbl.replace(
    t.counts,
    what,
    1 + Option.value(Hashtbl.find_opt(t.counts, what), ~default=0),
  );
  let run = f =>
    switch (f()) {
    | v => Ok(v)
    | exception e => Error("raised " ++ Printexc.to_string(e))
    };
  let (va, vb) = (run(old_f), run(new_f));
  let keyed =
    fun
    | Ok(v) => key(v)
    | Error(e) => e;
  let (a, b) = (keyed(va), keyed(vb));
  if (a != b) {
    let why =
      switch (va, vb) {
      | (Ok(va), Ok(vb)) =>
        switch (design(va, vb)) {
        | Some(_) as d => d
        | None => why()
        }
      | _ => why()
      };
    t.diffs = [
      (
        name,
        why,
        Printf.sprintf("%s %s\n  old: %s\n  new: %s", what, at, a, b),
      ),
      ...t.diffs,
    ];
  };
};

let opt = (f, o) =>
  switch (o) {
  | None => "None"
  | Some(x) => "Some(" ++ f(x) ++ ")"
  };

let indent_key = (ind: Web.ScratchCell.indent) =>
  Printf.sprintf(
    "base %d cut %s orig %s",
    ind.base,
    seg_key(ind.cut),
    seg_key(ind.orig),
  );

/* a cell: its kind, panes, indents and ctx (headless headers are a
   fresh grout) */
let cell_key = (e: Web.ScratchCell.t) =>
  String.concat(
    "\n",
    [
      Id.show(e.e_id),
      Web.ScratchCell.show_kind(e.e_kind),
      switch (e.e_kind) {
      | Headless(_)
      | Run(_) => "-"
      | Def(_)
      | Members => seg_key(F.zip_of_cell(e.e_header))
      },
      seg_key(F.zip_of_cell(e.e_body)),
      indent_key(e.e_header_indent),
      indent_key(e.e_body_indent),
      string_of_int(Hashtbl.hash(e.e_ctx)),
    ],
  );

/* a cell's body with its first token's label changed: an edit to splice */
let edited = (e: Web.ScratchCell.t): Web.ScratchCell.t => {
  let body = F.zip_of_cell(e.e_body);
  let rec first = (seg: Segment.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when List.length(Tile.label(t)) == 1 =>
          Some(Tile.label(t) |> List.hd)
        | Tile(t) => List.find_map(first, t.children)
        | _ => None
        },
      seg,
    );
  switch (first(body)) {
  | Some(tok) =>
    let (body', _) =
      CorpusUtil.edit_token(
        ~at=`First,
        ~keep_id=true,
        ~needle=tok,
        ~repl=tok ++ "_e",
        body,
      );
    {
      ...e,
      e_body: F.cell_of_seg(body'),
    };
  | None => e
  };
};

/* every piece of [seg], tiles first-class */
let pieces = (seg: Segment.t): list(Piece.t) => {
  let out = ref([]);
  let rec go = (seg: Segment.t) =>
    List.iter(
      (p: Piece.t) => {
        out := [p, ...out^];
        switch (p) {
        | Tile(t) => List.iter(go, t.children)
        | Projector(pr) => go([pr.syntax])
        | _ => ()
        };
      },
      seg,
    );
  go(seg);
  List.rev(out^);
};

let ops: list(Web.OutlineEdit.def_op) = [
  Delete,
  MoveUp,
  MoveDown,
  NewBelow,
  NewTypeBelow,
  NewModuleBelow,
  NewTestBelow,
  NewTestAbove,
  Duplicate,
];

/* [deep]: every tile and piece too, not just the rows; [edits]: the
   restructure ops on every row */
let check_program =
    (
      t: tally,
      ~deep: bool,
      ~edits: bool,
      ~sample=1,
      ~name: string,
      ~root: Sort.t,
      seg: Segment.t,
    ) => {
  t.programs = t.programs + 1;
  let term = MakeTerm.Incr.term_of_root(~root, seg);
  let info_map = DefStatics.calc(~settings=CoreSettings.on, term).merged;
  let before = Id.Map.of_list(List.map(id => (id, ()), ids(seg)));
  let mod_root = root == Sort.Mod;
  let rows =
    List.filteri(
      (i, _) => i mod sample == 0,
      List.filter_map((n: O.node) => n.o_id, O.flatten(O.of_term(term))),
    );
  let all = pieces(seg);
  let tiles =
    List.filter_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) => Some(t.id)
        | _ => None
        },
      all,
    );
  let ids_to_check = deep ? List.sort_uniq(compare, rows @ tiles) : rows;
  /* where ItemSpans divides differently from the rules it replaced (its
     test names them): a program with an unfinished `…in` tile, which runs
     on; one whose module member is a let chain, one item; and a piece in
     a member's body run, a level of its own as an expression
     definition's body is */
  let program_why =
    if (List.exists(
          (p: Piece.t) =>
            switch (p) {
            | Tile(tl) => Tile.ends_with_in(tl) && !Segment.is_in_tile(p)
            | _ => false
            },
          all,
        )) {
      Some({js|an unfinished `…in` runs on|js});
    } else if (ItemSpans.fold(
                 (found, path) =>
                   found
                   || {
                     let (lv, k) = Util.ListUtil.last(path);
                     ItemSpans.chain(lv, k) != None;
                   },
                 false,
                 ItemSpans.top(~root, seg),
               )) {
      Some("a member that is a let chain is one item");
    } else {
      None;
    };
  let why = (id: option(Id.t), ()) =>
    switch (program_why, Option.bind(id, id => F.find_path(~root, id, seg))) {
    | (Some(_) as why, _) => why
    | (None, Some(path))
        when
          List.exists(
            (st: F.step) =>
              switch (st.from, st.lv.owner) {
              | (Sub, Block(_)) => true
              | _ => false
              },
            path,
          ) =>
      Some("a member's body run is a level")
    | _ => None
    };
  let same = (~at=?, ~id=?, ~design=?, what, a, b, key) =>
    same(t, ~name, ~why=why(id), ~design?, ~at?, what, a, b, key);
  let row_ids = O.row_ids(term);
  same(
    "refusals",
    () => F0.refusals(~want=id => Id.Map.mem(id, row_ids), seg),
    () => F.refusals(~root, ~want=id => Id.Map.mem(id, row_ids), seg),
    m =>
      Id.Map.bindings(m)
      |> List.map(((id, r)) =>
           Id.show(id)
           ++ (
             switch ((r: F.refusal)) {
             | Unfinished => "u"
             | Projected => "p"
             | Directive => "d"
             }
           )
         )
      |> String.concat(","),
  );
  if (deep) {
    same(
      "refusals of every piece",
      () => F0.refusals(~want=_ => true, seg),
      () => F.refusals(~root, ~want=_ => true, seg),
      m =>
        Id.Map.bindings(m)
        |> List.map(((id, _)) => Id.show(id))
        |> String.concat(","),
    );
    List.iter(
      (p: Piece.t) => {
        let id = Piece.id(p);
        same(
          ~id,
          "line_indent",
          () => F0.line_indent(id, seg),
          () => F.line_indent(~root, id, seg),
          opt(string_of_int),
        );
      },
      all,
    );
  };
  List.iter(
    id => {
      let sid = Id.show(id);
      same(
        ~at=sid,
        ~id,
        "headless_content_deep",
        () => F0.headless_content_deep(id, seg),
        () => F.headless_content_deep(~root, id, seg),
        opt(((c, sym)) => sym ++ " " ++ seg_key(F.core_ws(c))),
      );
      same(
        ~at=sid,
        ~id,
        "test_run_deep",
        () => F0.test_run_deep(id, seg),
        () => F.test_run_deep(~root, id, seg),
        opt(((run, ms, n)) =>
          Printf.sprintf(
            "%s %s %d",
            seg_key(F.core_ws(run)),
            String.concat(",", List.map(Id.show, ms)),
            n,
          )
        ),
      );
      same(
        ~at=sid,
        ~id,
        "find_def",
        () => F0.find_def(id, seg),
        () => F.find_def(~root, id, seg),
        opt(seg_key),
      );
      same(
        ~at=sid,
        ~id,
        "find_pat",
        () => F0.find_pat(id, seg),
        () => F.find_pat(~root, id, seg),
        opt(seg_key),
      );
      same(
        ~at=sid,
        ~id,
        "tile_ctx",
        () => F0.tile_ctx(id, seg),
        () => F.tile_ctx(~root, id, seg),
        opt(((level, i, tl: Tile.t)) =>
          Printf.sprintf("%s %d %s", seg_key(level), i, Id.show(tl.id))
        ),
      );
      same(
        ~at=sid,
        ~id,
        "test_before_semi",
        () => F0.test_before_semi(id, seg),
        () => F.test_before_semi(~root, id, seg),
        opt(Id.show),
      );
      same(
        ~at=sid,
        ~id,
        "line_indent",
        () => F0.line_indent(id, seg),
        () => F.line_indent(~root, id, seg),
        opt(string_of_int),
      );
      let cells = [
        (
          "mk_entry",
          F0.mk_entry(~info_map, id),
          F.mk_entry(~root, ~info_map, id),
        ),
        (
          "mk_members_entry",
          F0.mk_members_entry(~info_map, id),
          F.mk_members_entry(~root, ~info_map, id),
        ),
        (
          "mk_run_entry",
          F0.mk_run_entry(~info_map, id),
          F.mk_run_entry(~root, ~info_map, id),
        ),
      ];
      List.iter(
        ((what, old_mk, new_mk)) => {
          same(
            ~at=sid,
            ~id,
            what,
            () => old_mk(seg),
            () => new_mk(seg),
            opt(cell_key),
          );
          /* splices and contents, of the old side's cell */
          switch (old_mk(seg)) {
          | Some(e) =>
            let e' = edited(e);
            same(
              ~at=sid,
              ~id,
              what ++ " splice",
              () => F0.splice_entry(e, seg),
              () => F.splice_entry(~root, e, seg),
              s => seg_key(s) ++ (s === seg ? " (same)" : ""),
            );
            same(
              ~at=sid,
              ~id,
              what ++ " splice edited",
              () => F0.splice_entry(e', seg),
              () => F.splice_entry(~root, e', seg),
              seg_key,
            );
            same(
              ~at=sid,
              ~id,
              what ++ " cell_content",
              () => F0.cell_content(e, seg),
              () => F.cell_content(~root, e, seg),
              opt(c => seg_key(F.core_ws(c))),
            );
          | None
          | exception _ => ()
          };
        },
        cells,
      );
    },
    ids_to_check,
  );
  if (edits) {
    let ctx: IE.ctx = {
      mod_root,
      term,
      info_map: lazy(info_map),
      is_open: id => O.expanded(~folds=[], id, term),
    };
    let made = r =>
      switch (r) {
      | Ok((s, row)) =>
        made_key(~before, s) ++ " -> " ++ row_key(~before, row)
      | Error(why) => "refused: " ++ why
      };
    let made_opt = r =>
      switch (r) {
      | Some((s, row)) =>
        made_key(~before, s) ++ " -> " ++ row_key(~before, row)
      | None => "refused"
      };
    let trivia = ((sa, ra), (sb, rb)) =>
      solid_key(~before, sa) == solid_key(~before, sb)
      && row_key(~before, ra) == row_key(~before, rb)
        ? Some(trivia_rule) : None;
    let stmt_row = id =>
      switch (O.kind_of(id, term)) {
      | Some(KTest | KStmt) => true
      | _ => false
      };
    let design_opt = (op: Web.OutlineEdit.def_op, a, b) =>
      switch (op, a, b) {
      | (_, Some(a), Some(b)) => trivia(a, b)
      | (NewTestBelow | NewTestAbove, Some(_), None) => Some(stmt_rule)
      | _ => None
      };
    /* a comment set apart lies between row [id]'s item and the item
       above it, or below */
    let apart = (~up, id) =>
      switch (IE.locate(~mod_root, id, seg)) {
      | Some((b, j, _)) =>
        let k = up ? j : j + 1;
        k > 0 && k < Array.length(b.sl) && !IE.under_item(b, k);
      | None => false
      };
    let design = (~op=?, id, a, b) =>
      switch (op, a, b) {
      | (_, Ok(a), Ok(b)) when trivia(a, b) != None => trivia(a, b)
      | (Some(Web.OutlineEdit.Indent), Ok(_), Error(_)) when stmt_row(id) =>
        Some(stmt_rule)
      | (Some(Web.OutlineEdit.Indent | MoveUp), _, _)
          when apart(~up=true, id) =>
        Some(apart_rule)
      | (Some(MoveDown), _, _) when apart(~up=false, id) =>
        Some(apart_rule)
      | _ => None
      };
    List.iter(
      id => {
        let sid = Id.show(id);
        List.iter(
          (op: Web.OutlineEdit.def_op) => {
            let what = Web.OutlineEdit.show_def_op(op) ++ " " ++ sid;
            same(
              ~at=what,
              ~id,
              ~design=design_opt(op),
              "apply " ++ Web.OutlineEdit.show_def_op(op),
              () => IE0.apply(~mod_root, op, id, seg),
              () => IE.apply(~mod_root, op, id, seg),
              made_opt,
            );
            switch (op) {
            | MoveUp
            | MoveDown
            | Indent
            | Outdent =>
              same(
                ~at=what,
                ~id,
                ~design=design(~op, id),
                "edit " ++ Web.OutlineEdit.show_def_op(op),
                () => IE0.edit(ctx, Op(op, id), seg),
                () => IE.edit(ctx, Op(op, id), seg),
                made,
              )
            | _ => ()
            };
          },
          ops @ [Indent, Outdent],
        );
        List.iter(
          ((label, e: IE.t)) =>
            same(
              ~at=sid,
              ~id,
              label,
              () => IE0.edit(ctx, e, seg),
              () => IE.edit(ctx, e, seg),
              made,
            ),
          [
            ("insert inside", InsertInside(id, "zz")),
            ("insert first", InsertFirst(id, "type Zz")),
            ("insert below", Insert(id, "module Zz")),
            ("blank below", Blank(Below, id, KConst)),
            ("blank first", Blank(First, id, KModule)),
            ("blank last", Blank(Last, id, KType)),
            ("rename", Rename(id, "renamed")),
            ("rekind", Rename(id, "module Renamed")),
          ],
        );
        List.iter(
          up =>
            same(
              ~at=sid,
              ~id,
              "spot",
              () => IE0.spot(~mod_root, ~up, id, seg),
              () => IE.spot(~mod_root, ~up, id, seg),
              opt((s: IE.spot) =>
                Printf.sprintf(
                  "%b %s %s %s %s %b",
                  s.s_member,
                  seg_key(s.s_item),
                  seg_key(s.s_trail),
                  opt(Id.show, s.s_module),
                  opt(Id.show, s.s_value),
                  s.s_edge,
                )
              ),
            ),
          [true, false],
        );
      },
      rows,
    );
  };
};

let parse = (~root, src) =>
  switch (CorpusUtil.parse(~root, src)) {
  | Some(seg) => Some(seg)
  | None => CorpusUtil.typed_seg(~root, src)
  };

let report = (what: string, t: tally) => {
  Printf.printf(
    "CellsParity %s: %d programs, %d checks, %d diffs\n",
    what,
    t.programs,
    t.checks,
    List.length(t.diffs),
  );
  Hashtbl.fold((k, n, acc) => [(k, n), ...acc], t.counts, [])
  |> List.sort(compare)
  |> List.iter(((k, n)) => Printf.printf("  %s: %d\n", k, n));
  List.iteri(
    (i, (name, why, d)) =>
      if (i < 400) {
        Printf.printf(
          "DIFF %s%s: %s\n",
          name,
          Option.fold(~none="", ~some=w => " (by design: " ++ w ++ ")", why),
          d,
        );
      },
    List.rev(t.diffs),
  );
  /* by design, per rule */
  let by = Hashtbl.create(4);
  List.iter(
    ((_, why, _)) =>
      Option.iter(
        w =>
          Hashtbl.replace(
            by,
            w,
            1 + Option.value(Hashtbl.find_opt(by, w), ~default=0),
          ),
        why,
      ),
    t.diffs,
  );
  Hashtbl.iter(
    (w, n) => Printf.printf("  by design (%s): %d diffs\n", w, n),
    by,
  );
};

/* the diffs no rule explains */
let unexplained = (t: tally): list(string) =>
  List.filter_map(
    ((name, why, d)) => why == None ? Some(name ++ ": " ++ d) : None,
    t.diffs,
  );

/* the programs that differ, but by [except]'s rules */
let differing = (~except=[], t: tally): list(string) =>
  List.sort_uniq(
    compare,
    List.filter_map(
      ((name, why, _)) =>
        switch (why) {
        | Some(w) when List.mem(w, except) => None
        | _ => Some(name)
        },
      t.diffs,
    ),
  );

let run = (~deep, ~edits, ~sample=?, programs) => {
  let t = tally();
  List.iter(
    ((name, root, src)) =>
      switch (parse(~root, src)) {
      | Some(seg) =>
        check_program(t, ~deep, ~edits, ~sample?, ~name, ~root, seg)
      | None => t.diffs = [(name, None, "unparseable"), ...t.diffs]
      },
    programs,
  );
  t;
};

/* shapes at the edges of the span rules */
let extra_shapes = [
  (
    "comment above a fun body's first let",
    "let f = fun x ->\n  # doc #\n  let c = x in\n  c\nin\nf(1)",
  ),
  (
    "a member's flat body with a comment",
    "module M = {\n  let f = fun x ->\n    # doc #\n    let c = x in\n    c;\n  let g = 2\n} in\nM.g",
  ),
  ("a ⇒ in a projector", "let a = 1 in\n^^fold(\"hidden\")"),
  ("a tuple tail", "let a = 1 in\n(a, a)"),
  ("a filter", "let a = 1 in\nhide a in\na + 1"),
  ("same-line statements", "let a = 1 in a; a; a"),
  ("a statement after a comment", "let a = 1 in\n# c #\ntest a == 1 end;\na"),
  (
    "indented members",
    "module M = {\n    let a = 1;\n      let b = 2\n} in\nM.a",
  ),
  (
    "nested modules",
    "module A = {\n  module B = {\n    let x = 1;\n    test x == 1 end\n  };\n  let y = B.x\n} in\nA.y",
  ),
  (
    "a let chain const",
    "let x =\n  let y = 1 in\n  let z = 2 in\n  y + z\nin\nx",
  ),
  ("a wrapped fun body", "let f = fun x -> (let y = x in y) in f(1)"),
  ("a wrapped const", "let x = (let y = 1 in y) in x"),
];

let shapes = () => {
  let t =
    run(
      ~deep=true,
      ~edits=true,
      List.map(((n, s)) => (n, Sort.Exp, s), extra_shapes)
      @ List.map(((n, s)) => (n, Sort.Exp, s), Test_ItemSpans.exp_shapes)
      @ List.map(((n, s)) => (n, Sort.Exp, s), CorpusUtil.edge_programs)
      @ List.map(
          ((n, s, _)) => (n, Sort.Exp, s),
          Test_HeadlessRows.shapes,
        )
      @ List.map(
          ((n, s)) => (n, Sort.Mod, s),
          Test_ItemSpans.letin_members,
        ),
    );
  report("shapes", t);
  check(list(string), "unexplained diffs", [], unexplained(t));
  /* by design: a module member that is a let chain is one item, its items
     a level of their own; and a member's body run is a level, as an
     expression definition's body is: its rows act there (never on the
     whole member), and a comment line directly above its first item goes
     with it */
  check(
    list(string),
    "only the shapes ItemSpans divides differently",
    [
      "a let chain among members",
      "a let chain holding a module",
      "a member's flat body with a comment",
      "a module of a let chain",
      "a statement opening with a let-in",
      {js|a ⇒ in parentheses|js},
    ],
    differing(~except=item_rules, t),
  );
};

/* an unfinished definition with items after it, which it runs on into */
let unfinished_before_others = "let a = 1 in\nlet b = 2\nlet c = 3 in\nc";

let incomplete = () => {
  let t =
    run(
      ~deep=true,
      ~edits=true,
      List.map(
        ((n, s)) => (n, Sort.Exp, s),
        CorpusUtil.incomplete_programs
        @ [("unfinished before others", unfinished_before_others)],
      ),
    );
  report("incomplete", t);
  check(list(string), "unexplained diffs", [], unexplained(t));
  /* by design: an `…in` tile cuts only with its `in`, so an unfinished
     `let x =` runs on into what follows, as the parser reads it; ops that
     would take what it runs into refuse */
  check(
    list(string),
    "only unfinished definitions",
    [
      "bare keyword",
      "def without body",
      "unfinished before others",
      "unfinished last def",
    ],
    differing(~except=item_rules, t),
  );
};

/* the frozen side walks the whole program for each lookup, so the corpus
   runs take minutes: only with CELLS_PARITY=1 */
let full = Sys.getenv_opt("CELLS_PARITY") == Some("1");
let skipped = what =>
  Printf.printf("CellsParity %s: skipped (CELLS_PARITY=1 runs it)\n", what);

/* every slide's rows and the stdlib's */
let corpus = () =>
  if (!full) {
    skipped("corpus");
  } else {
    let t =
      run(
        ~deep=false,
        ~edits=true,
        CorpusUtil.slide_corpus()
        @ List.map(
            ((p, s)) => (p, Sort.Mod, s),
            Test_ItemSpans.stdlib_corpus(),
          ),
      );
    report("corpus", t);
    check(
      list(string),
      "programs that differ",
      [],
      differing(~except=item_rules, t),
    );
  };

/* every fourth row of mega-1k and mega-mod-1k */
let megas = () =>
  if (!full) {
    skipped("megas");
  } else {
    let t =
      run(
        ~deep=false,
        ~edits=true,
        ~sample=4,
        List.map(
          ((f, root)) => (f, root, Option.get(CorpusUtil.mega_src(f))),
          [("mega-1k.hz", Sort.Exp), ("mega-mod-1k.hz", Sort.Mod)],
        ),
      );
    report("megas", t);
    check(list(string), "unexplained diffs", [], unexplained(t));
  };

/* every twentieth row of mega-4k */
let mega_4k = () =>
  if (!full) {
    skipped("mega-4k");
  } else {
    let t =
      run(
        ~deep=false,
        ~edits=true,
        ~sample=20,
        [
          (
            "mega-4k.hz",
            Sort.Exp,
            Option.get(CorpusUtil.mega_src("mega-4k.hz")),
          ),
        ],
      );
    report("mega-4k", t);
    check(list(string), "unexplained diffs", [], unexplained(t));
  };

/* what reads differently by design, pinned as it reads now */
let by_design = () => {
  let op = (~root=Sort.Exp, o, src, pick) => {
    let seg = CorpusUtil.seg_of(~root, src);
    let term = MakeTerm.Incr.term_of_root(~root, seg);
    Option.map(
      ((s, _)) => text_of(s),
      IE.apply(~mod_root=root == Sort.Mod, o, pick(term, seg), seg),
    );
  };
  let row = (label, term, _) => CorpusUtil.row_by_label(term, label);
  /* the ⇒ row under [label] */
  let result_of = (label, term, seg) =>
    switch (O.node_of(row(label, term, seg), term)) {
    | Some(n) =>
      List.find_map(
        (k: O.node) => k.o_kind == KTrail ? k.o_id : None,
        n.o_children,
      )
      |> Option.get
    | None => fail("no row " ++ label)
    };
  /* a member's body is a level, as an expression definition's is: its
     result is no item to delete (it took the member) */
  check(
    option(string),
    "a member body's result stays",
    None,
    op(
      Delete,
      "module M = {\n  let f = fun x ->\n    let a = x in\n    (a, a);\n  let g = 2\n} in\nM.f(1)",
      result_of("f"),
    ),
  );
  /* and a comment line directly above its first item goes with it */
  check(
    option(string),
    "a member body's doc comment goes with its item",
    Some("module M = {\n  let f = fun x ->\n    c;\n  let g = 2\n} in\nM.g"),
    op(
      Delete,
      "module M = {\n  let f = fun x ->\n    # doc #\n    let c = x in\n    c;\n  let g = 2\n} in\nM.g",
      row("c"),
    ),
  );
  /* an unfinished `let x =` runs on into what follows: alone, it deletes
     as one item, with the gap above it */
  let unfinished = (_, seg) =>
    List.hd(Segment.incomplete_tiles_deep(seg)).id;
  check(
    option(string),
    "a lone unfinished definition deletes",
    Some("let a = 1 in"),
    op(Delete, "let a = 1 in\nlet b = 2", unfinished),
  );
  /* but nothing takes it whole once it runs on into items after it: they
     would go with it */
  let first_in = (_, seg) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(tl) when Segment.is_in_tile(p) => Some(tl.id)
        | _ => None
        },
      seg,
    )
    |> Option.get;
  let swallowed = (_, seg) =>
    List.filter_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(tl) when Segment.is_in_tile(p) => Some(tl.id)
        | _ => None
        },
      seg,
    )
    |> List.rev
    |> List.hd;
  List.iter(
    ((what, o, pick)) =>
      check(
        option(string),
        what ++ " refuses",
        None,
        op(o, unfinished_before_others, pick),
      ),
    [
      ("deleting it", Web.OutlineEdit.Delete, unfinished),
      ("duplicating it", Duplicate, unfinished),
      ("moving it up", MoveUp, unfinished),
      ("moving it down", MoveDown, unfinished),
      ("moving the one above down past it", MoveDown, first_in),
      ("deleting a definition it runs into", Delete, swallowed),
    ],
  );
  /* Alt-arrows too */
  let seg = CorpusUtil.seg_of(unfinished_before_others);
  let term = MakeTerm.Incr.term_of_root(~root=Exp, seg);
  let ctx: IE.ctx = {
    mod_root: false,
    term,
    info_map: lazy(Statics.Map.empty),
    is_open: _ => true,
  };
  List.iter(
    o =>
      check(
        bool,
        "Alt-arrow refuses",
        true,
        Result.is_error(IE.edit(ctx, Op(o, unfinished((), seg)), seg)),
      ),
    [Web.OutlineEdit.MoveUp, MoveDown],
  );
  /* a member that is a let chain is one item */
  check(
    option(string),
    "a member moves above a whole let chain",
    Some(
      "module M = {\n  let x = 1;\n  let y = 2;\n  let a = x in\n  a + 1\n};\nM.y",
    ),
    op(
      ~root=Mod,
      MoveUp,
      "module M = {\n  let x = 1;\n  let a = x in\n  a + 1;\n  let y = 2\n};\nM.y",
      row("y"),
    ),
  );
};

let tests = (
  "CellsParity",
  [
    test_case("shapes", `Slow, shapes),
    test_case("incomplete programs", `Quick, incomplete),
    test_case("differences by design", `Quick, by_design),
    test_case("corpus", `Slow, corpus),
    test_case("mega-1k and mega-mod-1k", `Slow, megas),
    test_case("mega-4k", `Slow, mega_4k),
  ],
);
