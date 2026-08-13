/**
 * Tests for SubEditor.Target resolution, separator trimming, and
 * update-time confinement. Locators are structural (host tile id +
 * path) so they survive inner term-id churn.
 */
open Alcotest;
open Haz3lcore;
open Language;
open Action;

module SubEditor = Web.SubEditor;
module Target = SubEditor.Target;

let seg_string = (seg: Segment.t): string =>
  Segment.to_string(
    seg,
    ~projector_to_segment=_ => [],
    ~refractor_seg_to_seg=(r, s) => (r, s),
  );

let check_seg = (msg, expected: string, actual: option(Segment.t)) =>
  switch (actual) {
  | None => fail(msg ++ ": resolve returned None")
  | Some(seg) => check(string, msg, expected, seg_string(seg))
  };

let mold = (label: Label.t): Mold.t =>
  Mold.mk_op(
    Sort.Exp,
    List.init(max(0, List.length(label) - 1), _ => Sort.Exp),
  );

let mk_tile =
    (~id=Id.mk(), label: Label.t, children: list(Segment.t)): Base.tile => {
  id,
  label,
  mold: mold(label),
  shards: List.init(List.length(label), i => i),
  children,
};

let tile = (~id=?, label, children): Piece.t =>
  Piece.Tile(mk_tile(~id?, label, children));

let atom = (~id=?, name: string): Piece.t => tile(~id?, [name], []);

let space = (): Piece.t =>
  Piece.Secondary(Haz3lcore.Secondary.mk_space(Id.mk()));

let linebreak = (): Piece.t =>
  Piece.Secondary(Haz3lcore.Secondary.mk_newline(Id.mk()));

let splice = (~id, content: Segment.t): Piece.t =>
  Piece.Splice({
    id,
    content,
  });

let projector = (~id=Id.mk(), syntax: Segment.t): Piece.t =>
  Piece.Projector(
    ProjectorCore.mk(~id, ProjectorCore.Kind.Fold, syntax, ""),
  );

/* Synthetic host shaped like `case x | A => 1 | B => 2 end`.
 * Rule tiles only contain the pattern child; the body is a sibling
 * (mirrors Form.Rule). A parenthesized nested `| =>` must not affect
 * direct selectors. */
let mk_host = () => {
  let host_id = Id.mk();
  let scrut_id = Id.mk();
  let nested_rule = tile(["|", "=>"], [[atom("nested")]]);
  let child = [
    atom(~id=scrut_id, "x"),
    space(),
    tile(["(", ")"], [[nested_rule, atom("0")]]),
    space(),
    tile(["|", "=>"], [[atom("A")]]),
    atom("1"),
    space(),
    tile(["|", "=>"], [[atom("B")]]),
    atom("2"),
  ];
  let host = mk_tile(~id=host_id, ["case", "end"], [child]);
  (host_id, scrut_id, [Piece.Tile(host)]);
};

let rule = ["|", "=>"];

let scrut_target = (anchor: Id.t): Target.t =>
  Target.child(~anchor, 0)
  |> Target.until(Target.Before(Target.nthTile(rule, 0)));

let pattern_target = (anchor: Id.t, i: int): Target.t =>
  Target.child(~anchor, 0)
  |> Target.descend(Target.nthTile(rule, i), ~child=0);

let between_rules_target = (anchor: Id.t): Target.t =>
  Target.child(~anchor, 0)
  |> Target.from(Target.After(Target.nthTile(rule, 0)))
  |> Target.until(Target.Before(Target.nthTile(rule, 1)));

/* --- Resolve --- */

let test_whole_child = () => {
  let (host_id, _, root) = mk_host();
  let target = Target.child(~anchor=host_id, 0);
  check_seg(
    "whole child",
    "x (|nested=>0) |A=>1 |B=>2",
    Target.resolve(target, root),
  );
};

let test_prefix_until_first_rule = () => {
  let (host_id, _, root) = mk_host();
  check_seg(
    "scrut prefix",
    "x (|nested=>0) ",
    Target.resolve(scrut_target(host_id), root),
  );
};

let test_nth_rule_pattern = () => {
  let (host_id, _, root) = mk_host();
  check_seg(
    "first rule pattern",
    "A",
    Target.resolve(pattern_target(host_id, 0), root),
  );
  check_seg(
    "second rule pattern",
    "B",
    Target.resolve(pattern_target(host_id, 1), root),
  );
};

let test_between_bounds = () => {
  let (host_id, _, root) = mk_host();
  check_seg(
    "between rules",
    "1 ",
    Target.resolve(between_rules_target(host_id), root),
  );
};

let test_nested_label_ignored = () => {
  /* Direct NthTile(rule, 0) must pick the outer `|A=>`, not the nested
   * `|nested=>` inside parentheses. */
  let (host_id, _, root) = mk_host();
  check_seg(
    "direct first rule pattern",
    "A",
    Target.resolve(pattern_target(host_id, 0), root),
  );
};

/* --- Splice wrappers --- */

let test_of_splice_whole_content = () => {
  let id = Id.mk();
  let root = [atom("f"), splice(~id, [atom("1"), space(), atom("2")])];
  check_seg(
    "splice content",
    "1 2",
    Target.resolve(Target.of_splice(id), root),
  );
};

let test_of_splice_nested = () => {
  /* Splices reachable through tile children, projector syntax, and
   * inside another splice's content all resolve. */
  let in_child = Id.mk();
  let in_projector = Id.mk();
  let inner = Id.mk();
  let outer = Id.mk();
  let root = [
    tile(["(", ")"], [[splice(~id=in_child, [atom("a")])]]),
    projector([splice(~id=in_projector, [atom("b")])]),
    splice(~id=outer, [atom("c"), splice(~id=inner, [atom("d")])]),
  ];
  check_seg(
    "splice in tile child",
    "a",
    Target.resolve(Target.of_splice(in_child), root),
  );
  check_seg(
    "splice in projector syntax",
    "b",
    Target.resolve(Target.of_splice(in_projector), root),
  );
  check_seg(
    "splice in splice content",
    "d",
    Target.resolve(Target.of_splice(inner), root),
  );
  check_seg(
    "outer splice keeps nested wrapper",
    "cd",
    Target.resolve(Target.of_splice(outer), root),
  );
};

let test_of_splice_is_whole_content = () => {
  let id = Id.mk();
  check(
    bool,
    "of_splice is a whole-content target",
    true,
    Target.whole_content_id(Target.of_splice(id)) == Some(id),
  );
  check(
    bool,
    "narrowed targets are not whole-content",
    true,
    Target.whole_content_id(
      Target.of_splice(id)
      |> Target.until(Target.Before(Target.nthTile(rule, 0))),
    )
    == None,
  );
  check(
    bool,
    "child targets are not whole-content",
    true,
    Target.whole_content_id(Target.child(~anchor=id, 0)) == None,
  );
};

let test_tile_anchor_needs_a_step = () => {
  /* A bare tile anchor has no segment of its own — only splices do. */
  let (host_id, _, root) = mk_host();
  check(
    bool,
    "of_splice against a tile anchor",
    true,
    Target.resolve(Target.of_splice(host_id), root) == None,
  );
};

let test_missing_returns_none = () => {
  let (host_id, _, root) = mk_host();
  check(
    bool,
    "bad anchor",
    true,
    Target.resolve(Target.child(~anchor=Id.mk(), 0), root) == None,
  );
  check(
    bool,
    "bad child index",
    true,
    Target.resolve(Target.child(~anchor=host_id, 9), root) == None,
  );
  check(
    bool,
    "missing selector",
    true,
    Target.resolve(
      Target.child(~anchor=host_id, 0)
      |> Target.until(Target.Before(Target.nthTile(rule, 9))),
      root,
    )
    == None,
  );
  check(
    bool,
    "reversed bounds",
    true,
    Target.resolve(
      Target.child(~anchor=host_id, 0)
      |> Target.from(Target.Before(Target.nthTile(rule, 1)))
      |> Target.until(Target.Before(Target.nthTile(rule, 0))),
      root,
    )
    == None,
  );
};

let test_before_or_end_falls_back = () => {
  /* BeforeOrEnd bounds a scrut-style prefix even when no rule tile
   * exists yet (induction with zero cases): it falls back to the
   * segment end instead of failing to resolve. */
  let scrut_target' = (anchor: Id.t): Target.t =>
    Target.child(~anchor, 0)
    |> Target.until(Target.BeforeOrEnd(Target.nthTile(rule, 0)));
  /* With rules present: identical to Before. */
  let (host_id, _, root) = mk_host();
  check_seg(
    "with rules, same as Before",
    "x (|nested=>0) ",
    Target.resolve(scrut_target'(host_id), root),
  );
  /* With no rules: spans the whole child. */
  let host_id = Id.mk();
  let root = [
    Piece.Tile(
      mk_tile(
        ~id=host_id,
        ["induction", "end"],
        [[space(), atom("x"), space()]],
      ),
    ),
  ];
  check_seg(
    "no rules, whole child",
    " x ",
    Target.resolve(scrut_target'(host_id), root),
  );
  check(
    bool,
    "plain Before still fails with no rules",
    true,
    Target.resolve(scrut_target(host_id), root) == None,
  );
};

let test_stable_under_inner_id_churn = () => {
  let (host_id, scrut_id, root) = mk_host();
  let target = scrut_target(host_id);
  let before = Target.resolve(target, root);
  /* Replace the scrutinee tile's id (and content) while keeping the host. */
  let root' =
    switch (root) {
    | [Piece.Tile(host)] =>
      let child =
        switch (host.children) {
        | [seg] =>
          List.map(
            (p: Piece.t) =>
              switch (p) {
              | Piece.Tile(t) when t.id == scrut_id => atom(~id=Id.mk(), "yz")
              | p => p
              },
            seg,
          )
        | _ => fail("expected one child")
        };
      [
        Piece.Tile({
          ...host,
          children: [child],
        }),
      ];
    | _ => fail("expected host tile")
    };
  check_seg("before churn", "x (|nested=>0) ", before);
  check_seg(
    "after churn, same target",
    "yz (|nested=>0) ",
    Target.resolve(target, root'),
  );
};

/* --- Trim --- */

let test_trim_one_space_each_end = () => {
  let seg = [space(), atom("x"), space(), atom("y"), space()];
  let trimmed = SubEditor.trim_separators(seg);
  check(string, "trim spaces", "x y", seg_string(trimmed));
};

let test_trim_linebreak_and_indent = () => {
  /* Leading: linebreak + indent. Trailing: linebreak (no spaces between
   * content and the trailing linebreak — those would be kept as user
   * whitespace). */
  let seg = [linebreak(), space(), space(), atom("x"), linebreak()];
  let trimmed = SubEditor.trim_separators(seg);
  check(string, "trim linebreak+indent", "x", seg_string(trimmed));
};

let test_trim_keeps_user_mid_whitespace = () => {
  /* Two spaces after x: first is host separator (stripped), second is
   * the user's mid-typing space and must remain. */
  let seg = [space(), atom("x"), space(), space()];
  let trimmed = SubEditor.trim_separators(seg);
  check(string, "keep mid-typing space", "x ", seg_string(trimmed));
};

/* --- Confinement via real editor --- */

let caret_char = "¦";

let string_to_ltr_actions = (s: string): list(Action.t) =>
  s |> Token.to_list |> List.map(c => Action.Insert(c));

let mv_l = (n: int): list(Action.t) =>
  List.init(n, _ => Action.Move(Local(Left, ByChar)));

let perform_zip = (zip: Zipper.t, actions: list(Action.t)): Zipper.t => {
  let go = (a: Action.t, z: Zipper.t) =>
    Perform.go(
      ~settings=CoreSettings.off,
      ~statics=CachedStatics.empty,
      ~syntax=CachedSyntax.init(z),
      ~root=Exp,
      a,
      {
        zipper: z,
        col_target: None,
      },
    );
  List.fold_left(
    (z, a) =>
      switch (go(a, z)) {
      | Ok(z) => z
      | Error(err) => fail("Failed on action: " ++ Action.Failure.show(err))
      },
    zip,
    actions,
  );
};

let zipper_with_caret = (init: string): Zipper.t => {
  let rec split = (before, rest) =>
    switch (rest) {
    | [] => fail("No caret in: " ++ init)
    | [hd, ...tl] =>
      hd == caret_char
        ? (List.rev(before), tl) : split([hd, ...before], tl)
    };
  let (before, after) = split([], Token.to_list(init));
  let s = Token.of_list(before @ after);
  perform_zip(
    Zipper.init(),
    string_to_ltr_actions(s) @ mv_l(List.length(after)),
  );
};

let find_tile_by_label = (label: Label.t, seg: Segment.t): option(Base.tile) => {
  let rec go = (seg: Segment.t): option(Base.tile) =>
    List.fold_left(
      (found, p: Piece.t) =>
        switch (found) {
        | Some(_) => found
        | None =>
          switch (p) {
          | Piece.Tile(t) =>
            t.label == label
              ? Some(t)
              : List.fold_left(
                  (found, child) => found == None ? go(child) : found,
                  None,
                  t.children,
                )
          | _ => None
          }
        },
      None,
      seg,
    );
  go(seg);
};

let case_editor = (with_caret: string): (Editor.Model.t, Id.t) => {
  let z = zipper_with_caret(with_caret);
  let ed = Editor.Model.mk(z, ~root=Exp);
  switch (
    find_tile_by_label(["case", "end"], CachedSyntax.segment(ed.syntax))
  ) {
  | None => fail("no case tile")
  | Some(t) => (ed, t.id)
  };
};

let test_mk_scrut_from_case = () => {
  let (ed, case_id) = case_editor("case x | A => 1 end¦");
  let target = scrut_target(case_id);
  switch (SubEditor.mk(ed, ~target)) {
  | None => fail("mk returned None")
  | Some(sub) =>
    check(string, "mk scrut segment", "x", seg_string(sub.splice.segment))
  };
};

let test_confine_rejects_edge_delete = () => {
  let (ed, case_id) = case_editor("case ¦x | A => 1 end");
  let target = scrut_target(case_id);
  check(
    bool,
    "Destruct(Left) at left edge rejected",
    true,
    SubEditor.confine_pre(~target, ~action=Destruct(Left), ed) == None,
  );
  let (ed_r, case_id_r) = case_editor("case x¦ | A => 1 end");
  let target_r = scrut_target(case_id_r);
  check(
    bool,
    "Destruct(Right) at right edge rejected",
    true,
    SubEditor.confine_pre(~target=target_r, ~action=Destruct(Right), ed_r)
    == None,
  );
};

let test_confine_rejects_edit_outside = () => {
  /* Caret on the rule separator — outside the scrut splice. */
  let (ed, case_id) = case_editor("case x ¦| A => 1 end");
  let target = scrut_target(case_id);
  check(
    bool,
    "Insert outside splice rejected",
    true,
    SubEditor.confine_pre(~target, ~action=Insert("z"), ed) == None,
  );
};

let test_confine_allows_edit_inside = () => {
  let (ed, case_id) = case_editor("case ¦x | A => 1 end");
  let target = scrut_target(case_id);
  check(
    bool,
    "Insert inside splice allowed",
    true,
    Option.is_some(SubEditor.confine_pre(~target, ~action=Insert("z"), ed)),
  );
};

let tests = (
  "SubEditor",
  [
    test_case("resolve whole child", `Quick, test_whole_child),
    test_case(
      "resolve prefix until first rule",
      `Quick,
      test_prefix_until_first_rule,
    ),
    test_case("resolve nth rule pattern", `Quick, test_nth_rule_pattern),
    test_case("resolve between bounds", `Quick, test_between_bounds),
    test_case(
      "nested matching labels ignored",
      `Quick,
      test_nested_label_ignored,
    ),
    test_case(
      "resolve whole splice content",
      `Quick,
      test_of_splice_whole_content,
    ),
    test_case("resolve nested splices", `Quick, test_of_splice_nested),
    test_case(
      "of_splice is a whole-content target",
      `Quick,
      test_of_splice_is_whole_content,
    ),
    test_case(
      "tile anchors need a child step",
      `Quick,
      test_tile_anchor_needs_a_step,
    ),
    test_case(
      "malformed targets return None",
      `Quick,
      test_missing_returns_none,
    ),
    test_case(
      "BeforeOrEnd falls back to end",
      `Quick,
      test_before_or_end_falls_back,
    ),
    test_case(
      "stable under inner id churn",
      `Quick,
      test_stable_under_inner_id_churn,
    ),
    test_case(
      "trim one space each end",
      `Quick,
      test_trim_one_space_each_end,
    ),
    test_case(
      "trim linebreak and indent",
      `Quick,
      test_trim_linebreak_and_indent,
    ),
    test_case(
      "trim keeps user mid whitespace",
      `Quick,
      test_trim_keeps_user_mid_whitespace,
    ),
    test_case("mk scrut from case", `Quick, test_mk_scrut_from_case),
    test_case(
      "confine rejects edge delete",
      `Quick,
      test_confine_rejects_edge_delete,
    ),
    test_case(
      "confine rejects edit outside",
      `Quick,
      test_confine_rejects_edit_outside,
    ),
    test_case(
      "confine allows edit inside",
      `Quick,
      test_confine_allows_edit_inside,
    ),
  ],
);
