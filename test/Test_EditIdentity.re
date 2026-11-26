open Alcotest;
open Haz3lcore;
open Poly;

let parse = CorpusUtil.seg_of;
let zipper = src => Zipper.unzip(parse(src));
let segment = Zipper.unselect_and_zip;
let text = seg => Printer.of_zipper(~holes="?", Zipper.unzip(seg));
let edit = (z, action) =>
  switch (
    Perform.go(
      ~settings=Language.CoreSettings.on,
      ~statics=CachedStatics.empty,
      ~syntax=CachedSyntax.init(z),
      ~root=Exp,
      Structural(action),
      {
        zipper: z,
        col_target: None,
      },
    )
  ) {
  | Ok(next) => next
  | Error(err) => fail(Action.Failure.show(err))
  };

let check_survivors = (before, after) => {
  let after = EditIdentity.index(after);
  Id.Map.iter(
    (id, p) => {
      switch (Id.Map.find_opt(id, after)) {
      | Some(next) when compare(p, next) == 0 =>
        check(
          bool,
          "unchanged piece shares its old object",
          true,
          phys_equal(p, next),
        )
      | _ => ()
      }
    },
    EditIdentity.index(before),
  );
};
let check_all_retained = (before, after) => {
  let old = EditIdentity.index(before);
  let next = EditIdentity.index(after);
  check(int, "piece count", Id.Map.cardinal(old), Id.Map.cardinal(next));
  Id.Map.iter(
    (id, p) =>
      check(
        bool,
        "same id and object",
        true,
        switch (Id.Map.find_opt(id, next)) {
        | Some(q) => phys_equal(p, q)
        | None => false
        },
      ),
    old,
  );
};

let tests = (
  "Structural edit identity",
  [
    test_case(
      "zipper mappers retain untouched tile objects",
      `Quick,
      () => {
        let z = zipper("let m = { let x = 1 } in m");
        check_all_retained(
          segment(z),
          segment(ZipperBase.MapPiece.go(p => [p], z)),
        );
        check_all_retained(
          segment(z),
          segment(ZipperBase.MapSegment.go(seg => seg, z)),
        );
      },
    ),
    test_case(
      "stateful projector instances are not matched by printed content",
      `Quick,
      () => {
        let before = parse("^^slider(10)");
        let fresh = parse("^^slider(10)");
        check(
          bool,
          "fixture contains a projector",
          true,
          List.exists(
            ~f=
              (p: Piece.t) =>
                switch (p) {
                | Projector(_) => true
                | _ => false
                },
            before,
          ),
        );
        check(
          bool,
          "new widget remains a new instance",
          false,
          EditIdentity.same_content(before, fresh),
        );
        check(
          bool,
          "replacement does not resurrect the old widget",
          false,
          EditIdentity.same_content(
            before,
            EditIdentity.reuse(before, fresh),
          ),
        );
      },
    ),
    test_case(
      "unrelated separator spacing survives insertion",
      `Quick,
      () => {
        let z = zipper("let m = { let a = 1 ; let b = 2 ; let c = 3 } in m");
        let next = edit(z, Insert(After("m/b"), "let d = 4"));
        check(
          bool,
          "untouched space before separator",
          true,
          Test_AgentTools.contains_sub(text(segment(next)), "let a = 1 ;"),
        );
        check_survivors(segment(z), segment(next));
      },
    ),
    test_case(
      "narrow definition edit preserves unrelated layout and pieces",
      `Quick,
      () => {
        let z =
          zipper("let a = 1 in\n\n\nlet b = a + 1 in\n# keep me #\n\n b");
        let next = edit(z, Replace("b:definition", "a + 2"));
        check(
          string,
          "only requested code changes",
          "let a = 1 in\n\n\nlet b = a + 2 in\n# keep me #\n\n b",
          text(segment(next)),
        );
        check_survivors(segment(z), segment(next));
        let b = parse("a + 1");
        let fresh = parse("a + 2");
        let reused = EditIdentity.reuse(b, fresh);
        let old_ids = EditIdentity.index(b);
        check(
          bool,
          "unchanged operand is retained",
          true,
          Id.Map.exists(
            (id, p) =>
              switch (Id.Map.find_opt(id, EditIdentity.index(reused))) {
              | Some(q) => phys_equal(p, q)
              | None => false
              },
            old_ids,
          ),
        );
      },
    ),
    test_case(
      "member update does not reformat other members or modules",
      `Quick,
      () => {
        let src = "let m = {\n  let x = 1;\n\n\n  let y = 2\n} in\n\nlet n = {\n let z = 3\n} in m.x";
        let z = zipper(src);
        let next = edit(z, Replace("m/x:definition", "4"));
        check(
          string,
          "only member RHS changes",
          "let m = {\n  let x = 4;\n\n\n  let y = 2\n} in\n\nlet n = {\n let z = 3\n} in m.x",
          text(segment(next)),
        );
        check_survivors(segment(z), segment(next));
      },
    ),
    test_case(
      "insertion and indentation retain existing syntax objects",
      `Quick,
      () => {
        let z = zipper("let m = {\n  let x = 1;\n  let y = 2\n} in m");
        let next = edit(z, Insert(After("m/x"), "let z = 9"));
        check_survivors(segment(z), segment(next));
      },
    ),
    test_case(
      "broad replacement reuses ends without duplicating ids",
      `Quick,
      () => {
        let before = parse("[1, 1]");
        let after = EditIdentity.reuse(before, parse("[1, 1, 1]"));
        let rec ids = seg =>
          List.concat_map(
            ~f=
              (p: Piece.t) =>
                [
                  Piece.id(p),
                  ...switch (p) {
                     | Tile(t) => List.concat_map(~f=ids, t.children)
                     | _ => []
                     },
                ],
            seg,
          );
        let all = ids(after);
        check(
          int,
          "all ids unique",
          List.length(all),
          List.length(List.dedup_and_sort(~compare, all)),
        );
        check(string, "requested result", "[1, 1, 1]", text(after));
        check_survivors(before, after);
      },
    ),
    test_case(
      "comments are content, not identity-only changes",
      `Quick,
      () => {
        let before = parse("1 # old #");
        let after = EditIdentity.reuse(before, parse("1 # new #"));
        check(
          bool,
          "comment change is visible",
          false,
          EditIdentity.same_content(before, after),
        );
        check(string, "new comment kept", "1 # new #", text(after));
      },
    ),
    test_case(
      "outline move preserves definition identity",
      `Quick,
      () => {
        let before = parse("let a = 1 in\nlet b = 2 in\na + b");
        let id = CorpusUtil.row_by_label(CorpusUtil.outline_of(before), "b");
        switch (
          Web.ItemEdit.edit(
            {
              outline: CorpusUtil.outline_of(before),
              mod_root: false,
              info_map: lazy(Id.Map.empty),
              is_open: _ => false,
            },
            Op(MoveUp, id),
            before,
          )
        ) {
        | Error(e) => fail("outline move failed: " ++ e)
        | Ok((after, _)) =>
          check_survivors(before, after);
          check(
            bool,
            "moved binding keeps identity",
            true,
            Id.Map.mem(id, EditIdentity.index(after)),
          );
        };
      },
    ),
  ],
);
