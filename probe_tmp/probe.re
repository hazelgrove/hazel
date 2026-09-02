/* Probe: run canonical completion on the three buffers from Cyrus's
   PR #2374 comments and print the parse, the completed segment, and
   the insertion schedule. */
open Haz3lcore;

let print_seg = Printer.of_segment(~holes="?", ~refractors=[]);
let print_seg_g =
  Printer.of_segment(~holes="{G}", ~concave_holes="{g}", ~refractors=[]);

let show_delim = (d: CanonicalCompletion.delimiter_info) =>
  Printf.sprintf(
    "%S%s%s",
    d.text,
    d.needs_hole ? "+hole" : "",
    switch (d.typed_len) {
    | Some(n) => Printf.sprintf("(typed %d)", n)
    | None => ""
    },
  );

let piece_text = (seg: Segment.t, id: Id.t): string => {
  let rec find = (sg: Segment.t) =>
    sg
    |> List.fold_left(
         (acc, p: Piece.t) =>
           switch (acc) {
           | Some(_) => acc
           | None =>
             if (Id.equal(Piece.id(p), id)) {
               Some(
                 switch (p) {
                 | Tile(t) => String.concat("", t.label)
                 | Grout(_) => "<grout>"
                 | Secondary(w) =>
                   Secondary.is_linebreak(w) ? "<lb>" : "<space>"
                 | Projector(_) => "<proj>"
                 },
               );
             } else {
               switch (p) {
               | Tile(t) =>
                 t.children
                 |> List.fold_left(
                      (acc, ch) =>
                        switch (acc) {
                        | Some(_) => acc
                        | None => find(ch)
                        },
                      None,
                    )
               | _ => None
               };
             }
           },
         None,
       );
  switch (find(seg)) {
  | Some(s) => s
  | None => "<?>"
  };
};

let case = (name: string, input: string) => {
  Printf.printf("=== %s ===\ninput:\n%s\n", name, input);
  switch (Parser.to_segment(input, ~root=Exp)) {
  | None => print_endline("PARSE FAIL")
  | Some(seg) =>
    Printf.printf("parsed (grout marked):\n%s\n", print_seg_g(seg));
    let result =
      CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
    Printf.printf("completed:\n%s\n", print_seg(result.completed_seg));
    Printf.printf("insertions:\n");
    result.insertions
    |> List.iter((ins: CanonicalCompletion.insertion) =>
         Printf.printf(
           "  anchor=%s side=%s delims=[%s]\n",
           piece_text(seg, ins.adjacent_id),
           switch (ins.side) {
           | Left => "L"
           | Right => "R"
           },
           ins.delimiters |> List.map(show_delim) |> String.concat(", "),
         )
       );
    print_newline();
  };
};

/* Type a string through the real edit pipeline (Perform.go, statics on,
   auto-indent as the editor does it) and complete the resulting state. */
let typed_case = (~auto_reindent=false, name: string, input: string) => {
  let settings = {
    ...Language.CoreSettings.off,
    statics: true,
    auto_reindent,
  };
  let perform_one = (z: Zipper.t, a: Action.t): Zipper.t => {
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let statics =
      CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, term);
    switch (
      Perform.go(
        ~settings,
        ~statics,
        ~syntax=CachedSyntax.init(z),
        a,
        {
          zipper: z,
          col_target: None,
        },
        ~root=Exp,
      )
    ) {
    | Ok(z) => z
    | Error(err) =>
      Printf.printf("ACTION FAIL: %s\n", Action.Failure.show(err));
      z;
    };
  };
  let actions = input |> Token.to_list |> List.map(c => Action.Insert(c));
  let z = List.fold_left(perform_one, Zipper.init(), actions);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  Printf.printf("=== %s (typed) ===\n", name);
  Printf.printf("state (grout marked):\n%s\n", print_seg_g(seg));
  let result = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  Printf.printf("completed:\n%s\n", print_seg(result.completed_seg));
  Printf.printf("insertions:\n");
  result.insertions
  |> List.iter((ins: CanonicalCompletion.insertion) =>
       Printf.printf(
         "  anchor=%s side=%s delims=[%s]\n",
         piece_text(seg, ins.adjacent_id),
         switch (ins.side) {
         | Left => "L"
         | Right => "R"
         },
         ins.delimiters |> List.map(show_delim) |> String.concat(", "),
       )
     );
  print_newline();
};

/* Like typed_case but with extra actions appended after the typing. */
let typed_actions_case =
    (~auto_reindent=true, name, input, extra: list(Action.t)) => {
  let settings = {
    ...Language.CoreSettings.off,
    statics: true,
    auto_reindent,
  };
  let perform_one = (z: Zipper.t, a: Action.t): Zipper.t => {
    let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
    let statics =
      CachedStatics.init_from_term(~settings, ~is_dynamic_term=true, term);
    switch (
      Perform.go(
        ~settings,
        ~statics,
        ~syntax=CachedSyntax.init(z),
        a,
        {
          zipper: z,
          col_target: None,
        },
        ~root=Exp,
      )
    ) {
    | Ok(z) => z
    | Error(err) =>
      Printf.printf("ACTION FAIL: %s\n", Action.Failure.show(err));
      z;
    };
  };
  let actions =
    (input |> Token.to_list |> List.map(c => Action.Insert(c))) @ extra;
  let z = List.fold_left(perform_one, Zipper.init(), actions);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  Printf.printf("=== %s (typed+actions) ===\n", name);
  Printf.printf("state (grout marked):\n%s\n", print_seg_g(seg));
  let result = CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
  Printf.printf("completed:\n%s\n", print_seg(result.completed_seg));
  Printf.printf("insertions:\n");
  result.insertions
  |> List.iter((ins: CanonicalCompletion.insertion) =>
       Printf.printf(
         "  anchor=%s side=%s delims=[%s]\n",
         piece_text(seg, ins.adjacent_id),
         switch (ins.side) {
         | Left => "L"
         | Right => "R"
         },
         ins.delimiters |> List.map(show_delim) |> String.concat(", "),
       )
     );
  print_newline();
};

let () = {
  /* Issue A (decoration): blank line between let qsort( and test */
  case(
    "A: let qsort( / blank / test",
    "let qsort(\n\ntest qsort([5, 4, 8, 9, 3, 2, 7]) == [2, 3, 4, 5, 7, 8, 9] end",
  );
  /* Issue B: closing paren placement vs the : */
  case(
    "B: let qsort(xs : / test",
    "let qsort(xs :\ntest qsort([5, 4, 8, 9, 3, 2, 7]) == [2, 3, 4, 5, 7, 8, 9] end",
  );
  /* Issue C: end+in placement with indented if under the rule */
  case(
    "C: partition_at case, if body indent 4",
    "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\n    if ",
  );
  case(
    "C2: same, if body indent 2",
    "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\n  if ",
  );
  case(
    "C3: same, if body indent 6",
    "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\n      if ",
  );
  typed_case(
    "C-typed: partition_at, auto-indent",
    "let partition_at(xs : [Int], pivot: Int) =\ncase xs\n| [] => ([], [])\n| hd::tl =>\nif ",
  );
  typed_case(
    ~auto_reindent=true,
    "C-typed-reindent: partition_at, auto_reindent on",
    "let partition_at(xs : [Int], pivot: Int) =\ncase xs\n| [] => ([], [])\n| hd::tl =>\nif ",
  );
  typed_case(
    "B-typed: let qsort(xs : above test",
    "let qsort(xs :\ntest qsort([5, 4, 8, 9, 3, 2, 7]) == [2, 3, 4, 5, 7, 8, 9] end",
  );
  case(
    "C4: if at indent 0",
    "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\nif ",
  );
  typed_actions_case(
    "C5: typed inline, then Enter before if",
    "let partition_at(xs : [Int], pivot: Int) =\ncase xs\n| [] => ([], [])\n| hd::tl => if ",
    /* caret at end after "if "; move left over "if " (3 chars) then Enter */
    List.init(3, _ => Action.Move(Local(Left, ByChar)))
    @ [Action.Insert("\n")],
  );
  case("B2: let qsort(xs : with nothing following", "let qsort(xs :");
  typed_case(
    ~auto_reindent=true,
    "C7: mid-entry, Enter after => before typing if",
    "let partition_at(xs : [Int], pivot: Int) =\ncase xs\n| [] => ([], [])\n| hd::tl =>\n",
  );
  case("B3: trailing + with content following", "let x = (1 +\nf(3)");
  /* F-series: andrew's live repro 2026-09-01 */
  case(
    "F1: bare i, 1 below",
    "let f =\n    case 0\n    | 0 =>\n        i\n1",
  );
  case("F2: if, 1 below", "let f =\n    case 0\n    | 0 =>\n        if \n1");
  case("F3: no 1 below", "let f =\n    case 0\n    | 0 =>\n        if ");
  case(
    "F4: Cyrus program + 1 below",
    "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\n    if \n1",
  );
  case("F5: standalone case, if, 1 below", "case 0\n| 0 =>\n    if \n1");
  /* E-series: hole-min back-over boundary cases (issue 2) */
  case("E1: deleted test-end before ;", "test 1 == 1 ;\ntest 2 == 2 end");
  case("E4: lone paren, trailing ; , Exp follows", "(1 + 2;\n3");
  case("E5: lone paren, trailing : , Exp follows", "(x :\nf(3)");
  case("E6: lone paren, trailing + , Exp follows", "(1 +\nf(3)");
  /* D5: standalone case at top level, typed with auto-indent —
     exercises case's own indentation scheme with inc_ind = 0 */
  typed_case(
    ~auto_reindent=true,
    "D5: standalone top-level case, typed",
    "case xs\n| [] => 1\n| hd::tl =>\nif ",
  );
  /* D-series: same shapes with NO case expression anywhere */
  case("D1: dangling + , next line at col 0", "let y =\n  1 +\nif ");
  case(
    "D3 control: dangling + , next line indented",
    "let y =\n  1 +\n    if ",
  );
  case(
    "D4: dangling fun -> , next line at col 0",
    "let f =\n  fun x ->\nif ",
  );
  {
    /* D2: dangling +, next line indented but grout-headed */

    let input = "let y =\n  1 +\n    if ";
    switch (Parser.to_segment(input, ~root=Exp)) {
    | None => print_endline("D2 PARSE FAIL")
    | Some(seg) =>
      let rec last_lb = (i, j, ps: Segment.t) =>
        switch (ps) {
        | [] => j
        | [Piece.Secondary(s), ...tl] when Secondary.is_linebreak(s) =>
          last_lb(i + 1, i, tl)
        | [_, ...tl] => last_lb(i + 1, j, tl)
        };
      let j = last_lb(0, -1, seg);
      let (a, b) = Util.ListUtil.split_n(j + 1, seg);
      let grout: Piece.t =
        Grout({
          id: Id.mk(),
          shape: Concave,
        });
      let seg = a @ [grout] @ b;
      Printf.printf(
        "=== D2: dangling +, indented if line, grout-headed ===\n",
      );
      Printf.printf("state (grout marked):\n%s\n", print_seg_g(seg));
      let result =
        CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
      Printf.printf("completed:\n%s\n", print_seg(result.completed_seg));
      result.insertions
      |> List.iter((ins: CanonicalCompletion.insertion) =>
           Printf.printf(
             "  anchor=%s side=%s delims=[%s]\n",
             piece_text(seg, ins.adjacent_id),
             switch (ins.side) {
             | Left => "L"
             | Right => "R"
             },
             ins.delimiters |> List.map(show_delim) |> String.concat(", "),
           )
         );
      print_newline();
    };
  };
  /* C6: hypothesis — stale grout at the head of the if line makes
     count_leading_spaces read 0 despite visible indentation */

  let input = "let partition_at(xs : [Int], pivot: Int) =\n  case xs\n  | [] => ([], [])\n  | hd::tl =>\n    if ";
  switch (Parser.to_segment(input, ~root=Exp)) {
  | None => print_endline("C6 PARSE FAIL")
  | Some(seg) =>
    /* insert a concave grout right after the LAST linebreak,
       before the indent spaces */
    let rec last_lb = (i, j, ps: Segment.t) =>
      switch (ps) {
      | [] => j
      | [Piece.Secondary(s), ...tl] when Secondary.is_linebreak(s) =>
        last_lb(i + 1, i, tl)
      | [_, ...tl] => last_lb(i + 1, j, tl)
      };
    let j = last_lb(0, -1, seg);
    let (a, b) = Util.ListUtil.split_n(j + 1, seg);
    let grout: Piece.t =
      Grout({
        id: Id.mk(),
        shape: Concave,
      });
    let seg = a @ [grout] @ b;
    Printf.printf("=== C6: stale grout at if-line head ===\n");
    Printf.printf("state (grout marked):\n%s\n", print_seg_g(seg));
    let result =
      CanonicalCompletion.complete_segment_deep(~sort=Sort.Exp, seg);
    Printf.printf("completed:\n%s\n", print_seg(result.completed_seg));
    result.insertions
    |> List.iter((ins: CanonicalCompletion.insertion) =>
         Printf.printf(
           "  anchor=%s side=%s delims=[%s]\n",
           piece_text(seg, ins.adjacent_id),
           switch (ins.side) {
           | Left => "L"
           | Right => "R"
           },
           ins.delimiters |> List.map(show_delim) |> String.concat(", "),
         )
       );
  };
};

/* G-series: quiver ORDER stability under prefix typing (andrew 2026-09-01) */
let g = () => {
  case("G1: rule arrow pending", "case true\n| false ");
  case("G2: typed = prefix of =>", "case true\n| false =");
  case("G3: let-in pending", "let x = 1");
  case("G4: typed i prefix of in", "let x = 1 i");
  case("G5: if-else pending", "if true then 1");
  case("G6: typed e prefix of else", "if true then 1 e");
  case("G7: test-end pending", "test 1 == 1");
  case("G8: typed en prefix of end", "test 1 == 1 en");
  case("G9: case-end after rule body", "case true\n| false => 2");
  case("G10: typed e prefix of end", "case true\n| false => 2\ne");
};
let () = g();
