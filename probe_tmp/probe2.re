/* Probe the QUIVER DISPLAY pipeline (resolve_position -> sort ->
   coalesce_overlaps) on typed states, to explain chip-order flips. */
open Haz3lcore;
open Web;

let settings = {
  ...Language.CoreSettings.off,
  statics: true,
  auto_reindent: true,
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

let font_metrics: FontMetrics.t = {
  row_height: 20.0,
  col_width: 10.0,
};

let case = (name: string, input: string) => {
  let actions = input |> Token.to_list |> List.map(c => Action.Insert(c));
  let z = List.fold_left(perform_one, Zipper.init(), actions);
  let syntax = CachedSyntax.init(z);
  let engine_seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let caret = Zipper.Caret.point(syntax.measured, z);
  Printf.printf("=== %s ===\ncaret=(%d,%d)\n", name, caret.row, caret.col);
  let result = CanonicalCompletion.for_editor(engine_seg);
  let positioned =
    result.insertions
    |> List.mapi((idx, ins) =>
         QuiverDec.resolve_position(
           ~idx,
           ~seg=engine_seg,
           ~caret_pos=Some((caret.row, caret.col)),
           syntax.measured,
           ins,
         )
       )
    |> List.filter_map(x => x);
  let show_pin = (p: QuiverDec.positioned_insertion) =>
    Printf.printf(
      "  pin (%d,%d) [%s]\n",
      p.row,
      p.col,
      p.delimiters
      |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
      |> String.concat(" "),
    );
  Printf.printf("resolved pins (engine order):\n");
  List.iter(show_pin, positioned);
  let sorted =
    List.sort(
      (a: QuiverDec.positioned_insertion, b: QuiverDec.positioned_insertion) => {
        let row_cmp = Int.compare(a.row, b.row);
        row_cmp != 0 ? row_cmp : Int.compare(a.col, b.col);
      },
      positioned,
    );
  let chips = QuiverDec.coalesce_overlaps(~font_metrics, sorted);
  Printf.printf("displayed chips (after sort+coalesce):\n");
  List.iter(show_pin, chips);
  print_newline();
};

let () = {
  case("Q1: | false<space>", "case true\n| false ");
  case("Q2: | false =", "case true\n| false =");
  case("Q3: let x = 1", "let x = 1");
  case("Q4: let x = 1 i", "let x = 1 i");
  case("Q5: if true then 1", "if true then 1");
  case("Q6: if true then 1 e", "if true then 1 e");
};

let () = {
  case("Q7: let-wrapped, 3 obligations, pre", "let f = case true\n| false ");
  case(
    "Q8: let-wrapped, 3 obligations, typed =",
    "let f = case true\n| false =",
  );
};

let () = {
  case("R1: inner paren + typed i of outer in", "let x = (1 i");
  case("R2: middle = + leading [", "let x 1] in 2");
  case("R3: leading [ + trailing in", "let x = 1]");
};

/* T-series: tab put-down glom inventory */
let tab_case = (name: string, input: string, moves_left: int) => {
  let acts =
    (input |> Token.to_list |> List.map(c => Action.Insert(c)))
    @ List.init(moves_left, _ => Action.Move(Local(Left, ByChar)))
    @ [Action.Put_down];
  let z = List.fold_left(perform_one, Zipper.init(), acts);
  Printf.printf(
    "=== %s ===\n%s\n\n",
    name,
    Printer.of_zipper(~holes="?", ~caret="|", z),
  );
};

let () = {
  tab_case("T1: let x = 1 [tab]", "let x = 1", 0);
  tab_case("T2: rule body [tab] (drops end)", "case true\n| false => 2", 0);
  tab_case("T3: if true [tab] (drops then)", "if true", 0);
  tab_case("T4: if true ¦false [tab]", "if true false", 5);
  tab_case("T5: case true¦ [tab]", "case true", 0);
  tab_case("T6: test 1 == 1 [tab]", "test 1 == 1", 0);
};

let tab_case2 = (name: string, input: string, tabs: int) => {
  let acts =
    (input |> Token.to_list |> List.map(c => Action.Insert(c)))
    @ List.concat(List.init(tabs, _ => [Action.Put_down]));
  let z = List.fold_left(perform_one, Zipper.init(), acts);
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  Printf.printf(
    "=== %s ===\n%s\ntop-level tiles: %s\n\n",
    name,
    Printer.of_zipper(~holes="?", ~caret="|", z),
    seg
    |> List.filter_map((p: Piece.t) =>
         switch (p) {
         | Tile(t) =>
           Some(
             String.concat("", Tile.effective_label(t))
             ++ (Tile.is_complete(t) ? "" : "[inc]"),
           )
         | Grout(_) => Some("?g")
         | _ => None
         }
       )
    |> String.concat(" "),
  );
};

let () = {
  tab_case2("U1: let+case, one tab", "let x = case true\n| false => 2", 1);
  tab_case2("U2: let+case, two tabs", "let x = case true\n| false => 2", 2);
};

let () = {
  tab_case2(
    "U3: nested case, 1 tab",
    "let x = case true\n| false => case false\n| true => 1",
    1,
  );
  tab_case2(
    "U4: nested case, 2 tabs",
    "let x = case true\n| false => case false\n| true => 1",
    2,
  );
  tab_case2(
    "U5: nested case, 3 tabs",
    "let x = case true\n| false => case false\n| true => 1",
    3,
  );
};

/* U6: web tab dispatch parity — second "tab" goes through the chip
   (ApplyCompletion One), as CodeEditable dispatches when the caret is
   pinned to a chip */
let () = {
  let input = "let x = case true\n| false => 2";
  let acts =
    (input |> Token.to_list |> List.map(c => Action.Insert(c)))
    @ [Action.Put_down];
  let z = List.fold_left(perform_one, Zipper.init(), acts);
  let z =
    switch (CanonicalCompletion.chip_at_caret(z)) {
    | Some(ins) =>
      switch (
        ins.delimiters
        |> List.filter_map((d: CanonicalCompletion.delimiter_info) =>
             d.of_shard
           )
      ) {
      | [(id, _), ..._] =>
        Printf.printf("chip at caret -> ApplyCompletion(One)\n");
        perform_one(z, Action.ApplyCompletion(One(id)));
      | [] =>
        Printf.printf("chip has no of_shard\n");
        z;
      }
    | None =>
      Printf.printf("no chip at caret -> would Put_down\n");
      z;
    };
  Printf.printf(
    "=== U6: put_down end, then chip-tab in ===\n%s\n\n",
    Printer.of_zipper(~holes="?", ~caret="|", z),
  );
};

/* U7: Alt+M materialize-all from the pre-drop state */
let () = {
  let input = "let x = case true\n| false => 2";
  let acts =
    (input |> Token.to_list |> List.map(c => Action.Insert(c)))
    @ [Action.ApplyCompletion(All)];
  let z = List.fold_left(perform_one, Zipper.init(), acts);
  Printf.printf(
    "=== U7: materialize all ===\n%s\n",
    Printer.of_zipper(~holes="?", ~caret="|", z),
  );
};
