open Alcotest;
open Haz3lcore;

/* Ground-truth positions, rather than only flat/incremental parity: both
   implementations can agree while reserving a drawer on the wrong line. */
let compact = s => Str.global_replace(Str.regexp("[ \n\t\r]+"), "", s);

let cases = [
  (
    "single-line drawer height",
    "f(11, 22);\n333",
    [("f(11,22)", 5)],
    [("22", 0), ("333", 5)],
    5,
  ),
  (
    "multiline call keeps its arguments together",
    "f(11,\n22);\n333",
    [("f(11,22)", 2)],
    [("22", 0), ("333", 2)],
    2,
  ),
  (
    "multiline drawer uses its full height",
    "f(11,\n22);\n333",
    [("f(11,22)", 6)],
    [("22", 0), ("333", 6)],
    6,
  ),
  (
    "same-line drawers reserve the maximum height",
    "f(11,22) + f(33,44);\n333",
    [("f(11,22)", 2), ("f(33,44)", 5)],
    [("44", 0), ("333", 5)],
    5,
  ),
  (
    "nested drawers reserve space after their own calls",
    "f(f(11,22),\n44);\n333",
    [("f(11,22)", 3), ("f(f(11,22),44)", 5)],
    [("22", 0), ("44", 3), ("333", 8)],
    8,
  ),
  (
    "drawer at end of document",
    "f(11,\n22)",
    [("f(11,22)", 4)],
    [("22", 0)],
    4,
  ),
];

let case_ = (~measure, (name, source, drawers, positions, added_rows)) =>
  test_case(
    name,
    `Quick,
    () => {
      let z =
        PersistentZipper.parse_text(
          ~source="drawer measurement",
          ~root=Exp,
          source,
        )
        |> Option.get;
      let seg = Zipper.zip(z);
      let parsed = MakeTerm.from_zip_for_sem(z, ~root=Exp);
      let id_of = text =>
        Id.Map.bindings(parsed.term_data)
        |> List.find_map(((id, _)) =>
             switch (TermData.segment(id, parsed.term_data)) {
             | Some(seg)
                 when compact(Printer.of_segment(~holes="?", seg)) == text =>
               Some(id)
             | _ => None
             }
           )
        |> Option.get;
      let rows =
        drawers
        |> List.map(((text, n)) => (id_of(text), n))
        |> Id.Map.of_list;
      let base = measure(seg, Id.Map.empty);
      let opened = measure(seg, rows);
      List.iter(
        ((text, delta)) => {
          let id = id_of(text);
          let before = Measured.find_by_id(id, base) |> Option.get;
          let after = Measured.find_by_id(id, opened) |> Option.get;
          check(
            int,
            text ++ " row",
            before.origin.row + delta,
            after.origin.row,
          );
          check(int, text ++ " column", before.origin.col, after.origin.col);
        },
        positions,
      );
      check(
        int,
        "total rows",
        Measured.num_rows(base) + added_rows,
        Measured.num_rows(opened),
      );
      let closed = measure(seg, Id.Map.empty);
      check(
        int,
        "closing removes reserved rows",
        Measured.num_rows(base),
        Measured.num_rows(closed),
      );
    },
  );

let tests = (
  "Drawer measurement",
  List.map(
    case_(~measure=(seg, rows) =>
      Measured.of_segment(seg, Id.Map.empty, rows)
    ),
    cases,
  ),
);
