open Alcotest;
open Haz3lcore;
open Language;

/* The ⇓ toggle's probe: one bare drawer probe on the program's last
   expression, re-anchored as that expression changes */

let zipper = (src: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Exp, src)) {
  | Some(z) => z
  | None => Alcotest.fail("could not parse: " ++ src)
  };

let analysed = (z: Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  (info_map, CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, z));
};

let text = (syntax: CachedSyntax.t, id: Id.t): string =>
  switch (TermData.segment(id, syntax.term_data)) {
  | Some(seg) =>
    Printer.of_segment(~holes="?", ~indent="", ~is_single_line=true, seg)
    |> String.trim
  | None => "<no segment>"
  };

let tail_text = (src: string): string => {
  let (_, syntax) = analysed(zipper(src));
  switch (AutoProbePerform.tail_id(syntax)) {
  | Some(id) => text(syntax, id)
  | None => "<none>"
  };
};

let update = (~on, z: Zipper.t): Zipper.t => {
  let (info_map, syntax) = analysed(z);
  AutoProbePerform.update_tail(~on, ~syntax, ~info_map, z);
};

let finds_tail = () => {
  check(string, "a bare expression", "1 + 2", tail_text("1 + 2"));
  check(
    string,
    "past lets",
    "f(x)",
    tail_text("let x = 1 in\nlet f = fun y -> y in\nf(x)"),
  );
  check(
    string,
    "past a type and a test",
    "(a, b)",
    tail_text(
      "type t = Int in\nlet a = 1 in\ntest a == 1 end;\nlet b = 2 in\n(a, b)",
    ),
  );
};

/* on: a bare drawer probe at the tail; off: gone */
let toggles = () => {
  let z = zipper("let x = 1 in\nx + 1");
  let on = update(~on=true, z);
  let tail = on.refractors.tail_target;
  check(bool, "anchored", true, tail != None);
  let entry =
    Option.bind(tail, id =>
      Id.Map.find_opt(id, on.refractors.multis.ephemerals)
    );
  check(
    bool,
    "a bare drawer",
    true,
    switch (entry) {
    | Some(e) => e.model == ProbeProj.tail_model
    | None => false
    },
  );
  check(
    bool,
    "probed",
    true,
    Id.Map.mem(Option.get(tail), CachedStatics.probe_ids_of_zipper(on)),
  );
  let off = update(~on=false, on);
  check(bool, "unanchored", true, off.refractors.tail_target == None);
  check(
    bool,
    "removed",
    false,
    Id.Map.mem(Option.get(tail), off.refractors.multis.ephemerals),
  );
};

/* a steady program keeps its entry (and the drawer state in it) */
let steady = () => {
  let z = update(~on=true, zipper("let x = 1 in\nx + 1"));
  let again = update(~on=true, z);
  check(
    bool,
    "same ephemerals",
    true,
    again.refractors.multis.ephemerals === z.refractors.multis.ephemerals,
  );
};

/* a manual probe on the last expression wins over the tail's */
let manual_wins = () => {
  let z = update(~on=true, zipper("let x = 1 in\n^^probe(x + 1)"));
  let tail = Option.get(z.refractors.tail_target);
  check(
    bool,
    "a manual probe",
    true,
    List.mem_assoc(tail, z.refractors.manuals),
  );
  check(
    bool,
    "no ephemeral beside it",
    false,
    Id.Map.mem(tail, z.refractors.multis.ephemerals),
  );
};

/* the program's drawer is drawn after the program and reserves no rows,
   so toggling ⇓ never moves the code */
let reserves_nothing = () => {
  let z = update(~on=true, zipper("let x = 1 in\nx + 1"));
  let (info_map, _) = analysed(z);
  let rows =
    CachedSyntax.mk_refractor_rows(
      z,
      snd(analysed(z)).term_data,
      info_map,
      Language.Dynamics.Map.empty,
      ~elaborated=None,
    );
  check(bool, "no rows", true, Id.Map.is_empty(rows));
};

/* a drawer's rows go under its term's last line, not under its operator:
   `1 +` / `2` gets no blank line between */
let rows_under_last_line = () => {
  let z = zipper("1 +\n2");
  let (_, syntax) = analysed(z);
  let plus = Option.get(AutoProbePerform.tail_id(syntax));
  let key = CachedSyntax.rows_key(plus, syntax.term_data);
  check(bool, "not the operator", true, key != plus);
  let seg = Zipper.unselect_and_zip(z);
  let row_of_2 = rows => {
    let m = Measured.of_segment(seg, Id.Map.empty, rows);
    switch (Measured.find_by_id(key, m)) {
    | Some(mm) => mm.origin.row
    | None => Alcotest.fail("no 2")
    };
  };
  check(int, "2 on the next line", 1, row_of_2(Id.Map.singleton(key, 1)));
  check(
    int,
    "keyed by the operator, a blank line",
    2,
    row_of_2(Id.Map.singleton(plus, 1)),
  );
};

let tests = (
  "TailProbe",
  [
    test_case(
      "the program's drawer reserves no rows",
      `Quick,
      reserves_nothing,
    ),
    test_case(
      "drawer rows under the last line",
      `Quick,
      rows_under_last_line,
    ),
    test_case("the last expression", `Quick, finds_tail),
    test_case("on and off", `Quick, toggles),
    test_case("steady", `Quick, steady),
    test_case("a manual probe wins", `Quick, manual_wins),
  ],
);
