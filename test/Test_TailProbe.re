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

let tests = (
  "TailProbe",
  [
    test_case("the last expression", `Quick, finds_tail),
    test_case("on and off", `Quick, toggles),
    test_case("steady", `Quick, steady),
    test_case("a manual probe wins", `Quick, manual_wins),
  ],
);
