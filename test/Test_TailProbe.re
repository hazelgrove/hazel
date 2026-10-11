open Alcotest;
open Haz3lcore;
open Language;

/* The ⇓ toggle's probe: one drawer probe on the program's last
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

let is_program_value = (z: Zipper.t, id: Id.t): bool =>
  ProjectorInfo.is_program_value(~tail=z.refractors.tail_target, id, Probe);

/* ⇓ on over a program, with its tail */
let tail_on = (src: string): (Zipper.t, Id.t) => {
  let z = update(~on=true, zipper(src));
  (z, Option.get(z.refractors.tail_target));
};

/* a pin or step-into makes the ⇓ probe a manual probe */
let promoted = (): (Zipper.t, Id.t) => {
  let (z, tail) = tail_on("let x = 1 in\nx + 1");
  let (info_map, syntax) = analysed(z);
  (
    ProbePerform.promote_to_manual(tail, z)
    |> ProbePerform.add_ids_from_multi_term(~syntax, ~info_map),
    tail,
  );
};

/* a manual probe already on the last expression, then ⇓ on */
let manual_first = (): (Zipper.t, Id.t) => {
  let z = update(~on=true, zipper("let x = 1 in\n^^probe(x + 1)"));
  (z, Option.get(AutoProbePerform.tail_id(snd(analysed(z)))));
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

/* on: a drawer probe at the tail; off: gone */
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
    "a drawer",
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

/* a manual probe already on the last expression wins: no ⇓ probe, and
   it stays an ordinary probe */
let manual_wins = () => {
  let (z, last) = manual_first();
  check(
    bool,
    "a manual probe",
    true,
    List.mem_assoc(last, z.refractors.manuals),
  );
  check(bool, "no ⇓ probe", true, z.refractors.tail_target == None);
  check(
    bool,
    "no ephemeral beside it",
    false,
    Id.Map.mem(last, z.refractors.multis.ephemerals),
  );
  check(bool, "steady", true, update(~on=true, z) === z);
};

/* the ⇓ probe stays itself once made manual, until ⇓ is off */
let promoted_stays = () => {
  let (z, tail) = promoted();
  let again = update(~on=true, z);
  check(bool, "still the ⇓ probe", true, is_program_value(again, tail));
  check(bool, "steady", true, again === z);
  let off = update(~on=false, z);
  check(bool, "off: not", false, is_program_value(off, tail));
  check(
    bool,
    "a drawer probe of its own",
    true,
    List.assoc_opt(tail, off.refractors.manuals)
    |> Option.map((e: Refractors.entry) => e.model)
    == Some(ProbeProj.tail_model),
  );
};

/* a probe placed on the ⇓ probe's term replaces it */
let placed_replaces = () => {
  let (z, tail) = tail_on("let x = 1 in\nx + 1");
  let (info_map, syntax) = analysed(z);
  let z =
    ProbePerform.add_manual(~syntax, tail, info_map, z) |> update(~on=true);
  check(
    bool,
    "a plain manual probe",
    true,
    List.assoc_opt(tail, z.refractors.manuals)
    == Some(Refractors.mk_entry(Probe)),
  );
  check(bool, "no ⇓ probe", true, z.refractors.tail_target == None);
};

/* the ⇓ probe's projector is told so, whichever way it's held */
let projector_flag = () => {
  let flag = ((z: Zipper.t, id: Id.t)): bool => {
    let (info_map, syntax) = analysed(z);
    Web.RefractorView.mk_data(
      ~refractors=CachedSyntax.refractor_entries(z.refractors),
      ~syntax,
      ~indicated=None,
      ~statics=info_map,
      ~dynamics=Dynamics.Map.empty,
      ~sample_focus=z.refractors.sample_focus,
      ~editor_active=false,
      ~tail=z.refractors.tail_target,
      (),
    )
    |> List.exists((d: Web.ProjectorView.Model.projector_data) =>
         d.p.id == id && d.info.program_value
       );
  };
  check(bool, "the ⇓ probe", true, flag(tail_on("let x = 1 in\nx + 1")));
  check(bool, "made manual", true, flag(promoted()));
  check(bool, "a manual probe already there", false, flag(manual_first()));
};

/* models saved while the ⇓ probe's had a `bare` field keep the rest */
let old_model_loads = () => {
  let saved = "((active_renderer())(drawer_mode true)(dropdown_redraw 3)(auto_rich false)(rich_off true)(bare true))";
  let m = ProbeProj.probe_model_of_sexp(Sexplib.Sexp.of_string(saved));
  check(bool, "drawer", true, m.drawer_mode);
  check(int, "redraw count", 3, m.dropdown_redraw);
  check(bool, "rich off", true, m.rich_off);
  /* and through the projector, as a stored manual probe */
  let z = zipper("let x = 1 in\nx + 1");
  let (info_map, syntax) = analysed(z);
  let z =
    Zipper.add_manual(
      ~model=saved,
      Option.get(AutoProbePerform.tail_id(syntax)),
      Probe,
      z,
    );
  let rows =
    CachedSyntax.mk_refractor_rows(
      z,
      syntax.term_data,
      info_map,
      Dynamics.Map.empty,
      ~elaborated=None,
    );
  check(bool, "its drawer reserves rows", false, Id.Map.is_empty(rows));
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

/* a pin or step-into makes the ⇓ probe a manual probe; its drawer stays
   after the program, so still no rows */
let promoted_reserves_nothing = () => {
  let (z, tail) = promoted();
  let (info_map, syntax) = analysed(z);
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
  let rows =
    CachedSyntax.mk_refractor_rows(
      z,
      syntax.term_data,
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
      "a promoted program drawer reserves no rows",
      `Quick,
      promoted_reserves_nothing,
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
    test_case("made manual, still the ⇓ probe", `Quick, promoted_stays),
    test_case("a probe placed on it replaces it", `Quick, placed_replaces),
    test_case("its projector is told", `Quick, projector_flag),
    test_case("an old model with bare loads", `Quick, old_model_loads),
  ],
);
