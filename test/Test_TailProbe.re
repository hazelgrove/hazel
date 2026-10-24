open Alcotest;
open Haz3lcore;
open Language;

/* The ⇓ toggle's probe: one drawer probe on the program's last
   expression, re-anchored as that expression changes */

let zipper = CorpusUtil.typed_zipper(~root=Exp);

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

let tail_text = (~root=Sort.Exp, src: string): string => {
  let (_, syntax) =
    switch (root) {
    | Exp => analysed(zipper(src))
    | _ =>
      let z = CorpusUtil.typed_zipper(~root, src);
      (
        Id.Map.empty,
        CachedSyntax.mk(
          ~root,
          ~info_map=Id.Map.empty,
          ~dyn_map=Id.Map.empty,
          z,
        ),
      );
    };
  switch (AutoProbePerform.tail_id(syntax)) {
  | Some(id) => text(syntax, id)
  | None => "<none>"
  };
};

let update = (~on, z: Zipper.t): Zipper.t => {
  let (info_map, syntax) = analysed(z);
  AutoProbePerform.update_tail(~on, ~syntax, ~info_map, z);
};

/* the rows [z]'s drawers reserve under the code */
let reserved = (z: Zipper.t) => {
  let (info_map, syntax) = analysed(z);
  CachedSyntax.mk_refractor_rows(
    z,
    syntax.term_data,
    info_map,
    Dynamics.Map.empty,
    ~elaborated=None,
  );
};

/* by the predicate the views use */
let is_program_value = (z: Zipper.t, id: Id.t): bool =>
  switch (Id.Map.find_opt(id, CachedSyntax.refractor_entries(z.refractors))) {
  | Some(e) =>
    ProjectorInfo.is_program_value(
      ~tail=z.refractors.tail_target,
      id,
      e.kind,
      e.model,
    )
  | None => false
  };

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
  /* a module root's: its last item, if an expression (a world slide's
     members) */
  check(
    string,
    "the last member",
    "x + 1",
    tail_text(~root=Mod, "let x = 1;\ntest x == 1 end;\nx + 1"),
  );
  check(
    string,
    "none after a definition",
    "<none>",
    tail_text(~root=Mod, "let x = 1;\nlet y = x"),
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

/* a manual probe on the last expression wins over the tail's: it stays
   an ordinary probe, and the tail still names its term */
let manual_wins = () => {
  let (z, last) = manual_first();
  check(
    bool,
    "a manual probe",
    true,
    List.mem_assoc(last, z.refractors.manuals),
  );
  check(bool, "the tail", true, z.refractors.tail_target == Some(last));
  check(
    bool,
    "no ephemeral beside it",
    false,
    Id.Map.mem(last, z.refractors.multis.ephemerals),
  );
  check(bool, "not the program value", false, is_program_value(z, last));
  check(bool, "steady", true, update(~on=true, z) === z);
};

/* the ⇓ probe stays itself once made manual; ⇓ off makes it a drawer
   probe of its own, and ⇓ on takes it back */
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
  check(
    bool,
    "on again: the ⇓ probe",
    true,
    is_program_value(update(~on=true, off), tail),
  );
};

/* and through a save and load: the probe saved with the slide comes back
   into a fresh editor as the ⇓ probe */
let promoted_reloads = () => {
  let (z, tail) = promoted();
  let seg = Zipper.unselect_and_zip(z);
  Web.ScratchPersist.write_side("tailtest", "promoted", side =>
    {
      ...side,
      probes: z.refractors.manuals,
    }
  );
  let loaded =
    ZipperBase.update_refractors(Zipper.unzip(seg), r =>
      {
        ...r,
        manuals: Web.ScratchPersist.read_probes("tailtest", "promoted", seg),
      }
    );
  check(bool, "unanchored", true, loaded.refractors.tail_target == None);
  let z = update(~on=true, loaded);
  check(bool, "the ⇓ probe", true, is_program_value(z, tail));
  check(bool, "no rows", true, Id.Map.is_empty(reserved(z)));
};

/* a ⇓ probe entry made afresh (an auto-probe anchor on the last
   expression let go, say) still opens as a drawer */
let remade_drawer = () => {
  let (z, tail) = tail_on("let x = 1 in\nx + 1");
  let (info_map, syntax) = analysed(z);
  let z =
    ProbePerform.rm_multi(~drill=false, ~syntax, ~info_map, tail, z)
    |> ProbePerform.add_ids_from_multi_term(~syntax, ~info_map);
  check(
    bool,
    "a drawer",
    true,
    Id.Map.find_opt(tail, z.refractors.multis.ephemerals)
    == Some(Refractors.mk_entry(~model=ProbeProj.tail_model, Probe)),
  );
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

/* the ⇓ probe draws no arms on the code, whichever way it's held */
let no_arms = () => {
  let arms = ((z: Zipper.t, _)): int => {
    let (_, syntax) = analysed(z);
    Web.Arms.Refractors.of_zipper(
      ~font_metrics=Web.FontMetrics.init,
      ~syntax,
      ~completion=Web.Arms.lazy_completion(~root=Exp, z),
      z,
    )
    |> List.length;
  };
  check(int, "the ⇓ probe", 0, arms(tail_on("let x = 1 in\nx + 1")));
  check(int, "made manual", 0, arms(promoted()));
  check(
    bool,
    "a manual probe already there",
    true,
    arms(manual_first()) > 0,
  );
};

/* the program's drawer is drawn after the program and reserves no rows,
   so toggling ⇓ never moves the code */
let reserves_nothing = () => {
  let z = update(~on=true, zipper("let x = 1 in\nx + 1"));
  check(bool, "no rows", true, Id.Map.is_empty(reserved(z)));
};

/* a pin or step-into makes the ⇓ probe a manual probe; its drawer stays
   after the program, so still no rows */
let promoted_reserves_nothing = () => {
  let (z, tail) = promoted();
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
  check(bool, "no rows", true, Id.Map.is_empty(reserved(z)));
};

/* its rows come back when it stops being the ⇓ probe, though no
   refractor entry changed */
let rows_follow_role = () => {
  let (z, _) = promoted();
  let (info_map, syntax) = analysed(z);
  check(
    bool,
    "none as the ⇓ probe",
    true,
    Id.Map.is_empty(syntax.refractor_rows),
  );
  let off = update(~on=false, z);
  let syntax = CachedSyntax.calculate(off, info_map, Id.Map.empty, syntax);
  check(
    bool,
    "its own once ⇓ is off",
    false,
    Id.Map.is_empty(syntax.refractor_rows),
  );
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
    test_case("rows follow the ⇓ role", `Quick, rows_follow_role),
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
    test_case("made manual, saved and loaded", `Quick, promoted_reloads),
    test_case("a remade entry is a drawer", `Quick, remade_drawer),
    test_case("its projector is told", `Quick, projector_flag),
    test_case("no arms on the code", `Quick, no_arms),
  ],
);
