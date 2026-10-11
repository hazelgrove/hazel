open Alcotest;
open Haz3lcore;
open Language;

/* A drawer under each top-level theorem, holding its proof */

let zipper = (src: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Exp, src)) {
  | Some(z) => z
  | None => Alcotest.fail("could not parse: " ++ src)
  };

let syntax = (z: Zipper.t): CachedSyntax.t => {
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  CachedSyntax.mk(~info_map, ~dyn_map=Id.Map.empty, z);
};

let update = (~on, z: Zipper.t): Zipper.t =>
  AutoProbePerform.update_proofs(
    ~proofs=on ? Theorems : NoProofs,
    ~syntax=syntax(z),
    z,
  );

let src = "theorem a = 1 + 1 == 2 in\nlet x = 5 in\ntheorem b = x == 5 in\nx";

let placed = () => {
  let z = zipper(src);
  let ids = AutoProbePerform.theorem_ids(syntax(z));
  check(int, "both theorems", 2, List.length(ids));
  let on = update(~on=true, z);
  check(
    bool,
    "a drawer for each",
    true,
    List.for_all(
      id =>
        switch (Id.Map.find_opt(id, on.refractors.proofs)) {
        | Some(e) => e.kind == Proof
        | None => false
        },
      ids,
    ),
  );
  check(
    bool,
    "never sampled",
    false,
    List.exists(
      id => Id.Map.mem(id, CachedStatics.probe_ids_of_zipper(on)),
      ids,
    ),
  );
  let again = update(~on=true, on);
  check(
    bool,
    "steady",
    true,
    again.refractors.proofs === on.refractors.proofs,
  );
  let off = update(~on=false, on);
  check(bool, "off", true, Id.Map.is_empty(off.refractors.proofs));
};

/* a theorem's cell holds only its statement: the drawer goes under it
   and names the theorem it shows */
let in_a_cell = () => {
  let z = zipper("1 + 1 == 2");
  let thm = Id.mk();
  let placed =
    AutoProbePerform.update_proofs(
      ~proofs=ProofOf(thm),
      ~syntax=syntax(z),
      z,
    );
  check(
    bool,
    "one drawer, naming its theorem",
    true,
    switch (Id.Map.bindings(placed.refractors.proofs)) {
    | [(_, e)] => e.model == ProofProj.model_string(~theorem=thm, ())
    | _ => false
    },
  );
};

/* a proof drawer's step takes focus as the steps drawer's does: view
   state the model keeps through recalculation, so the step draws its
   selection and takes the keys */
let step_focus = () => {
  let calc = ce =>
    Web.CellEditor.Update.calculate(
      ~settings=CoreSettings.on,
      ~proofs=Theorems,
      ~is_edited=true,
      ~statics_mode=Force,
      ~queue_worker=None,
      ~stitch=x => x,
      ce,
    );
  let ce =
    calc(
      Web.CellEditor.Model.mk(
        Editor.Model.mk(~root=Exp, zipper("theorem a = 1 + 1 == 2 in\n1")),
      ),
    );
  check(
    int,
    "one theorem",
    1,
    List.length(Util.Calc.get_saved([], ce.result.theorems.thms)),
  );
  let focus = Some((0, Web.StepperBase.Here()));
  let focused =
    Web.EvalResult.Update.update(
      ~settings=Web.Settings.Model.init,
      TheoremsAction(TheoremFocus(focus)),
      ce.result,
    );
  check(bool, "not an edit", false, focused.is_edit);
  check(bool, "not an undo step", false, focused.historic);
  let ce =
    calc({
      ...ce,
      result: focused.model,
    });
  check(bool, "kept", true, ce.result.theorems.focus == focus);
};

let tests = (
  "ProofDrawers",
  [
    test_case("a drawer under each theorem", `Quick, placed),
    test_case("a theorem's cell", `Quick, in_a_cell),
    test_case("a proof step takes focus", `Quick, step_focus),
  ],
);
