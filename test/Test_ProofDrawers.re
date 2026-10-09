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

let tests = (
  "ProofDrawers",
  [
    test_case("a drawer under each theorem", `Quick, placed),
    test_case("a theorem's cell", `Quick, in_a_cell),
  ],
);
