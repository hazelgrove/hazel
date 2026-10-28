/* Correction phase (root, after 67d37166): two D80 safety controls.

   C1. Funext domain on a FRESH goal. `witness_step` freshens ids
       (Exp.replace_all_ids), so the instantiated body has no info_map
       entries and the funext domain comes from `arrow_domain_syntactic`,
       which reads `Fun`'s type field. Statics stores the PARAMETER type
       there (Statics.re `Fun(p_elab, e_elab, Some(p.ty), n)`).
   C2. Instantiating a proven quantified lemma at a non-standard term. A
       Proven `forall (f : Int -> Int) -> !!f` cited by `axiom` at
       `fun (x : Int) -> 1 / x` must not prove `!!` of that partial
       lambda (D80: binders range over standard values). The identity
       lambda is the positive; primitive reflexivity at any term stays
       valid (D81).
   Receipts: the statics type information of the old unannotated funext
   examples (no inference is claimed). */
open Alcotest;
open Language;

module RE = Test_ResetExistentials;

let funext_refusals = (o: RE.obs, name) =>
  RE.nodes(o, name, Proof.Funext) |> List.concat_map(RE.refused_rules);

let ho_fresh =
  "theorem t = exists (n : Int) -> (fun (f : Int -> Int) -> 0) == (fun (f : Int -> Int) -> 0) proof\n"
  ++ "  == true by witness 0 end; funext f => ?\nin 0";

let fo_fresh =
  "theorem t = exists (n : Int) -> (fun (x : Int) -> x) == (fun (x : Int) -> x) proof\n"
  ++ "  == true by witness 0 end; funext x => ?\nin 0";

let lemma =
  "theorem lemma = forall (f : Int -> Int) -> !!f proof\n"
  ++ "  generalize f => forall (f : Int -> Int !! hf) => == true by hf end\nin\n";

let bad_instance =
  lemma
  ++ "theorem bad = !!(fun (x : Int) -> 1 / x) proof axiom lemma at 0 on !!(fun (x : Int) -> 1 / x) end in 0";

let good_instance =
  lemma
  ++ "theorem good = !!(fun (x : Int) -> x) proof axiom lemma at 0 on !!(fun (x : Int) -> x) end in 0";

let refl_partial =
  "theorem r = (fun (x : Int) -> 1 / x) == (fun (x : Int) -> 1 / x) proof\n"
  ++ "  axiom refl_eq at 0 on (fun (x : Int) -> 1 / x) == (fun (x : Int) -> 1 / x) end\nin 0";

/* Statics' own type information for each lambda of an old unannotated
   funext example: the lambda's synthesized type and the parameter-type
   field the elaboration stores. Printed only. */
let type_info_receipt = (label, src) => {
  let (statics, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      Test_Evaluator_Prelude.parse_exp(src),
    );
  let user = Test_Evaluator_Prelude.parse_exp(src);
  let show_ty = t => Typ.pretty_print(t);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Fun(p, _, _, _) =>
            print_endline(
              "CORR typeinfo "
              ++ label
              ++ " lambda "
              ++ RE.print_exp(e)
              ++ " : "
              ++ (
                Statics.Map.ty_of(Exp.rep_id(e), statics)
                |> Option.map(show_ty)
                |> Option.value(~default="<no info>")
              )
              ++ " | param "
              ++ (
                Statics.Map.ty_of(Pat.rep_id(p), statics)
                |> Option.map(show_ty)
                |> Option.value(~default="<no info>")
              ),
            )
          | _ => ()
          };
          continue(e);
        },
      user,
    );
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Fun(_, _, field, _) =>
            print_endline(
              "CORR typeinfo "
              ++ label
              ++ " elaborated Fun type field: "
              ++ (
                field |> Option.map(show_ty) |> Option.value(~default="None")
              ),
            )
          | _ => ()
          };
          continue(e);
        },
      elab,
    );
  ();
};

let tests = (
  "ResetStandardnessCorrections",
  [
    test_case("C1 negative: higher-order funext on a fresh (post-witness) goal is refused", `Quick, () => {
      let o = RE.observe("ho_fresh", ho_fresh);
      RE.valid("ho_fresh", o);
      check(list(string), "funext-domain mark", ["funext-domain"], funext_refusals(o, "t"));
    }),
    test_case("C1 positive: known first-order fresh goal is allowed", `Quick, () => {
      let o = RE.observe("fo_fresh", fo_fresh);
      RE.valid("fo_fresh", o);
      check(list(string), "no funext refusal", [], funext_refusals(o, "t"));
    }),
    test_case("C2 precondition: the lemma forall (f : Int -> Int) -> !!f is Proven, statics clean", `Quick, () => {
      let o = RE.observe("bad_instance", bad_instance);
      RE.valid("bad_instance", o);
      check(string, "lemma", "Proven", RE.full(o, "lemma"));
    }),
    test_case("C2 negative: instantiating it at fun x -> 1 / x does not prove !!(partial lambda)", `Quick, () => {
      let o = RE.observe("bad_instance", bad_instance);
      RE.valid("bad_instance", o);
      check(bool, "bad not Proven", false, RE.full(o, "bad") == "Proven");
      print_endline(
        "CORR runtime (fun x -> 1 / x)(0) ~> "
        ++ RE.eval_print("(fun (x : Int) -> 1 / x)(0)"),
      );
    }),
    test_case("C2 positive: identity lambda instance is Proven", `Quick, () => {
      let o = RE.observe("good_instance", good_instance);
      RE.valid("good_instance", o);
      check(string, "good", "Proven", RE.full(o, "good"));
    }),
    test_case("D81: primitive reflexivity at a partial lambda stays Proven", `Quick, () => {
      let o = RE.observe("refl_partial", refl_partial);
      RE.valid("refl_partial", o);
      check(string, "refl", "Proven", RE.full(o, "r"));
    }),
    test_case("receipt: statics type info of the old unannotated funext examples", `Quick, () => {
      List.iter(
        ((l, s)) => type_info_receipt(l, s),
        [
          ("commute", "theorem t = (fun x -> x + 1) == (fun x -> 1 + x) proof ? in t"),
          ("inv", "let inv = fun x -> 1 / x in theorem t = inv == inv proof ? in t"),
          ("annotated", "theorem t = (fun (x : Int) -> x + 1) == (fun (x : Int) -> 1 + x) proof ? in t"),
        ],
      )
    }),
  ],
);
