open Alcotest;
open Language;
open Test_Evaluator_Prelude;
open Test_ProofMap.Harness;

/* The prover's documentation slides tell the reader what each theorem
 * on them shows. These tests hold the checker to what the slides say. */

let read_slide = (file: string): string => {
  let path = Filename.concat("hazel-programs/docs/reference", file);
  let path =
    List.find(
      ~f=Stdlib.Sys.file_exists,
      [path, Filename.concat("../../..", path)],
    )
    |> Option.value(~default=path);
  let ic = Stdlib.open_in_bin(path);
  let text = Stdlib.really_input_string(ic, Stdlib.in_channel_length(ic));
  Stdlib.close_in(ic);
  text;
};

let status = (pm: ProofMap.t, proof: Proof.t): string =>
  Web.Obligations.Status.(label(of_proof(pm, proof)));

let theorem_names = (e: Exp.t): list(string) => {
  let names = ref([]);
  let f_exp = (continue, e: Exp.t) => {
    switch (e.term) {
    | Theorem(p, _, _, _) =>
      names := Option.to_list(pat_var_name(p)) @ names^
    | _ => ()
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, e);
  List.rev(names^);
};

/* Every theorem on the slide, in order, with the status the slide's
 * text gives it. */
let slide = (file, expected: list((string, string))) =>
  test_case(
    file,
    `Quick,
    () => {
      let (state, _, elab) =
        read_slide(file) |> parse_exp |> eval_with_proof;
      let pm = EvaluatorState.get_proof_map(state);
      check(
        list(string),
        "the theorems",
        List.map(~f=fst, expected),
        theorem_names(elab),
      );
      List.iter(
        ~f=
          ((name, want)) =>
            check(
              string,
              name,
              want,
              status(pm, proof_of_named(name, elab)),
            ),
        expected,
      );
    },
  );

/* `div_ok`'s `rewrite refl` at `8 / z` incurs `z != 0`, which the
 * slide's `have nz` discharges. */
let test_div_ok_discharges_by_nz = () => {
  let (state, _, elab) =
    read_slide("prover-side-conditions.hz") |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let discharged_by = (ob: Obligation.t) =>
    switch (Obligation.remote_fact(ob)) {
    | Some((name, _)) => name
    | None => Obligation.show_discharge(ob.discharge)
    };
  check(
    list(string),
    "its side conditions and what discharges them",
    ["z > 0 by pos", "z != 0 by nz"],
    ProofMap.obligations_of_proof(pm, proof_of_named("div_ok", elab))
    |> List.map(~f=(ob: Obligation.t) =>
         print_exp(ob.goal) ++ " by " ++ discharged_by(ob)
       ),
  );
};

let tests = (
  "ProverDocs",
  [
    test_case(
      "div_ok's z != 0 is discharged by nz",
      `Quick,
      test_div_ok_discharges_by_nz,
    ),
    slide(
      "prover-theorems.hz",
      [("six", "proven"), ("seven", "refuted"), ("eight", "incomplete")],
    ),
    slide("prover-stepper.hz", [("try_it", "incomplete")]),
    slide(
      "prover-lemmas.hz",
      [("rev_one", "proven"), ("rev_two", "proven")],
    ),
    slide("prover-induction.hz", [("app_nil", "proven")]),
    slide(
      "prover-side-conditions.hz",
      [("div_open", "incomplete · 1 open"), ("div_ok", "proven")],
    ),
    slide(
      "prover-reference.hz",
      [
        ("app_nil", "proven"),
        ("app_nil_fun", "proven"),
        ("shuffle", "proven"),
        ("acc", "proven"),
        ("div_pos", "proven"),
        ("div_neg", "proven"),
        ("sum_three", "proven"),
        ("sign_pos", "proven"),
        ("sign_le", "proven"),
      ],
    ),
  ],
);
