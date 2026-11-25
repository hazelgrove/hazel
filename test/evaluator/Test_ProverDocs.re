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

/* `rewrite or_true` at `8 / z > 0` incurs `z != 0`: open in
 * `div_open`, and discharged by the fact `nz` in `div_ok`. */
let test_division_conditions = () => {
  let (state, _, elab) =
    read_slide("prover-side-conditions.hz") |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let conditions = name =>
    ProofMap.obligations_of_proof(pm, proof_of_named(name, elab))
    |> List.map(~f=(ob: Obligation.t) =>
         print_exp(ob.goal)
         ++ " by "
         ++ (
           switch (Obligation.remote_fact(ob)) {
           | Some((fact, _)) => fact
           | None => Obligation.show_discharge(ob.discharge)
           }
         )
       );
  check(
    list(string),
    "div_open",
    ["z != 0 by Pending"],
    conditions("div_open"),
  );
  check(
    list(string),
    "div_ok",
    ["z > 0 by pos", "z != 0 by nz"],
    conditions("div_ok"),
  );
};

/* The standardness slide's `!!` circles: closed by evaluation, then
 * open. */
let test_standardness_conditions = () => {
  let (state, _, elab) =
    read_slide("prover-standardness.hz") |> parse_exp |> eval_with_proof;
  let pm = EvaluatorState.get_proof_map(state);
  let conditions = name =>
    ProofMap.obligations_of_proof(pm, proof_of_named(name, elab))
    |> List.map(~f=(ob: Obligation.t) =>
         print_exp(ob.goal) ++ " " ++ Obligation.show_discharge(ob.discharge)
       );
  check(
    list(string),
    "parse_ok",
    [{|!! int_of_string("3") Evaluated|}],
    conditions("parse_ok"),
  );
  check(
    list(string),
    "parse_open",
    [{|!! int_of_string("x") Pending|}],
    conditions("parse_open"),
  );
};

let tests = (
  "ProverDocs",
  [
    test_case(
      "the standardness slide's circles: closed, then open",
      `Quick,
      test_standardness_conditions,
    ),
    test_case(
      "the division slide's z != 0: open, then discharged by nz",
      `Quick,
      test_division_conditions,
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
      "prover-standardness.hz",
      [
        ("parse_ok", "proven"),
        ("parse_open", "incomplete · 1 open"),
        ("length_std", "proven"),
      ],
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
