open Util;

/* Tools for writing a theorem's proof: see the open proof holes with
 * their goals, check a candidate proof against the prover, and write
 * one into the program. Proof syntax is in the `proofs` docs pack. */

let function_ = (~name, ~description, ~properties, ~required) =>
  `Assoc([
    ("type", `String("function")),
    (
      "function",
      `Assoc([
        ("name", `String(name)),
        ("description", `String(description)),
        (
          "parameters",
          `Assoc([
            ("type", `String("object")),
            ("properties", `Assoc(properties)),
            ("required", `List(List.map(s => `String(s), required))),
          ]),
        ),
      ]),
    ),
  ]);

let hole =
  `Assoc([
    ("type", `String("integer")),
    ("description", `String("The hole's number, from proof_holes")),
  ]);

let proof =
  `Assoc([
    ("type", `String("string")),
    (
      "description",
      `String(
        "Proof text to put in place of the hole, in the syntax of the `proofs` docs pack. It may leave `?` holes of its own.",
      ),
    ),
  ]);

let proof_holes: API.Json.t =
  function_(
    ~name="proof_holes",
    ~description=
      {|
Lists the unfinished proofs in the program: each `?` or `??` hole in a theorem's proof, numbered, with the theorem, the goal the prover hands that hole, and the facts in scope there (hypotheses and earlier theorems, with their statements). Holes written `??` are the ones the user asked you to fill.

Read the `proofs` docs pack (read_docs) for the proof syntax before writing a proof.
|},
    ~properties=[],
    ~required=[],
  );

let check_proof: API.Json.t =
  function_(
    ~name="check_proof",
    ~description=
      {|
Checks proof text in place of a hole without changing the program, and reports what the prover makes of it: the steps it marks (with the goal each step was given), induction cases and `?` holes left open (with their goals and hypotheses, such as `ih`), obligations left unproven, or that the proof proves the goal.

Use it as often as you like: build a proof a few steps at a time, leaving `?` for the rest, and read the goals it reports.
|},
    ~properties=[("hole", hole), ("proof", proof)],
    ~required=["hole", "proof"],
  );

let fill_proof: API.Json.t =
  function_(
    ~name="fill_proof",
    ~description=
      {|
Writes proof text in place of a hole in the program, and reports what the prover makes of it, as check_proof does. Check a proof with check_proof first; write it when it proves the goal, or when the user should see how far it got.
|},
    ~properties=[("hole", hole), ("proof", proof)],
    ~required=["hole", "proof"],
  );
