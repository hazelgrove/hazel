open Alcotest;
open Haz3lcore;

/* Several projectors assume their term is a literal of a particular type, and
 * say so with `failwith` rather than an error state:
 *
 *   CheckboxProj:  failwith("Checkbox: Get: not boolean literal")
 *   SliderProj:    failwith("Slider: Get: not integer literal")
 *   SliderFProj:   failwith("SliderF: Get: not float literal")
 *   TextAreaProj:  failwith("TextArea: get: Not string literal")
 *
 * Their `error` is `None`, and `view` calls `get`, so a mismatch is not a badge
 * or a fallback -- it aborts the render.
 *
 * The only thing standing between that and a crash-on-load is `init`: a
 * `^^check(...)` trigger in program text goes through `ProjectorPerform.init`
 * (via `Triggers.invoked_projector`), which returns None when the term is the
 * wrong shape, so the projector is simply not created. That guard is invisible
 * from the outside and easy to drop while refactoring, and dropping it means a
 * hand-written `.hz` or an older save takes the editor down on open.
 *
 * So this pins the guard in both directions: matching literals project, and
 * mismatched ones are declined rather than crashing. */

let projectors_of = (z: Zipper.t) =>
  MakeTerm.go(Zipper.zip(z)).projectors |> Id.Map.bindings;

let parse = (text: string): Zipper.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, text)) {
  | None => fail("could not parse: " ++ text)
  | Some(z) => z
  };

/* Parsing must not raise, and must not leave a projector behind, when the
   trigger sits on a term the projector cannot represent. */
let declines = (name, text) =>
  test_case("declines " ++ name, `Quick, () =>
    switch (parse(text)) {
    | z =>
      check(
        int,
        "no projector was created for " ++ text,
        0,
        List.length(projectors_of(z)),
      )
    | exception exn =>
      failf("parsing %s raised: %s", text, Printexc.to_string(exn))
    }
  );

let accepts = (name, text) =>
  test_case("accepts " ++ name, `Quick, () =>
    switch (parse(text)) {
    | z =>
      check(
        int,
        "a projector was created for " ++ text,
        1,
        List.length(projectors_of(z)),
      )
    | exception exn =>
      failf("parsing %s raised: %s", text, Printexc.to_string(exn))
    }
  );

let tests = (
  "ProjectorGuard",
  [
    /* Fixture guard: if the trigger syntax stops creating projectors at all,
       every "declines" test below would pass for the wrong reason. */
    accepts("check on a bool", {|^^check(true)|}),
    accepts("slider on an int", {|^^slider(42)|}),
    accepts("sliderf on a float", {|^^sliderf(1.5)|}),
    accepts("text on a string", {|^^text("hi")|}),
    /* The mismatches, one per failwith above. */
    declines("check on an int", {|^^check(1)|}),
    declines("check on a string", {|^^check("true")|}),
    declines("slider on a bool", {|^^slider(true)|}),
    declines("slider on a float", {|^^slider(1.5)|}),
    declines("sliderf on an int", {|^^sliderf(1)|}),
    declines("text on an int", {|^^text(1)|}),
    /* Non-literals are the case a hand-edited file is most likely to hit. */
    declines("check on a variable", {|let x = true in ^^check(x)|}),
    declines("slider on an expression", {|^^slider(1 + 1)|}),
    declines("check on a hole", {|^^check(?)|}),
  ],
);
