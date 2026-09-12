/* Hazel's instantiation of the Fumola printer.

   FumolaPrint is generic in the host term a Fumola program can embed: it takes
   a function that renders one as Fumola source, and knows nothing about Hazel.
   This is where the two are joined, so every caller in Hazel gets one entry
   point rather than passing the bridge around.

   The bridge is FumolaSource, which renders a Hazel *value* as Fumola source.
   That is the same crossing the livelit made when it prefixed a program with
   `let input = …`, and the same limit applies: only values with a written form
   can cross. What is new is where the crossing can happen. The livelit had one
   `input` slot at the boundary of an opaque string; `hazel … end` is a form of
   the Fumola grammar, so an embedded Hazel expression is a real tile subtree,
   with Hazel's statics and completion, and can stand anywhere a Fumola term
   can -- as many times as the program wants. */

/* A Hazel expression with no Fumola spelling reads as a hole, so has_hole
   refuses to print the program rather than the runtime refusing to parse it.
   The message FumolaSource produces says what could not be written; surfacing
   it in the editor is M2's job. */
let hazel_has_hole = (e: TermBase.Exp.t): bool =>
  switch (FumolaSource.of_exp(e)) {
  | Ok(_) => false
  | Error(_) => true
  };

/* Rendering runs after has_hole has said the program can be printed, so the
   error branch here is unreachable for a program Hazel agreed to send. It
   still has to produce something, and a hole is the honest something: it makes
   a printer used without the check fail at the Fumola parser rather than
   silently emit a different program. */
let hazel = (e: TermBase.Exp.t): string =>
  switch (FumolaSource.of_exp(e)) {
  | Ok(source) => source
  | Error(_) => FumolaPrint.hole(EmptyHole)
  };

let of_exp = (~explicit=false, e: FumolaTermBase.t): string =>
  FumolaPrint.of_exp(~hazel, ~explicit, e);

let program = (~explicit=false, ds: list(FumolaTermBase.dec)): string =>
  FumolaPrint.program(~hazel, ~explicit, ds);

let has_hole = (e: FumolaTermBase.t): bool =>
  FumolaPrint.has_hole(~hazel_has_hole, e);

/* What could not be written, for a program that has_hole refused. Returns the
   first such reason in the term, which is the one worth showing. */
let rec why_unprintable = (e: FumolaTermBase.t): option(string) =>
  switch (Annotated.term_of(e)) {
  | Hazel(h) =>
    switch (FumolaSource.of_exp(h)) {
    | Ok(_) => None
    | Error(message) => Some(message)
    }
  | _ => FumolaPrint.children(e) |> List.find_map(why_unprintable)
  };
