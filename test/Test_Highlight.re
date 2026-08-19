open Alcotest;
open Web;
open Haz3lcore;

/* `Highlight` turns a term id into the SVG outline the editor draws around it,
 * and it was at 0% coverage. It is not cosmetic code: `of_tile` enforces the
 * structural invariant that a tile with N children has N+1 shards, and
 * `of_projector` requires a measurement, and both `failwith` when the invariant
 * does not hold. That aborts the render, so the user sees a blank or broken
 * editor rather than a slightly wrong outline.
 *
 * I hit `Highlight.of_tile: shard mismatch` in the browser while working on a
 * refractor and spent a while blaming the wrong layer for it. This is the test
 * that would have located it: highlight every id in a program and require that
 * nothing raises. Cheap, and it covers the shapes that actually break -- tiles
 * with several children, and segments containing a refractor. */

let font_metrics = FontMetrics.init;

let syntax_of = (text: string): CachedSyntax.t => {
  let model =
    switch (Parser.to_zipper(~root=Sort.Exp, text)) {
    | None => Alcotest.fail("could not parse: " ++ text)
    | Some(z) =>
      Editor.Model.mk(z, ~root=Sort.Exp) |> CodeWithStatics.Model.mk
    };
  CodeWithStatics.Update.calculate(
    ~settings=Settings.Model.init.core,
    ~is_edited=true,
    ~stitch=x => x,
    ~dynamics=model.dynamics,
    ~is_dynamic_term=false,
    model,
  ).
    editor.
    syntax;
};

/* Every id the editor could ask to highlight. */
let ids_of = (syntax: CachedSyntax.t) => syntax.segment |> Segment.ids;

/* Highlight every id and report the first that raises. `color` returning [] for
   an id with no segment is fine; raising is not. */
let highlight_all = (text: string) => {
  let syntax = syntax_of(text);
  let ids = ids_of(syntax);
  check(
    bool,
    "the program has ids to highlight",
    true,
    List.length(ids) > 0,
  );
  List.iter(
    id =>
      switch (Highlight.color(~syntax, ~font_metrics, ["test"], id)) {
      | _ => ()
      | exception exn =>
        failf(
          "highlighting an id of %s raised: %s",
          text,
          Printexc.to_string(exn),
        )
      },
    ids,
  );
};

/* Shapes chosen for their shard/child structure, which is what `of_tile`
   counts: multi-child delimited forms, nesting, and a refractor. */
let programs = [
  ("simple binop", {|1 + 1|}),
  ("let", {|let x = 1 in x|}),
  ("function", {|fun x -> x + 1|}),
  ("if/then/else", {|if true then 1 else 2|}),
  ("case with arms", {|case 1 | 1 => "a" | _ => "b" end|}),
  (
    "nested case in let",
    {|let f = fun x -> case x | 0 => 0 | n => n end in f(1)|},
  ),
  ("tuple and list", {|([1, 2, 3], (1, 2))|}),
  ("multiline", {|let x = 1 in
let y = 2 in
x + y|}),
  ("string with delimiters", {|"a (b) [c]"|}),
  ("empty hole", {|1 + ?|}),
  /* The case that crashed in the browser: a segment containing a refractor. */
  ("refractor", {|^^probe(1 + 1)|}),
  ("refractor in a let body", {|let x = 1 in ^^probe(x + 1)|}),
];

let tests = (
  "Highlight",
  List.map(
    ((name, program)) =>
      test_case("highlights every id: " ++ name, `Quick, () =>
        highlight_all(program)
      ),
    programs,
  ),
);
