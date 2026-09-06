open Alcotest;
open Haz3lcore;

/* Pat-rooted editors (modular-editors header cells): text enters
   through the REAL editing path (Parser.to_zipper inserts char by
   char, molding at Pat root), then MakeTerm.from_zip_for_pat reads
   the semantic pattern back out. */

let parse_pat = (s: string): option(Language.Pat.t) =>
  Parser.to_zipper(~root=Sort.Pat, s)
  |> Option.map(MakeTerm.from_zip_for_pat);

let shape = (p: Language.Pat.t): string =>
  switch (p.term) {
  | Var(x) => "var:" ++ x
  | Wild => "wild"
  | Asc({term: Var(x), _}, _) => "asc-var:" ++ x
  | Asc({term: Tuple(ps), _}, _) =>
    Printf.sprintf("asc-tuple:%d", List.length(ps))
  | Asc(_, _) => "asc:other"
  | Tuple(ps) => Printf.sprintf("tuple:%d", List.length(ps))
  | ListLit(ps) => Printf.sprintf("list:%d", List.length(ps))
  | EmptyHole => "hole"
  | _ => "other"
  };

let case_ = (name, src, expect) =>
  test_case(
    name,
    `Quick,
    () => {
      let got =
        switch (parse_pat(src)) {
        | Some(p) => shape(p)
        | None => "<no parse>"
        };
      check(string, src, expect, got);
    },
  );

let tests = (
  "PatRootEditor",
  [
    case_("bare var", "x", "var:x"),
    case_("wildcard", "_", "wild"),
    case_("ascribed var", "x : Int", "asc-var:x"),
    case_("ascribed tuple", "(a, b) : (Int, Int)", "asc:other"),
    case_("list pattern", "[x, y]", "list:2"),
  ],
);
