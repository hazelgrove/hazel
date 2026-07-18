open Alcotest;
open Haz3lcore;
open Util;

let mk_zipper = (code: string): Zipper.t => {
  switch (Parser.to_zipper(~root=Exp, code)) {
  | Some(z) => z
  | None => Alcotest.fail("Failed to parse: " ++ code)
  };
};

let mk_root_and_syntax = (code: string) => {
  let z = mk_zipper(code);
  let root = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  (root, CachedSyntax.init(z));
};

let start = (~cursor, ~selector, code) => {
  let (root, syntax) = mk_root_and_syntax(code);
  switch (SelectorFind.start(~selector, ~root, ~caret_point=cursor, ~syntax)) {
  | Ok(session) => session
  | Error(msg) => Alcotest.fail(msg)
  };
};

let active_text = (session: SelectorFind.session): string =>
  switch (SelectorFind.print_active(session)) {
  | Some(text) => text
  | None => Alcotest.fail("Expected active selector match")
  };

let code = "let a = 1 in let b = 2 in let c = 3 in a + b + c";
let selector = "let _ = %";

let tests = (
  "SelectorFind",
  [
    test_case(
      "starts at first match after cursor",
      `Quick,
      () => {
        let session = start(~cursor=Point.zero, ~selector, code);
        check(string, "active", "1", active_text(session));
        check(string, "summary", "1 of 3", SelectorFind.summary(session));
      },
    ),
    test_case(
      "strictly starts after current match",
      `Quick,
      () => {
        let (root, syntax) = mk_root_and_syntax(code);
        let matches =
          Selector.query(selector, root)
          |> SelectorFind.sort_by_position(_, syntax);
        let first = List.hd(matches);
        let first_start =
          SelectorFind.measure_start(
            first.focused_id,
            syntax.term_data,
            syntax.measured,
          )
          |> Option.get;
        let session =
          switch (
            SelectorFind.start(
              ~selector,
              ~root,
              ~caret_point=first_start,
              ~syntax,
            )
          ) {
          | Ok(session) => session
          | Error(msg) => Alcotest.fail(msg)
          };
        check(string, "active", "2", active_text(session));
      },
    ),
    test_case(
      "wraps when cursor is after all matches",
      `Quick,
      () => {
        let session =
          start(~cursor=Point.mk(~row=999, ~col=0), ~selector, code);
        check(string, "active", "1", active_text(session));
      },
    ),
    test_case(
      "cycles next and previous with wrap",
      `Quick,
      () => {
        let session = start(~cursor=Point.zero, ~selector, code);
        let session = SelectorFind.next(session);
        check(string, "next", "2", active_text(session));
        let session = SelectorFind.prev(session);
        check(string, "prev", "1", active_text(session));
        let session = SelectorFind.prev(session);
        check(string, "wrap prev", "3", active_text(session));
      },
    ),
    test_case(
      "cycling preserves original search anchor",
      `Quick,
      () => {
        let session = start(~cursor=Point.zero, ~selector, code);
        let session = SelectorFind.next(session);
        check(
          bool,
          "anchor unchanged",
          true,
          Point.equals(Point.zero, session.anchor_point),
        );
      },
    ),
  ],
);
