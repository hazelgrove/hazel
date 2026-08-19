open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* HtmlRenderer is the value-side counterpart to HTMLProj, and mirrors its two
 * modes: static HTML, and a running (init, update, view, subs) app. `parse`
 * is the whole of its contract — what it returns is what puts "View as html"
 * in the sample context menu, what ^^probe_html renders, and which of the two
 * commit targets the rendering uses. */

let parse = (program: string): option(Haz3lcore.HtmlRenderer.value) =>
  Haz3lcore.HtmlRenderer.parse(Sort.Exp, parse_and_evaluate(program));

let kind = (program: string): string =>
  switch (parse(program)) {
  | Some(Static(_)) => "static"
  | Some(App(_)) => "app"
  | None => "rejected"
  };

let noop_app = {|(0, fun (m, _) -> (m, CmdNone), fun m -> Int(m), fun _ -> SubNone)|};

let tests = (
  "HtmlRenderer",
  [
    test_case(
      "recognizes static HTML",
      `Quick,
      () => {
        check(
          string,
          "a literal element",
          "static",
          kind({|Div([], [Text("hi")])|}),
        );
        check(string, "a nullary element", "static", kind({|Br|}));
        check(
          string,
          "a generic SVG node",
          "static",
          kind({|Node("svg", [Create("viewBox", "0 0 1 1")], [])|}),
        );
      },
    ),
    /* The point of the value seam: HTMLProj can only draw syntax that is
       already HTML or a literal 4-tuple, so a computed one shows as code
       there. Here the application has been evaluated before parse sees it. */
    test_case("recognizes HTML that was computed, not written", `Quick, () =>
      check(
        string,
        "an application returning HTML",
        "static",
        kind({|let f = fun n -> Div([], [Int(n)]) in f(3)|}),
      )
    ),
    test_case(
      "recognizes an MVU app",
      `Quick,
      () => {
        check(string, "a literal 4-tuple", "app", kind(noop_app));
        check(
          string,
          "the labeled form",
          "app",
          kind(
            {|(init=0, update=fun (m, _) -> (m, CmdNone),
             view=fun m -> Int(m), subs=fun _ -> SubNone)|},
          ),
        );
      },
    ),
    /* The case this whole seam exists for: an app produced by a function has
       no 4-tuple in its syntax, so only the evaluated value can reveal it. */
    test_case("recognizes an app that was computed", `Quick, () =>
      check(
        string,
        "an application returning an app",
        "app",
        kind(
          {|let mk = fun n -> (n, fun (m, _) -> (m, CmdNone),
                                  fun m -> Int(m), fun _ -> SubNone) in mk(7)|},
        ),
      )
    ),
    /* Syntax commit splices `f(html)` rather than evaluating the handler,
       so what stands in for `f` matters: the binding name keeps the edit
       short and keeps it pointing at the definition. */
    test_case("a let-bound handler carries its name", `Quick, () =>
      check(
        option(string),
        "named bump",
        Some("bump"),
        Haz3lcore.HtmlRenderer.handler_name(
          parse_and_evaluate(
            {|let bump = fun node -> Div([], [node]) in bump|},
          ),
        ),
      )
    ),
    test_case("an inline lambda has no name to splice", `Quick, () =>
      check(
        option(string),
        "anonymous",
        None,
        Haz3lcore.HtmlRenderer.handler_name(
          parse_and_evaluate({|fun node -> Div([], [node])|}),
        ),
      )
    ),
    test_case(
      "rejects values that are neither",
      `Quick,
      () => {
        check(string, "a number", "rejected", kind({|1 + 1|}));
        check(string, "a list", "rejected", kind({|[1, 2, 3]|}));
        check(string, "a string", "rejected", kind({|"Div"|}));
        check(string, "another ADT", "rejected", kind({|Some(3)|}));
        check(string, "a function", "rejected", kind({|fun x -> x|}));
        /* A 4-tuple whose update/view are not functions is just a tuple. */
        check(string, "a plain 4-tuple", "rejected", kind({|(1, 2, 3, 4)|}));
      },
    ),
  ],
);
