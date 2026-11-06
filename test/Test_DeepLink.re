open Alcotest;
open Web;

/* Deep links, without a browser: the query is passed in as a lookup. */

let request = testable(DeepLink.pp_request, (==));
let action =
  testable(
    (f, a) => Format.pp_print_string(f, Haz3lcore.Action.show(a)),
    (==),
  );

let query = (params: list((string, string)), name) =>
  List.assoc_opt(name, params);
let parse = params => DeepLink.parse(query(params));
let pt = (row, col): Util.Point.t => {
  row,
  col,
};
let on_slide = (~position=?, slide): DeepLink.request => {
  place:
    Some({
      target: Documentation(slide),
      position,
    }),
  panel: None,
};

let parsing = [
  test_case("no parameters: nothing to do", `Quick, () =>
    check(request, "none", DeepLink.none, parse([]))
  ),
  test_case("an empty parameter counts as missing", `Quick, () =>
    check(
      request,
      "none",
      DeepLink.none,
      parse([("slide", ""), ("panel", ""), ("caret", "")]),
    )
  ),
  test_case("a slide, by name", `Quick, () =>
    check(
      request,
      "slide",
      on_slide("livelits-builtins"),
      parse([("slide", "livelits-builtins")]),
    )
  ),
  test_case("a caret: line and column, 1-based", `Quick, () =>
    check(
      request,
      "caret",
      on_slide(~position=Caret(pt(11, 3)), "intro"),
      parse([("slide", "intro"), ("caret", "12,4")]),
    )
  ),
  test_case("a caret without a column: the head of the line", `Quick, () =>
    check(
      request,
      "caret",
      on_slide(~position=Caret(pt(11, 0)), "intro"),
      parse([("slide", "intro"), ("caret", "12")]),
    )
  ),
  test_case("a selection between two points", `Quick, () =>
    check(
      request,
      "select",
      on_slide(
        ~position=
          Select({
            from: pt(11, 0),
            to_: pt(13, 19),
          }),
        "intro",
      ),
      parse([("slide", "intro"), ("select", "12,1-14,20")]),
    )
  ),
  test_case(
    "a selection of whole lines runs to the last one's end", `Quick, () =>
    check(
      request,
      "select",
      on_slide(
        ~position=
          Select({
            from: pt(11, 0),
            to_: pt(13, DeepLink.line_end - 1),
          }),
        "intro",
      ),
      parse([("slide", "intro"), ("select", "12-14")]),
    )
  ),
  test_case("a selection wins over a caret", `Quick, () =>
    check(
      request,
      "select",
      on_slide(
        ~position=
          Select({
            from: pt(0, 0),
            to_: pt(1, 1),
          }),
        "intro",
      ),
      parse([("slide", "intro"), ("caret", "5,5"), ("select", "1,1-2,2")]),
    )
  ),
  test_case("a caret with no slide or share is ignored", `Quick, () =>
    check(request, "none", DeepLink.none, parse([("caret", "12,4")]))
  ),
  test_case("a caret that is not numbers is ignored", `Quick, () =>
    check(
      request,
      "no position",
      on_slide("intro"),
      parse([
        ("slide", "intro"),
        ("caret", "twelve"),
        ("select", "1-2-3"),
      ]),
    )
  ),
  test_case("a share: a new scratchpad with this text", `Quick, () =>
    check(
      request,
      "share",
      {
        place:
          Some({
            target:
              Scratchpad({
                name: "Demo",
                text: "let x = 1 in x",
              }),
            position: None,
          }),
        panel: None,
      },
      parse([
        ("name", "Demo"),
        ("share", Util.StringUtil.compress("let x = 1 in x")),
      ]),
    )
  ),
  test_case("a share wins over a slide, and needs no name", `Quick, () =>
    check(
      request,
      "share",
      {
        place:
          Some({
            target:
              Scratchpad({
                name: "Unknown Share",
                text: "1",
              }),
            position: None,
          }),
        panel: None,
      },
      parse([("slide", "intro"), ("share", "1")]),
    )
  ),
  test_case(
    "a panel, by its short name or loosely",
    `Quick,
    () => {
      let panel = name => parse([("panel", name)]).panel;
      check(bool, "probes", true, panel("probes") == Some(Open(Probes)));
      check(bool, "Probes", true, panel("Probes") == Some(Open(Probes)));
      check(
        bool,
        "docs",
        true,
        panel("docs") == Some(Open(LanguageDocumentation)),
      );
      check(bool, "none", true, panel("none") == Some(Closed));
      check(bool, "closed", true, panel("closed") == Some(Closed));
      check(bool, "junk", true, panel("nonsense") == None);
    },
  ),
];

/* parse(to_params(r)) is r again, for requests whose slide is a slug (a
   slide's name goes into a link as its slug). */
let round_trips = {
  let back = (r: DeepLink.request) =>
    check(
      request,
      DeepLink.show_request(r),
      r,
      parse(DeepLink.to_params(r)),
    );
  [
    test_case(
      "every panel, and a closed sidebar",
      `Quick,
      () => {
        List.iter(
          p =>
            back({
              place: None,
              panel: Some(Open(p)),
            }),
          SidebarModel.Settings.all_of_panel,
        );
        back({
          place: None,
          panel: Some(Closed),
        });
      },
    ),
    test_case(
      "a slide with a caret, and with a selection",
      `Quick,
      () => {
        back(on_slide(~position=Caret(pt(3, 7)), "livelits-builtins"));
        back(
          on_slide(
            ~position=
              Select({
                from: pt(0, 0),
                to_: pt(9, 4),
              }),
            "intro",
          ),
        );
      },
    ),
    test_case("a share whose text needs escaping", `Quick, () =>
      back({
        place:
          Some({
            target:
              Scratchpad({
                name: "Two words & more",
                text: "let s = \"a b\" in\n  s ++ \"?=&#%\"",
              }),
            position: Some(Caret(pt(1, 2))),
          }),
        panel: Some(Open(Probes)),
      })
    ),
  ];
};

let applying = [
  test_case(
    "a slide's name matches loosely",
    `Quick,
    () => {
      let names = ["Introduction", "Livelits / Builtins"];
      let index = slide => DeepLink.slide_index(~slide, names);
      check(option(int), "slug", Some(1), index("livelits-builtins"));
      check(option(int), "squashed", Some(1), index("LivelitsBuiltins"));
      check(option(int), "title", Some(1), index("Livelits / Builtins"));
      check(option(int), "nothing", None, index("Livelits / Nothing"));
    },
  ),
  test_case(
    "a slide's slug",
    `Quick,
    () => {
      check(
        string,
        "title",
        "livelits-builtins",
        DeepLink.slug("Livelits / Builtins"),
      );
      check(string, "trailing", "what-next", DeepLink.slug("What next?"));
    },
  ),
  test_case(
    "the editor action for a position",
    `Quick,
    () => {
      check(
        option(action),
        "nothing",
        None,
        DeepLink.action(DeepLink.none),
      );
      check(
        option(action),
        "no position",
        None,
        DeepLink.action(on_slide("intro")),
      );
      check(
        option(action),
        "caret",
        Some(Move(Point(pt(1, 2), None))),
        DeepLink.action(on_slide(~position=Caret(pt(1, 2)), "intro")),
      );
      check(
        option(action),
        "select",
        Some(Select(PointToPoint((pt(1, 2), pt(3, 4))))),
        DeepLink.action(
          on_slide(
            ~position=
              Select({
                from: pt(1, 2),
                to_: pt(3, 4),
              }),
            "intro",
          ),
        ),
      );
    },
  ),
  test_case(
    "a caret where a selection's ends meet",
    `Quick,
    () => {
      check(
        bool,
        "caret",
        true,
        DeepLink.position_of_span((pt(2, 2), pt(2, 2))) == Caret(pt(2, 2)),
      );
      check(
        bool,
        "select",
        true,
        DeepLink.position_of_span((pt(2, 2), pt(3, 0)))
        == Select({
             from: pt(2, 2),
             to_: pt(3, 0),
           }),
      );
    },
  ),
  test_case(
    "the panel over the stored settings",
    `Quick,
    () => {
      let stored = Settings.Model.init;
      let sidebar = (panel: option(DeepLink.panel_request)) =>
        DeepLink.settings(
          {
            place: None,
            panel,
          },
          stored,
        ).
          sidebar;
      check(bool, "none: as stored", true, sidebar(None) == stored.sidebar);
      check(bool, "closed", false, sidebar(Some(Closed)).show);
      check(bool, "open", true, sidebar(Some(Open(Probes))).show);
      check(
        bool,
        "open: that panel",
        true,
        sidebar(Some(Open(Probes))).panel == Probes,
      );
    },
  ),
];

let tests = ("DeepLink", parsing @ round_trips @ applying);
