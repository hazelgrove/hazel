open Alcotest;
open Haz3lcore;

/* The color picker reads its literal through Parens and nested projectors,
   so it has to write back through them too: a picker that initialises but
   whose writes fall through leaves the syntax silently unchanged. */

module L = ColorProj.Literal;

let literal = testable(L.pp_literal, L.equal_literal);

let parse = (s: string): Language.Exp.t =>
  switch (Parser.to_term(s, ~root=Language.Sort.Exp)) {
  | Some(e) => e
  | None => fail("failed to parse " ++ s)
  };

let info_of_seg = (syntax: Segment.t): ProjectorBase.info => {
  id: Id.invalid,
  syntax,
  statics: None,
  dynamics: None,
  elaborated: None,
  utility: ProjectorInfo.utility,
};

let info_of_text = (s: string): ProjectorBase.info =>
  switch (Parser.to_segment(s, ~root=Exp)) {
  | Some(seg) => info_of_seg(seg)
  | None => fail("failed to parse " ++ s)
  };

let info_of_exp = (e: Language.Exp.t): ProjectorBase.info =>
  info_of_seg(ProjectorInfo.utility.term_to_seg(~inline=true, Exp(e)));

let term_of_seg = (seg: Segment.t): Language.Exp.t =>
  switch (ProjectorInfo.utility.seg_to_term(seg)) {
  | Some(Exp(e)) => e
  | _ => fail("segment is not an expression")
  };

let read = (seg: Segment.t): option(L.literal) =>
  L.of_any(Exp(term_of_seg(seg)));

let has_slider = (e: Language.Exp.t): bool => {
  let found = ref(false);
  let _ =
    Language.Exp.map_term(
      ~f_exp=
        (continue, e) =>
          switch (e.term) {
          | Projector({kind: Slider, _}, _) =>
            found := true;
            continue(e);
          | _ => continue(e)
          },
      e,
    );
  found^;
};

/* `Rgb(<slider 1>, 2, 3)`: a slider projector on the first component. */
let rgb_with_slider = (): Language.Exp.t => {
  let (module S) = ProjectorInit.to_module(Slider);
  let first = ref(true);
  Language.Exp.map_term(
    ~f_exp=
      (continue, e) =>
        switch (e.term) {
        | Atom(Int(_)) when first^ =>
          first := false;
          let model =
            switch (S.init(Exp(e))) {
            | Some(m) => m
            | None => fail("slider should init on an int")
            };
          {
            ...e,
            term:
              Projector(
                {
                  kind: Slider,
                  model,
                },
                e,
              ),
          };
        | _ => continue(e)
        },
    parse("Rgb(1, 2, 3)"),
  );
};

let writes_through = (text, ()) => {
  let info = info_of_text(text);
  check(
    option(literal),
    "reads " ++ text,
    Some(L.LRgb(1, 2, 3)),
    read(info.syntax),
  );
  check(
    option(literal),
    "write lands in " ++ text,
    Some(L.LRgb(10, 20, 30)),
    read(L.put(info, LRgb(10, 20, 30))),
  );
  check(
    option(literal),
    "kind change lands in " ++ text,
    Some(L.LOklch(50., 0.1, 120.)),
    read(L.put(info, LOklch(50., 0.1, 120.))),
  );
};

let slider_component = () => {
  let info = info_of_exp(rgb_with_slider());
  check(
    bool,
    "fixture has a slider",
    true,
    has_slider(term_of_seg(info.syntax)),
  );
  check(
    option(literal),
    "reads through the slider",
    Some(L.LRgb(1, 2, 3)),
    read(info.syntax),
  );
  let same_kind = L.put(info, LRgb(10, 20, 30));
  check(
    option(literal),
    "same-kind write lands",
    Some(L.LRgb(10, 20, 30)),
    read(same_kind),
  );
  check(
    bool,
    "same-kind write keeps the slider",
    true,
    has_slider(term_of_seg(same_kind)),
  );
  let other_kind = L.put(info, LOklch(50., 0.1, 120.));
  check(
    option(literal),
    "kind change lands",
    Some(L.LOklch(50., 0.1, 120.)),
    read(other_kind),
  );
};

let tests = [
  (
    "ColorProj.Literal",
    [
      test_case("bare literal", `Quick, writes_through("Rgb(1, 2, 3)")),
      test_case(
        "parenthesized literal",
        `Quick,
        writes_through("(Rgb(1, 2, 3))"),
      ),
      test_case(
        "parenthesized argument",
        `Quick,
        writes_through("Rgb((1, 2, 3))"),
      ),
      test_case(
        "parenthesized component",
        `Quick,
        writes_through("Rgb((1), 2, 3)"),
      ),
      test_case("slider on a component", `Quick, slider_component),
    ],
  ),
];
