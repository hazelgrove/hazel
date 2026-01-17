open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* A simple sample picker projector for Strudel patterns.
 * Shows a dropdown of common drum/sample names.
 *
 * Applicable to: Sample(String) where String is a single sample name
 * (no mini-notation, no spaces, just a bare sample name like "bd" or "piano")
 *
 * Common samples from dirt-samples:
 * DRUMS: bd, sd, hh, oh, cp, rim, tom, cb, lt, mt, ht
 * MELODIC: piano, rhodes, bass, strings, pluck
 * FX: noise, metal */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* Sample categories and their samples */
  let drum_samples = [
    "bd",
    "sd",
    "hh",
    "oh",
    "cp",
    "rim",
    "tom",
    "cb",
    "lt",
    "mt",
    "ht",
  ];
  let melodic_samples = ["piano", "rhodes", "bass", "strings", "pluck"];
  let fx_samples = ["noise", "metal"];

  /* Validate that string is a simple sample name (no mini-notation) */
  let is_simple_sample = (s: string): bool => {
    let trimmed = String.trim(s);
    /* Must be non-empty, no spaces, no special chars */
    String.length(trimmed) > 0
    && !String.contains(trimmed, ' ')
    && !String.contains(trimmed, '*')
    && !String.contains(trimmed, '/')
    && !String.contains(trimmed, '[')
    && !String.contains(trimmed, ']')
    && !String.contains(trimmed, '<')
    && !String.contains(trimmed, '>')
    && !String.contains(trimmed, '~');
  };

  /* Extract string from Sample constructor application */
  let string_of = (any: Language.Any.t): option(string) =>
    switch (any) {
    | Exp({term: Ap(_, {term: Constructor("Sample", _), _}, arg), _}) =>
      switch (arg.term) {
      | Atom(String(s)) when is_simple_sample(s) => Some(s)
      | _ => None
      }
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (string_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let get = (info: info): string =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(string_of)
    ) {
    | Some(s) => s
    | None => ""
    };

  let put = (info: info, v: string): Base.segment =>
    switch (
      info.utility.lift_syntax(
        fun
        | Exp({term: Ap(dir, ctor, arg), _} as t) =>
          Exp({
            ...t,
            term:
              Ap(
                dir,
                ctor,
                {
                  ...arg,
                  term: Atom(String(v)),
                },
              ),
          })
        | _ => failwith("SamplePicker: Put: not Sample constructor"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("SamplePicker: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  /* Compact dropdown - 2 rows height */
  let placeholder = (_, _) => {
    ProjectorShape.horizontal: 12,
    vertical: Tab(2),
  };
  let update = (model, _, _) => model;

  /* Create a sample option button */
  let sample_option = (~current, ~parent, ~info, sample) => {
    let is_selected = current == sample;
    Node.button(
      ~attrs=[
        Attr.classes(["sample-option"] @ (is_selected ? ["selected"] : [])),
        Attr.on_click(_ => {parent(SetSyntax(put(info, sample)))}),
      ],
      [Node.text(sample)],
    );
  };

  let view = ({info, parent, _}: View.args(model, action)) => {
    let current = get(info);

    /* Group samples by category */
    let drum_section =
      Node.div(
        ~attrs=[Attr.classes(["sample-category"])],
        [
          Node.span(
            ~attrs=[Attr.classes(["category-label"])],
            [Node.text("Drums")],
          ),
          Node.div(
            ~attrs=[Attr.classes(["sample-options"])],
            List.map(sample_option(~current, ~parent, ~info), drum_samples),
          ),
        ],
      );

    let melodic_section =
      Node.div(
        ~attrs=[Attr.classes(["sample-category"])],
        [
          Node.span(
            ~attrs=[Attr.classes(["category-label"])],
            [Node.text("Melodic")],
          ),
          Node.div(
            ~attrs=[Attr.classes(["sample-options"])],
            List.map(
              sample_option(~current, ~parent, ~info),
              melodic_samples,
            ),
          ),
        ],
      );

    let fx_section =
      Node.div(
        ~attrs=[Attr.classes(["sample-category"])],
        [
          Node.span(
            ~attrs=[Attr.classes(["category-label"])],
            [Node.text("FX")],
          ),
          Node.div(
            ~attrs=[Attr.classes(["sample-options"])],
            List.map(sample_option(~current, ~parent, ~info), fx_samples),
          ),
        ],
      );

    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["sample-picker"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["current-sample"])],
            [Node.text(current)],
          ),
          drum_section,
          melodic_section,
          fx_section,
        ],
      ),
    );
  };
};
