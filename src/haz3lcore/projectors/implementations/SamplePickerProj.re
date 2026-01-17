open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* A simple sample picker projector for Strudel patterns.
 * Shows a grid of common drum/sample names from dirt-samples.
 *
 * Applicable to: Sample(String) where String is a single sample name
 * (no mini-notation, no spaces, just a bare sample name like "bd" or "bass")
 *
 * Available samples from dirt-samples:
 * DRUMS: bd, sd, hh, cp, cb, lt, mt, ht
 * OTHER: bass, metal */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* All samples in a flat list for a compact grid layout */
  let all_samples = [
    "bd",
    "sd",
    "hh",
    "cp",
    "cb",
    "lt",
    "mt",
    "ht",
    "bass",
    "metal",
  ];

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
  /* 5x2 grid */
  let placeholder = (_, _) => {
    ProjectorShape.horizontal: 22,
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

    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["sample-picker"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["sample-grid"])],
            List.map(sample_option(~current, ~parent, ~info), all_samples),
          ),
        ],
      ),
    );
  };
};
