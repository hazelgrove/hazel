open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

module C = Language.BuiltinsADT.Color;

/* A picker over the canonical `Oklch(l, c, h)` literal.

   The Colors config slide is built from these literals, and OCaml prints
   floats with six decimals — `Oklch(25.000000, 0.015000, 240.000000)` — which
   is unreadable across 38 seeds. Shipping every seed pre-wrapped in this
   projector means the slide opens as swatches instead, the same trick the
   Shortcuts slide plays with keybinding widgets. The underlying term is
   untouched, so removing the projector reveals the same literal and statics
   are unaffected.

   OKLCH rather than a native <input type="color">: that control is sRGB hex,
   so round-tripping through it would quantise every seed and silently discard
   the wide-gamut chroma the palette is written in. */
module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* Parens survive parsing, so look through them before matching. */
  let rec unparen = (e: Language.Exp.t): Language.Exp.t =>
    switch (e.term) {
    | Parens(inner) => unparen(inner)
    | _ => e
    };

  let floats_of = (e: Language.Exp.t): option((float, float, float)) =>
    switch (unparen(e).term) {
    | Ap(Forward, ctr, arg) =>
      switch (unparen(ctr).term, unparen(arg).term) {
      | (Constructor("Oklch", _), Tuple([l, c, h])) =>
        switch (unparen(l).term, unparen(c).term, unparen(h).term) {
        | (Atom(Float(l)), Atom(Float(c)), Atom(Float(h))) =>
          Some((l, c, h))
        | _ => None
        }
      | _ => None
      }
    | _ => None
    };

  let oklch_of = (any: Language.Any.t): option((float, float, float)) =>
    switch (any) {
    | Exp(e) => floats_of(e)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (oklch_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let get = (info: info): (float, float, float) =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(oklch_of)
    ) {
    | Some(t) => t
    | None => failwith("Color: Get: not an Oklch literal")
    };

  /* Rewrite the three components in place, preserving every id: the tuple and
     the constructor are the same nodes, only the leaves change. */
  let put = (info: info, (l, c, h): (float, float, float)): Base.segment => {
    let set = (e: Language.Exp.t, v): Language.Exp.t => {
      ...e,
      term: Atom(Float(v)),
    };
    let rewrite = (e: Language.Exp.t): Language.Exp.t =>
      switch (e.term) {
      | Ap(d, ctr, {term: Tuple([el, ec, eh]), _} as tup) => {
          ...e,
          term:
            Ap(
              d,
              ctr,
              {
                ...tup,
                term: Tuple([set(el, l), set(ec, c), set(eh, h)]),
              },
            ),
        }
      | _ => e
      };
    switch (
      info.utility.lift_syntax(
        ~inline=true,
        fun
        | Exp(e) => Exp(rewrite(e))
        | other => other,
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("Color: Put: lift failed")
    };
  };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;
  /* swatch plus three sliders */
  let placeholder = (_, _) => ProjectorCore.Shape.inline(22);
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;

  let view = ({info, parent, _}: View.args(model, action)) => {
    let (l, c, h) = get(info);
    let slider = (~min, ~max, ~step, value, of_string) =>
      WebUtil.range(
        ~attrs=[
          Attr.create("step", step),
          Attr.class_("color-slider"),
          Attr.on_input((_, v) =>
            parent(SetSyntax(put(info, of_string(float_of_string(v)))))
          ),
        ],
        ~min,
        ~max,
        value,
      );
    View.mk(
      Node.div(
        ~attrs=[Attr.class_("color-picker")],
        [
          Node.div(
            ~attrs=[
              Attr.class_("color-swatch"),
              Attr.create(
                "style",
                "background-color: " ++ C.to_css(C.Oklch(l, c, h)),
              ),
            ],
            [],
          ),
          slider(~min="0", ~max="100", ~step="1", string_of_float(l), l' =>
            (l', c, h)
          ),
          slider(~min="0", ~max="0.4", ~step="0.005", string_of_float(c), c' =>
            (l, c', h)
          ),
          slider(~min="0", ~max="360", ~step="1", string_of_float(h), h' =>
            (l, c, h')
          ),
        ],
      ),
    );
  };
};
