open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Js_of_ocaml;

module C = Language.BuiltinsADT.Color;

/* A picker over the canonical `Oklch(l, c, h)` literal.

   The Colors config slide is built from these literals, and `Atom.to_literal`
   prints floats with six decimals, so the raw form reads
   `Oklch(25.000000, 0.015000, 240.000000)` — unreadable across 38 seeds. The
   slide ships every literal pre-wrapped in this projector, the same trick the
   Shortcuts slide plays with keybinding widgets. The underlying term is
   untouched, so removing a projector reveals the same literal and statics are
   unaffected.

   Closed it is a swatch one character wide; clicking opens a panel with a
   lightness × chroma plane and a hue strip, both drag-editable, plus a text
   field that reads and accepts hex/rgb as well as oklch.

   Why not <input type="color">: that control is sRGB hex, so round-tripping
   through it would quantise every seed and silently discard the wide-gamut
   chroma the palette is written in. Editing stays in OKLCH; RGB is an entry
   and display format, converted at the boundary.

   Why no canvas: the plane is a stack of thin divs, each a horizontal
   `oklch()` gradient at its own lightness, so the BROWSER interpolates in
   OKLCH and the colours are exact. That needs no draw hook, no mount
   callback, and no imperative redraw — it is just the model rendered. */
module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Oklch
    | Rgb;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type target =
    | Plane
    | Hue;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    open_: bool,
    mode,
    /* Held while dragging: the view reads THIS instead of the syntax, and it
       is written to the syntax once, on pointerup. `SetSyntax` mints a fresh
       projector id, so committing on every mousemove would recreate the
       widget mid-gesture and drop the drag. */
    preview: option((float, float, float)),
    dragging: option(target),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Toggle
    | SetMode(mode)
    | Grab(target, float, float)
    | Move(float, float)
    | Release;

  let init_model = {
    open_: false,
    mode: Oklch,
    preview: None,
    dragging: None,
  };

  /* Chroma axis. 0.4 is past the sRGB boundary at most hues, so the right
     edge of the plane is out of gamut and renders clamped — which is the
     honest thing to show, since the palette itself is not gamut-limited. */
  let max_chroma = 0.4;
  /* The plane is drawn as horizontal strips, so this is its vertical
     resolution; too few and the lightness axis bands visibly. Only an OPEN
     picker renders them, and at most a handful are open at once. */
  let strips = 56;

  /* --- reading and writing the literal --- */

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
    | Some(_) => Some(init_model)
    | None => None
    };

  let get = (info: info): (float, float, float) =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(oklch_of)
    ) {
    | Some(t) => t
    | None => failwith("Color: Get: not an Oklch literal")
    };

  /* What the widget is currently showing: mid-drag that is the preview, and
     otherwise whatever the syntax says. */
  let showing = (model: model, info: info) =>
    switch (model.preview) {
    | Some(t) => t
    | None => get(info)
    };

  /* Rewrite the three components in place, preserving every id: the
     constructor and the tuple are the same nodes, only the leaves change. */
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

  /* --- text formats --- */

  let round_to = (places: int, x: float) => {
    let m = 10. ** float_of_int(places);
    Float.round(x *. m) /. m;
  };

  let text_of = (mode: mode, (l, c, h)) =>
    switch (mode) {
    | Rgb => C.hex_of_oklch((l, c, h))
    | Oklch =>
      C.to_css(Oklch(round_to(2, l), round_to(4, c), round_to(2, h)))
    };

  /* Accepts either format in either mode: someone pasting a hex code into the
     OKLCH field means the colour, not a syntax error. */
  let parse_text = (s: string): option((float, float, float)) => {
    let s = String.trim(s);
    let inside_oklch =
      switch (String.index_opt(s, '('), String.rindex_opt(s, ')')) {
      | (Some(i), Some(j)) when j > i && String.sub(s, 0, i) == "oklch" =>
        Some(String.sub(s, i + 1, j - i - 1))
      | _ => None
      };
    switch (inside_oklch) {
    | Some(body) =>
      let num = t => {
        let t = String.trim(t);
        let t =
          String.length(t) > 0 && t.[String.length(t) - 1] == '%'
            ? String.sub(t, 0, String.length(t) - 1) : t;
        float_of_string_opt(t);
      };
      switch (
        String.split_on_char(' ', body)
        |> List.filter(t => String.trim(t) != "")
        |> List.map(num)
      ) {
      | [Some(l), Some(c), Some(h), ..._] => Some((l, c, h))
      | _ => None
      };
    | None => C.oklch_of_css(s)
    };
  };

  /* --- geometry --- */

  /* Fraction of the way across and down the element the event landed on.
     Deliberately un-annotated so it accepts both pointer and mouse events —
     the drag needs pointerdown/pointerup for capture and mousemove for the
     movement, and js_of_ocaml gives those distinct object types. */
  let fractions = (e): (float, float) => {
    let target = e##.currentTarget |> Js.Opt.get(_, _ => failwith("target"));
    let r = target##getBoundingClientRect;
    let clamp = x => Float.min(1., Float.max(0., x));
    let w = r##.right -. r##.left;
    let h = r##.bottom -. r##.top;
    (
      clamp((float_of_int(e##.clientX) -. r##.left) /. Float.max(1., w)),
      clamp((float_of_int(e##.clientY) -. r##.top) /. Float.max(1., h)),
    );
  };

  let apply = (t: target, (fx, fy), (l, c, h)) =>
    switch (t) {
    | Plane => (100. *. (1. -. fy), max_chroma *. fx, h)
    | Hue => (l, c, 360. *. fx)
    };

  let update = (model: model, info: info, action: action) =>
    switch (action) {
    | Toggle => {
        ...model,
        open_: !model.open_,
        preview: None,
        dragging: None,
      }
    | SetMode(mode) => {
        ...model,
        mode,
      }
    | Grab(t, fx, fy) => {
        ...model,
        dragging: Some(t),
        preview: Some(apply(t, (fx, fy), showing(model, info))),
      }
    | Move(fx, fy) =>
      switch (model.dragging) {
      | None => model
      | Some(t) => {
          ...model,
          preview: Some(apply(t, (fx, fy), showing(model, info))),
        }
      }
    | Release => {
        ...model,
        dragging: None,
        preview: None,
      }
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;

  /* Closed, the swatch is one column. Open, the panel needs real estate, so
     the shape reserves it rather than overlapping the code beneath. */
  let placeholder = (model, _) =>
    model.open_
      ? ProjectorCore.Shape.{
          horizontal: 26,
          vertical: Block(8),
        }
      : ProjectorCore.Shape.inline(2);

  let css_of = ((l, c, h)) => C.to_css(C.Oklch(l, c, h));

  let view = ({model, info, local, parent, _}: View.args(model, action)) => {
    let (l, c, h) as current = showing(model, info);
    let swatch = (~extra=[], ()) =>
      Node.div(
        ~attrs=[
          Attr.classes(["cp-swatch", ...extra]),
          Attr.create("style", "background-color: " ++ css_of(current)),
        ],
        [],
      );

    if (!model.open_) {
      View.mk(
        Node.div(
          ~attrs=[
            Attr.classes(["cp-closed"]),
            Attr.on_pointerdown(_ =>
              Effect.Many([local(Toggle), Effect.Stop_propagation])
            ),
          ],
          [swatch()],
        ),
      );
    } else {
      /* Pointer capture keeps the gesture alive when it leaves the element,
         which is what makes the plane feel like a colour picker rather than a
         set of steppers. */
      let grab = (t, e: Js.t(Dom_html.pointerEvent)) => {
        let target =
          e##.currentTarget |> Js.Opt.get(_, _ => failwith("target"));
        JsUtil.setPointerCapture(target, e##.pointerId);
        let (fx, fy) = fractions(e);
        Effect.Many([local(Grab(t, fx, fy)), Effect.Stop_propagation]);
      };
      let move = (e: Js.t(Dom_html.mouseEvent)) =>
        switch (model.dragging) {
        | None => Effect.Ignore
        | Some(_) =>
          let (fx, fy) = fractions(e);
          local(Move(fx, fy));
        };
      let release = (_e: Js.t(Dom_html.pointerEvent)) =>
        switch (model.preview) {
        | None => local(Release)
        | Some(t) =>
          Effect.Many([parent(SetSyntax(put(info, t))), local(Release)])
        };
      let drag_attrs = t => [
        Attr.on_pointerdown(grab(t)),
        Attr.on_mousemove(move),
        Attr.on_pointerup(release),
      ];

      /* Each strip is one lightness, gradient-ed across chroma. The browser
         interpolates in OKLCH, so the plane is exact rather than an sRGB
         approximation of it. */
      let plane_strip = i => {
        let l =
          100. *. (1. -. (float_of_int(i) +. 0.5) /. float_of_int(strips));
        Node.div(
          ~attrs=[
            Attr.classes(["cp-strip"]),
            Attr.create(
              "style",
              "background: linear-gradient(to right, "
              ++ css_of((l, 0., h))
              ++ ", "
              ++ css_of((l, max_chroma, h))
              ++ ")",
            ),
          ],
          [],
        );
      };
      let pct = x => Printf.sprintf("%.2f%%", x *. 100.);
      let plane =
        Node.div(
          ~attrs=[Attr.classes(["cp-plane"]), ...drag_attrs(Plane)],
          List.init(strips, plane_strip)
          @ [
            Node.div(
              ~attrs=[
                Attr.classes(["cp-dot"]),
                Attr.create(
                  "style",
                  "left: "
                  ++ pct(c /. max_chroma)
                  ++ "; top: "
                  ++ pct(1. -. l /. 100.),
                ),
              ],
              [],
            ),
          ],
        );
      let hue_bar =
        Node.div(
          ~attrs=[
            Attr.classes(["cp-hue"]),
            Attr.create(
              "style",
              "background: linear-gradient(to right, "
              ++ String.concat(
                   ", ",
                   List.init(13, i =>
                     css_of((70., 0.18, float_of_int(i) *. 30.))
                   ),
                 )
              ++ ")",
            ),
            ...drag_attrs(Hue),
          ],
          [
            Node.div(
              ~attrs=[
                Attr.classes(["cp-dot", "cp-dot-hue"]),
                Attr.create("style", "left: " ++ pct(h /. 360.)),
              ],
              [],
            ),
          ],
        );
      let tab = (m, text) =>
        Node.div(
          ~attrs=[
            Attr.classes(["cp-tab", ...model.mode == m ? ["on"] : []]),
            Attr.on_pointerdown(_ =>
              Effect.Many([local(SetMode(m)), Effect.Stop_propagation])
            ),
          ],
          [Node.text(text)],
        );
      let entry =
        Node.input(
          ~attrs=[
            Attr.classes(["cp-text"]),
            Attr.string_property("value", text_of(model.mode, current)),
            Attr.on_change((_, v) =>
              switch (parse_text(v)) {
              | Some(t) => parent(SetSyntax(put(info, t)))
              | None => Effect.Ignore
              }
            ),
            /* The editor treats keystrokes as edits; the text field needs
               them. */
            Attr.on_keydown(_ => Effect.Stop_propagation),
          ],
          (),
        );
      View.mk(
        Node.div(
          ~attrs=[
            Attr.classes(["cp-panel"]),
            /* A click inside the panel must not move the editor caret. */
            Attr.on_pointerdown(_ => Effect.Stop_propagation),
          ],
          [
            plane,
            hue_bar,
            Node.div(
              ~attrs=[Attr.classes(["cp-row"])],
              [
                Node.div(
                  ~attrs=[
                    Attr.classes(["cp-close"]),
                    Attr.on_pointerdown(_ =>
                      Effect.Many([local(Toggle), Effect.Stop_propagation])
                    ),
                  ],
                  [swatch(~extra=["cp-swatch-lg"], ())],
                ),
                tab(Oklch, "oklch"),
                tab(Rgb, "rgb"),
                entry,
              ],
            ),
          ],
        ),
      );
    };
  };
};
