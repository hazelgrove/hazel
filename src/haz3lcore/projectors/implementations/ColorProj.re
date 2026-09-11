open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Js_of_ocaml;

module C = Language.BuiltinsADT.Color;

/* A picker over the two colour literals, `Oklch(l, c, h)` and `Rgb(r, g, b)`.
   Closed it is a one-character swatch; open, its surface follows the literal,
   because the two are not the same kind of thing. Oklch has unbounded chroma
   and describes colours sRGB cannot show, so its plane is lightness x chroma
   and runs past the gamut on purpose, rendering the excess clamped. Rgb is
   three bytes, all of them displayable, so its plane is the saturation x value
   square people expect and there is no clamped region to draw.

   Picking a tab CONVERTS the literal, so the tab means one thing throughout:
   the axes, the number, and the text on disk. Oklch -> Rgb is lossy and the
   tab says so; the reverse is exact.

   Not `<input type="color">`: it is sRGB hex, so an OKLCH literal round-tripped
   through it loses the wide-gamut chroma the palette is written in. And no
   canvas: each plane is one or two CSS gradients, so the browser interpolates
   and there is nothing to redraw imperatively. */
module M: Projector = {
  /* Read off the term, not held in the model, so the lit tab and the
     constructor on screen cannot disagree. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type form =
    | AsOklch
    | AsRgb;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type target =
    | Plane /* Oklch: lightness x chroma.  Rgb: saturation x value */
    | Hue;

  /* Whichever representation is being edited. Rgb mode holds HSV, not bytes,
     because that is what a saturation x value square addresses: drag value to
     the floor and the hue must still be there on the way back up, which is
     what `C.hsv_of_rgb`'s `~like` preserves. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type components =
    | Lch(float, float, float) /* l 0..100, chroma, hue 0..360 */
    | Hsv(float, float, float); /* hue 0..360, s 0..1, v 0..1 */

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    open_: bool,
    /* Feedback only; the syntax is written once, on pointerup, so a drag is
       one undoable edit rather than one per mousemove. */
    preview: option(components),
    /* A THROTTLE, not a correctness check: `move` sees this only after a
       render, so a mousemove cannot enqueue another preview until the last one
       is drawn. Each preview is a `SetModel` and `CodeEditable` recalculates
       on any projector action, so ungated it floods the queue and the drag
       stops moving at all. Being a render behind is the point. */
    dragging: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Toggle
    | Set(target, float, float)
    | Release;

  let init_model = {
    open_: false,
    preview: None,
    dragging: false,
  };

  /* The model is serialised into the saved slide, so a shape change meets
     sexps from older builds and the caller does not catch. May ONLY fall back,
     never edit: `Cook` round-trips through this on every read (view,
     placeholder, update, error), so a field cleared here is cleared every
     frame. Clearing `preview`/`dragging` here killed dragging outright. */
  let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
    switch (model_of_sexp(sexp)) {
    | exception _ => init_model
    | m => m
    };

  /* Which surface the press in progress began on. Outside the model because
     the model is a render behind; one slot suffices, the browser having one
     primary pointer. Without it the opening click sets a colour: its
     pointerdown lands on the closed swatch and its pointerup on the plane
     that just appeared under the pointer. */
  let gesture: ref(option(target)) = ref(None);

  /* Past the sRGB boundary at most hues, so the plane's right edge renders
     clamped -- honest, since the palette is not gamut-limited. */
  let max_chroma = 0.4;
  /* The OKLCH plane's vertical resolution. Chroma interpolates continuously
     along a strip but lightness can only step between them, and CSS cannot
     express a 2D gradient; 160 keeps the steps under a pixel. Only an open
     picker renders them. */
  let strips = 160;

  /* --- reading and writing the literal --- */

  let rec unparen = (e: Language.Exp.t): Language.Exp.t =>
    switch (e.term) {
    | Parens(inner) => unparen(inner)
    | _ => e
    };

  /* In the literal's own units: bytes stay bytes, so reading an `Rgb` does not
     lose a step through OKLCH before anything is touched. */
  type literal =
    | LOklch(float, float, float)
    | LRgb(int, int, int);

  let form_of_literal =
    fun
    | LOklch(_) => AsOklch
    | LRgb(_) => AsRgb;

  let byte_of = (v): option(int) => Bigint.to_int(v);

  let literal_of = (e: Language.Exp.t): option(literal) =>
    switch (unparen(e).term) {
    | Ap(Forward, ctr, arg) =>
      switch (unparen(ctr).term, unparen(arg).term) {
      | (Constructor("Oklch", _), Tuple([l, c, h])) =>
        switch (unparen(l).term, unparen(c).term, unparen(h).term) {
        | (Atom(Float(l)), Atom(Float(c)), Atom(Float(h))) =>
          Some(LOklch(l, c, h))
        | _ => None
        }
      | (Constructor("Rgb", _), Tuple([r, g, b])) =>
        switch (unparen(r).term, unparen(g).term, unparen(b).term) {
        | (Atom(Int(r)), Atom(Int(g)), Atom(Int(b))) =>
          switch (byte_of(r), byte_of(g), byte_of(b)) {
          | (Some(r), Some(g), Some(b)) => Some(LRgb(r, g, b))
          | _ => None
          }
        | _ => None
        }
      | _ => None
      }
    | _ => None
    };

  let literal_of_any = (any: Language.Any.t): option(literal) =>
    switch (any) {
    | Exp(e) => literal_of(e)
    | _ => None
    };

  let literal_of_info = (info: info): literal =>
    switch (
      info.syntax
      |> info.utility.seg_to_term
      |> OptUtil.and_then(literal_of_any)
    ) {
    | Some(t) => t
    | None => failwith("Color: Get: not an Oklch or Rgb literal")
    };

  let form_of_info = (info: info): form =>
    switch (
      info.syntax
      |> info.utility.seg_to_term
      |> OptUtil.and_then(literal_of_any)
    ) {
    | Some(l) => form_of_literal(l)
    | None => AsOklch
    };

  let init = (any: Language.Any.t) =>
    switch (literal_of_any(any)) {
    | Some(_) => Some(init_model)
    | None => None
    };

  /* --- moving between the two representations --- */

  /* Nothing to preserve for a colour read fresh off the term, so the
     degenerate hue is arbitrary; mid-drag the preview carries the real one. */
  let components_of_literal =
    fun
    | LOklch(l, c, h) => Lch(l, c, h)
    | LRgb(r, g, b) => {
        let (h, s, v) = C.hsv_of_rgb(~like=(0., 0., 0.), (r, g, b));
        Hsv(h, s, v);
      };

  let css_of_components =
    fun
    | Lch(l, c, h) => C.to_css(C.Oklch(l, c, h))
    | Hsv(h, s, v) => {
        let (r, g, b) = C.rgb_of_hsv((h, s, v));
        C.to_css(C.Rgb(r, g, b));
      };

  /* The only place a representation changes; the crossing cases are what a tab
     click performs. Oklch -> Rgb quantises and clamps per channel, so hue can
     move as well as chroma. The reverse is exact. */
  let literal_of_components = (form: form, c: components): literal =>
    switch (form, c) {
    | (AsOklch, Lch(l, c, h)) => LOklch(l, c, h)
    | (AsRgb, Hsv(h, s, v)) =>
      let (r, g, b) = C.rgb_of_hsv((h, s, v));
      LRgb(r, g, b);
    | (AsRgb, Lch(l, c, h)) =>
      let (r, g, b) = C.rgb_of_oklch((l, c, h));
      LRgb(r, g, b);
    | (AsOklch, Hsv(hh, s, v)) =>
      let (l, c, h) = C.oklch_of_rgb(C.rgb_of_hsv((hh, s, v)));
      LOklch(l, c, h);
    };

  /* Mid-drag the preview, otherwise the syntax. */
  let showing = (model: model, info: info): components =>
    switch (model.preview) {
    | Some(t) => t
    | None => components_of_literal(literal_of_info(info))
    };

  /* Same form rewrites only the three leaves, so every id survives. Changing
     form replaces the constructor token too, and is the only thing that does. */
  let put = (info: info, l: literal): Base.segment => {
    let same_form = form_of_info(info) == form_of_literal(l);
    let (v1, v2, v3) =
      switch (l) {
      | LOklch(l, c, h) => Language.Atom.(Float(l), Float(c), Float(h))
      | LRgb(r, g, b) =>
        Language.Atom.(
          Int(Bigint.of_int(r)),
          Int(Bigint.of_int(g)),
          Int(Bigint.of_int(b)),
        )
      };
    let set = (e: Language.Exp.t, v): Language.Exp.t => {
      ...e,
      term: Atom(v),
    };
    /* Minted at the call site: a hoisted FreshGrammar id passes statics and
       then faults in `Highlight.of_tile` on a shard mismatch. */
    let ctr = () =>
      C.ctr(
        switch (l) {
        | LOklch(_) => "Oklch"
        | LRgb(_) => "Rgb"
        },
      );
    let rewrite = (e: Language.Exp.t): Language.Exp.t =>
      switch (e.term) {
      | Ap(d, c, {term: Tuple([el, ec, eh]), _} as tup) => {
          ...e,
          term:
            Ap(
              d,
              same_form ? c : ctr(),
              {
                ...tup,
                term: Tuple([set(el, v1), set(ec, v2), set(eh, v3)]),
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

  let text_of = (form: form, c: components) =>
    switch (form, c) {
    | (AsRgb, Hsv(h, s, v)) =>
      let (r, g, b) = C.rgb_of_hsv((h, s, v));
      Printf.sprintf("#%02x%02x%02x", r, g, b);
    | (AsRgb, Lch(l, c, h)) => C.hex_of_oklch((l, c, h))
    | (AsOklch, Lch(l, c, h)) =>
      C.to_css(Oklch(round_to(2, l), round_to(4, c), round_to(2, h)))
    | (AsOklch, Hsv(h, s, v)) =>
      let (l, c, hh) = C.oklch_of_rgb(C.rgb_of_hsv((h, s, v)));
      C.to_css(C.Oklch(l, c, hh));
    };

  /* Either format in either tab -- a pasted hex means the colour, not a
     syntax error -- landing in the form the literal already has, so only the
     tabs change representation. Hex parses to bytes, so pasting one into an
     `Rgb` literal is exact. */
  let parse_text = (form: form, s: string): option(literal) => {
    let s = String.trim(s);
    let inside_oklch =
      switch (String.index_opt(s, '('), String.rindex_opt(s, ')')) {
      | (Some(i), Some(j)) when j > i && String.sub(s, 0, i) == "oklch" =>
        Some(String.sub(s, i + 1, j - i - 1))
      | _ => None
      };
    let parsed =
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
        | [Some(l), Some(c), Some(h), ..._] => Some(LOklch(l, c, h))
        | _ => None
        };
      | None =>
        switch (C.rgb_of_css(s)) {
        | Some((r, g, b)) => Some(LRgb(r, g, b))
        | None => None
        }
      };
    switch (parsed) {
    | None => None
    | Some(l) when form_of_literal(l) == form => Some(l)
    | Some(l) => Some(literal_of_components(form, components_of_literal(l)))
    };
  };

  /* Only the primary button drives the widget; anything else, a right-click
     above all, must fall through UNSTOPPED, or it opens the picker instead of
     the editor's context menu. (`Pointer.Event` lives in src/web.) */
  let primary = (e): bool => e##.button == 0;

  /* --- geometry --- */

  /* Where in the element the event landed. Un-annotated so it takes both
     pointer and mouse events, which js_of_ocaml types distinctly. */
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

  let apply = (t: target, (fx, fy), current: components): components =>
    switch (current, t) {
    | (Lch(_, _, h), Plane) => Lch(100. *. (1. -. fy), max_chroma *. fx, h)
    | (Lch(l, c, _), Hue) => Lch(l, c, 360. *. fx)
    | (Hsv(h, _, _), Plane) => Hsv(h, fx, 1. -. fy)
    | (Hsv(_, s, v), Hue) => Hsv(360. *. fx, s, v)
    };

  let update = (model: model, info: info, action: action) =>
    switch (action) {
    | Toggle => {
        open_: !model.open_,
        preview: None,
        dragging: false,
      }
    | Set(t, fx, fy) => {
        ...model,
        preview: Some(apply(t, (fx, fy), showing(model, info))),
        dragging: true,
      }
    | Release => {
        ...model,
        preview: None,
        dragging: false,
      }
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;

  /* Reserve the panel's room rather than overlap the code beneath. Both
     surfaces are a plane and a hue strip, so both fit one box. */
  let placeholder = (model, _) =>
    model.open_
      ? ProjectorCore.Shape.{
          horizontal: 26,
          vertical: Block(8),
        }
      : ProjectorCore.Shape.inline(2);

  let view = ({model, info, local, parent, _}: View.args(model, action)) => {
    let current = showing(model, info);
    let form = form_of_info(info);
    let swatch = (~extra=[], ()) =>
      Node.div(
        ~attrs=[
          Attr.classes(["cp-swatch", ...extra]),
          Attr.create(
            "style",
            "background-color: " ++ css_of_components(current),
          ),
        ],
        [],
      );

    if (!model.open_) {
      View.mk(
        Node.div(
          ~attrs=[
            Attr.classes(["cp-closed"]),
            Attr.on_pointerdown(e =>
              primary(e)
                ? Effect.Many([local(Toggle), Effect.Stop_propagation])
                : Effect.Ignore
            ),
          ],
          [swatch()],
        ),
      );
    } else {
      /* These handlers are the ones the LAST render installed, so a click --
         pointerdown, mousemove and pointerup inside one frame -- runs entirely
         against a model that predates it. `release` therefore recomputes from
         its own event instead of reading back a preview that may not have
         arrived; without that, clicking the plane did nothing. */
      let element = e =>
        e##.currentTarget |> Js.Opt.get(_, _ => failwith("target"));
      let mine = t => gesture^ == Some(t);
      let grab = (t, e: Js.t(Dom_html.pointerEvent)) =>
        if (!primary(e)) {
          Effect.Ignore;
        } else {
          /* Capture keeps the gesture alive outside the element. */
          JsUtil.setPointerCapture(element(e), e##.pointerId);
          gesture := Some(t);
          let (fx, fy) = fractions(e);
          Effect.Many([local(Set(t, fx, fy)), Effect.Stop_propagation]);
        };
      let move = (t, e: Js.t(Dom_html.mouseEvent)) =>
        if (!model.dragging) {
          Effect.Ignore;
        } else {
          let (fx, fy) = fractions(e);
          local(Set(t, fx, fy));
        };
      let release = (t, e: Js.t(Dom_html.pointerEvent)) =>
        if (!primary(e) || !mine(t)) {
          Effect.Ignore;
        } else {
          gesture := None;
          let c = apply(t, fractions(e), showing(model, info));
          Effect.Many([
            parent(SetSyntax(put(info, literal_of_components(form, c)))),
            local(Release),
          ]);
        };
      let drag_attrs = t => [
        Attr.on_pointerdown(grab(t)),
        Attr.on_mousemove(move(t)),
        Attr.on_pointerup(release(t)),
      ];
      let pct = x => Printf.sprintf("%.2f%%", x *. 100.);
      let dot = (~extra=[], style) =>
        Node.div(
          ~attrs=[
            Attr.classes(["cp-dot", ...extra]),
            Attr.create("style", style),
          ],
          [],
        );
      let gradient = stops =>
        "background: linear-gradient(to right, "
        ++ String.concat(", ", stops)
        ++ ")";
      let track = (~cls, ~style, ~t, ~at, children) =>
        Node.div(
          ~attrs=[
            Attr.classes([cls]),
            Attr.create("style", style),
            ...drag_attrs(t),
          ],
          [dot(~extra=["cp-dot-hue"], "left: " ++ pct(at)), ...children],
        );
      let srgb = ((r, g, b)) => C.to_css(C.Rgb(r, g, b));

      /* Each surface is plane, hue strip, then whatever else it needs. */
      let surface =
        switch (current) {
        | Lch(l, c, h) =>
          /* Strips of constant lightness, each a chroma gradient. The browser
             interpolates in OKLCH, so the plane is exact rather than an sRGB
             approximation of it — and past the gamut it renders clamped,
             which is the truthful picture. */
          let strip = i => {
            let l =
              100.
              *. (1. -. (float_of_int(i) +. 0.5) /. float_of_int(strips));
            Node.div(
              ~attrs=[
                Attr.classes(["cp-strip"]),
                Attr.create(
                  "style",
                  gradient([
                    C.to_css(C.Oklch(l, 0., h)),
                    C.to_css(C.Oklch(l, max_chroma, h)),
                  ]),
                ),
              ],
              [],
            );
          };
          [
            Node.div(
              ~attrs=[Attr.classes(["cp-plane"]), ...drag_attrs(Plane)],
              List.init(strips, strip)
              @ [
                dot(
                  "left: "
                  ++ pct(c /. max_chroma)
                  ++ "; top: "
                  ++ pct(1. -. l /. 100.),
                ),
              ],
            ),
            track(
              ~cls="cp-hue",
              ~style=
                gradient(
                  List.init(13, i =>
                    C.to_css(C.Oklch(70., 0.18, float_of_int(i) *. 30.))
                  ),
                ),
              ~t=Hue,
              ~at=h /. 360.,
              [],
            ),
          ];
        | Hsv(h, s, v) =>
          /* Two flat gradients rather than 56 strips: white-to-hue across,
             transparent-to-black down. Everything here is in gamut by
             construction, so there is no clamped region to draw. */
          let plane =
            Node.div(
              ~attrs=[
                Attr.classes(["cp-plane"]),
                Attr.create(
                  "style",
                  "background: linear-gradient(to top, #000, #0000), "
                  ++ "linear-gradient(to right, #fff, "
                  ++ srgb(C.rgb_of_hsv((h, 1., 1.)))
                  ++ ")",
                ),
                ...drag_attrs(Plane),
              ],
              [dot("left: " ++ pct(s) ++ "; top: " ++ pct(1. -. v))],
            );
          [
            plane,
            track(
              ~cls="cp-hue",
              ~style=
                gradient(
                  List.init(13, i =>
                    srgb(C.rgb_of_hsv((float_of_int(i) *. 30., 1., 1.)))
                  ),
                ),
              ~t=Hue,
              ~at=h /. 360.,
              [],
            ),
          ];
        };

      let tab = (f, text) => {
        let lossy = f == AsRgb && form == AsOklch;
        Node.div(
          ~attrs=
            [
              Attr.classes(["cp-tab", ...form == f ? ["on"] : []]),
              Attr.on_pointerdown(e =>
                if (!primary(e)) {
                  Effect.Ignore;
                } else if (f == form) {
                  Effect.Stop_propagation;
                } else {
                  Effect.Many([
                    parent(
                      SetSyntax(
                        put(info, literal_of_components(f, current)),
                      ),
                    ),
                    Effect.Stop_propagation,
                  ]);
                }
              ),
            ]
            @ (
              lossy
                ? [
                  Attr.create(
                    "title",
                    "Rewrites the literal as sRGB bytes. Eight bits per "
                    ++ "channel, and anything outside sRGB is clamped -- per "
                    ++ "channel, so hue can shift as well as chroma. Going "
                    ++ "back to oklch does not restore it. One undo step.",
                  ),
                ]
                : []
            ),
          [Node.text(text)],
        );
      };
      let entry =
        Node.input(
          ~attrs=[
            Attr.classes(["cp-text"]),
            Attr.string_property("value", text_of(form, current)),
            Attr.on_change((_, v) =>
              switch (parse_text(form, v)) {
              | Some(l) => parent(SetSyntax(put(info, l)))
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
            /* A click inside the panel must not move the editor caret. This
               one swallows EVERY button, unlike the handlers above: an open
               panel is its own surface, so a right-click inside it is inert
               rather than opening the editor's menu on top of it. The closed
               swatch is the opposite -- there the code underneath is what you
               are pointing at. */
            Attr.on_pointerdown(_ => Effect.Stop_propagation),
          ],
          surface
          @ [
            Node.div(
              ~attrs=[Attr.classes(["cp-row"])],
              [
                Node.div(
                  ~attrs=[
                    Attr.classes(["cp-close"]),
                    Attr.on_pointerdown(e =>
                      primary(e)
                        ? Effect.Many([
                            local(Toggle),
                            Effect.Stop_propagation,
                          ])
                        : Effect.Ignore
                    ),
                  ],
                  [swatch(~extra=["cp-swatch-lg"], ())],
                ),
                tab(AsOklch, "oklch"),
                tab(AsRgb, "rgb"),
                entry,
              ],
            ),
          ],
        ),
      );
    };
  };
};
