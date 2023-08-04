open Haz3lcore;
open Virtual_dom.Vdom;

type t = {
  name: string, // key to store model state
  inject: UpdateAction.t => Ui_effect.t(unit),
  update: DHExp.t,
  model: DHExp.t,
  font_metrics: FontMetrics.t,
};

let dhexp_view = (~font_metrics, d) =>
  DHCode.view_tylr(
    ~settings={
      evaluate: true,
      show_case_clauses: true,
      show_fn_bodies: true,
      show_casts: true,
      show_unevaluated_elaboration: false,
    },
    ~selected_hole_instance=None,
    ~font_metrics,
    ~width=80,
    d,
  );

let render_style_attr: DHExp.t => string =
  fun
  | Ap(Constructor("AccentColor"), StringLit(s)) => "accent-color: " ++ s
  | Ap(Constructor("AlignContent"), StringLit(s)) => "align-content: " ++ s
  | Ap(Constructor("AlignItems"), StringLit(s)) => "align-items: " ++ s
  | Ap(Constructor("AlignSelf"), StringLit(s)) => "align-self: " ++ s
  | Ap(Constructor("All"), StringLit(s)) => "all: " ++ s
  | Ap(Constructor("Animation"), StringLit(s)) => "animation: " ++ s
  | Ap(Constructor("AnimationDelay"), StringLit(s)) =>
    "animation-delay: " ++ s
  | Ap(Constructor("AnimationDirection"), StringLit(s)) =>
    "animation-direction: " ++ s
  | Ap(Constructor("AnimationDuration"), StringLit(s)) =>
    "animation-duration: " ++ s
  | Ap(Constructor("AnimationFillMode"), StringLit(s)) =>
    "animation-fill-mode: " ++ s
  | Ap(Constructor("AnimationIterationCount"), StringLit(s)) =>
    "animation-iteration-count: " ++ s
  | Ap(Constructor("AnimationName"), StringLit(s)) => "animation-name: " ++ s
  | Ap(Constructor("AnimationPlayState"), StringLit(s)) =>
    "animation-play-state: " ++ s
  | Ap(Constructor("AnimationTimingFunction"), StringLit(s)) =>
    "animation-timing-function: " ++ s
  | Ap(Constructor("BackfaceVisibility"), StringLit(s)) =>
    "backface-visibility: " ++ s
  | Ap(Constructor("Background"), StringLit(s)) => "background: " ++ s
  | Ap(Constructor("BackgroundAttachment"), StringLit(s)) =>
    "background-attachment: " ++ s
  | Ap(Constructor("BackgroundClip"), StringLit(s)) =>
    "background-clip: " ++ s
  | Ap(Constructor("BackgroundColor"), StringLit(s)) =>
    "background-color: " ++ s
  | Ap(Constructor("BackgroundImage"), StringLit(s)) =>
    "background-image: " ++ s
  | Ap(Constructor("BackgroundOrigin"), StringLit(s)) =>
    "background-origin: " ++ s
  | Ap(Constructor("BackgroundPosition"), StringLit(s)) =>
    "background-position: " ++ s
  | Ap(Constructor("BackgroundRepeat"), StringLit(s)) =>
    "background-repeat: " ++ s
  | Ap(Constructor("BackgroundSize"), StringLit(s)) =>
    "background-size: " ++ s
  | Ap(Constructor("Border"), StringLit(s)) => "border: " ++ s
  | Ap(Constructor("BorderBottom"), StringLit(s)) => "border-bottom: " ++ s
  | Ap(Constructor("BorderBottomColor"), StringLit(s)) =>
    "border-bottom-color: " ++ s
  | Ap(Constructor("BorderBottomLeftRadius"), StringLit(s)) =>
    "border-bottom-left-radius: " ++ s
  | Ap(Constructor("BorderBottomRightRadius"), StringLit(s)) =>
    "border-bottom-right-radius: " ++ s
  | Ap(Constructor("BorderBottomStyle"), StringLit(s)) =>
    "border-bottom-style: " ++ s
  | Ap(Constructor("BorderBottomWidth"), StringLit(s)) =>
    "border-bottom-width: " ++ s
  | Ap(Constructor("BorderCollapse"), StringLit(s)) =>
    "border-collapse: " ++ s
  | Ap(Constructor("BorderColor"), StringLit(s)) => "border-color: " ++ s
  | Ap(Constructor("BorderImage"), StringLit(s)) => "border-image: " ++ s
  | Ap(Constructor("BorderImageOutset"), StringLit(s)) =>
    "border-image-outset: " ++ s
  | Ap(Constructor("BorderImageRepeat"), StringLit(s)) =>
    "border-image-repeat: " ++ s
  | Ap(Constructor("BorderImageSlice"), StringLit(s)) =>
    "border-image-slice: " ++ s
  | Ap(Constructor("BorderImageSource"), StringLit(s)) =>
    "border-image-source: " ++ s
  | Ap(Constructor("BorderImageWidth"), StringLit(s)) =>
    "border-image-width: " ++ s
  | Ap(Constructor("BorderLeft"), StringLit(s)) => "border-left: " ++ s
  | Ap(Constructor("BorderLeftColor"), StringLit(s)) =>
    "border-left-color: " ++ s
  | Ap(Constructor("BorderLeftStyle"), StringLit(s)) =>
    "border-left-style: " ++ s
  | Ap(Constructor("BorderLeftWidth"), StringLit(s)) =>
    "border-left-width: " ++ s
  | Ap(Constructor("BorderRadius"), StringLit(s)) => "border-radius: " ++ s
  | Ap(Constructor("BorderRight"), StringLit(s)) => "border-right: " ++ s
  | Ap(Constructor("BorderRightColor"), StringLit(s)) =>
    "border-right-color: " ++ s
  | Ap(Constructor("BorderRightStyle"), StringLit(s)) =>
    "border-right-style: " ++ s
  | Ap(Constructor("BorderRightWidth"), StringLit(s)) =>
    "border-right-width: " ++ s
  | Ap(Constructor("BorderSpacing"), StringLit(s)) => "border-spacing: " ++ s
  | Ap(Constructor("BorderStyle"), StringLit(s)) => "border-style: " ++ s
  | Ap(Constructor("BorderTop"), StringLit(s)) => "border-top: " ++ s
  | Ap(Constructor("BorderTopColor"), StringLit(s)) =>
    "border-top-color: " ++ s
  | Ap(Constructor("BorderTopLeftRadius"), StringLit(s)) =>
    "border-top-left-radius: " ++ s
  | Ap(Constructor("BorderTopRightRadius"), StringLit(s)) =>
    "border-top-right-radius: " ++ s
  | Ap(Constructor("BorderTopStyle"), StringLit(s)) =>
    "border-top-style: " ++ s
  | Ap(Constructor("BorderTopWidth"), StringLit(s)) =>
    "border-top-width: " ++ s
  | Ap(Constructor("BorderWidth"), StringLit(s)) => "border-width: " ++ s
  | Ap(Constructor("Bottom"), StringLit(s)) => "bottom: " ++ s
  | Ap(Constructor("BoxShadow"), StringLit(s)) => "box-shadow: " ++ s
  | Ap(Constructor("BoxSizing"), StringLit(s)) => "box-sizing: " ++ s
  | Ap(Constructor("CaptionSide"), StringLit(s)) => "caption-side: " ++ s
  | Ap(Constructor("Clear"), StringLit(s)) => "clear: " ++ s
  | Ap(Constructor("Clip"), StringLit(s)) => "clip: " ++ s
  | Ap(Constructor("Color"), StringLit(s)) => "color: " ++ s
  | Ap(Constructor("Content"), StringLit(s)) => "content: " ++ s
  | Ap(Constructor("CounterIncrement"), StringLit(s)) =>
    "counter-increment: " ++ s
  | Ap(Constructor("CounterReset"), StringLit(s)) => "counter-reset: " ++ s
  | Ap(Constructor("Cursor"), StringLit(s)) => "cursor: " ++ s
  | Ap(Constructor("Direction"), StringLit(s)) => "direction: " ++ s
  | Ap(Constructor("Display"), StringLit(s)) => "display: " ++ s
  | Ap(Constructor("EmptyCells"), StringLit(s)) => "empty-cells: " ++ s
  | Ap(Constructor("Float"), StringLit(s)) => "float: " ++ s
  | Ap(Constructor("FlexDirection"), StringLit(s)) => "flex-direction: " ++ s
  | Ap(Constructor("Font"), StringLit(s)) => "font: " ++ s
  | Ap(Constructor("FontFamily"), StringLit(s)) => "font-family: " ++ s
  | Ap(Constructor("FontSize"), StringLit(s)) => "font-size: " ++ s
  | Ap(Constructor("FontSizeAdjust"), StringLit(s)) =>
    "font-size-adjust: " ++ s
  | Ap(Constructor("FontStretch"), StringLit(s)) => "font-stretch: " ++ s
  | Ap(Constructor("FontStyle"), StringLit(s)) => "font-style: " ++ s
  | Ap(Constructor("FontVariant"), StringLit(s)) => "font-variant: " ++ s
  | Ap(Constructor("FontWeight"), StringLit(s)) => "font-weight: " ++ s
  | Ap(Constructor("Gap"), StringLit(s)) => "gap: " ++ s
  | Ap(Constructor("Height"), StringLit(s)) => "height: " ++ s
  | Ap(Constructor("JustifyContent"), StringLit(s)) =>
    "justify-content: " ++ s
  | Ap(Constructor("Left"), StringLit(s)) => "left: " ++ s
  | Ap(Constructor("LetterSpacing"), StringLit(s)) => "letter-spacing: " ++ s
  | Ap(Constructor("LineHeight"), StringLit(s)) => "line-height: " ++ s
  | Ap(Constructor("ListStyle"), StringLit(s)) => "list-style: " ++ s
  | Ap(Constructor("ListStyleImage"), StringLit(s)) =>
    "list-style-image: " ++ s
  | Ap(Constructor("ListStylePosition"), StringLit(s)) =>
    "list-style-position: " ++ s
  | Ap(Constructor("ListStyleType"), StringLit(s)) =>
    "list-style-type: " ++ s
  | Ap(Constructor("Margin"), StringLit(s)) => "margin: " ++ s
  | Ap(Constructor("MarginBottom"), StringLit(s)) => "margin-bottom: " ++ s
  | Ap(Constructor("MarginLeft"), StringLit(s)) => "margin-left: " ++ s
  | Ap(Constructor("MarginRight"), StringLit(s)) => "margin-right: " ++ s
  | Ap(Constructor("MarginTop"), StringLit(s)) => "margin-top: " ++ s
  | Ap(Constructor("MaxHeight"), StringLit(s)) => "max-height: " ++ s
  | Ap(Constructor("MaxWidth"), StringLit(s)) => "max-width: " ++ s
  | Ap(Constructor("MinHeight"), StringLit(s)) => "min-height: " ++ s
  | Ap(Constructor("MinWidth"), StringLit(s)) => "min-width: " ++ s
  | Ap(Constructor("Opacity"), StringLit(s)) => "opacity: " ++ s
  | Ap(Constructor("Orphans"), StringLit(s)) => "orphans: " ++ s
  | Ap(Constructor("Outline"), StringLit(s)) => "outline: " ++ s
  | Ap(Constructor("OutlineColor"), StringLit(s)) => "outline-color: " ++ s
  | Ap(Constructor("OutlineStyle"), StringLit(s)) => "outline-style: " ++ s
  | Ap(Constructor("OutlineWidth"), StringLit(s)) => "outline-width: " ++ s
  | Ap(Constructor("Overflow"), StringLit(s)) => "overflow: " ++ s
  | Ap(Constructor("OverflowX"), StringLit(s)) => "overflow-x: " ++ s
  | Ap(Constructor("OverflowY"), StringLit(s)) => "overflow-y: " ++ s
  | Ap(Constructor("Padding"), StringLit(s)) => "padding: " ++ s
  | Ap(Constructor("PaddingBottom"), StringLit(s)) => "padding-bottom: " ++ s
  | Ap(Constructor("PaddingLeft"), StringLit(s)) => "padding-left: " ++ s
  | Ap(Constructor("PaddingRight"), StringLit(s)) => "padding-right: " ++ s
  | Ap(Constructor("PaddingTop"), StringLit(s)) => "padding-top: " ++ s
  | Ap(Constructor("PageBreakAfter"), StringLit(s)) =>
    "page-break-after: " ++ s
  | Ap(Constructor("PageBreakBefore"), StringLit(s)) =>
    "page-break-before: " ++ s
  | Ap(Constructor("PageBreakInside"), StringLit(s)) =>
    "page-break-inside: " ++ s
  | Ap(Constructor("Position"), StringLit(s)) => "position: " ++ s
  | Ap(Constructor("Quotes"), StringLit(s)) => "quotes: " ++ s
  | Ap(Constructor("Right"), StringLit(s)) => "right: " ++ s
  | Ap(Constructor("TableLayout"), StringLit(s)) => "table-layout: " ++ s
  | Ap(Constructor("TextAlign"), StringLit(s)) => "text-align: " ++ s
  | Ap(Constructor("TextDecoration"), StringLit(s)) =>
    "text-decoration: " ++ s
  | Ap(Constructor("TextIndent"), StringLit(s)) => "text-indent: " ++ s
  | Ap(Constructor("TextTransform"), StringLit(s)) => "text-transform: " ++ s
  | Ap(Constructor("Top"), StringLit(s)) => "top: " ++ s
  | Ap(Constructor("UnicodeBidi"), StringLit(s)) => "unicode-bidi: " ++ s
  | Ap(Constructor("VerticalAlign"), StringLit(s)) => "vertical-align: " ++ s
  | Ap(Constructor("Visibility"), StringLit(s)) => "visibility: " ++ s
  | Ap(Constructor("WhiteSpace"), StringLit(s)) => "white-space: " ++ s
  | Ap(Constructor("Widows"), StringLit(s)) => "widows: " ++ s
  | Ap(Constructor("Width"), StringLit(s)) => "width: " ++ s
  | Ap(Constructor("WordSpacing"), StringLit(s)) => "word-spacing: " ++ s
  | Ap(Constructor("ZIndex"), StringLit(s)) => "z-index: " ++ s
  | _ => "";

/*

 TODO: Event types to support:

 Dom_html.mouseEvent
 detail: int
 method clientX : int
 method clientY : int
 method ctrlKey : bool
 method shiftKey : bool
 method altKey : bool
 method metaKey : bool

 Dom_html.keyboardEvent
 key: string
 method ctrlKey : bool
 method shiftKey : bool
 method altKey : bool
 method metaKey : bool

 Effects to support:

 Stop_propagaton
 Prevent_default
 */

/* TODO: Handlers to implement:

   on_dblclick
   on_mousedown
   on_mouseup
   on_mousemove

   on_keydown
   on_keyup
   on_keypress
    */

let render_styles = styles =>
  styles
  |> List.map(render_style_attr)
  |> String.concat(";")
  |> Attr.create("style");

let update = ({name, update, model, _}: t, handler, arg): UpdateAction.t => {
  let model =
    Interface.eval_d2d(Ap(update, Tuple([model, Ap(handler, arg)])));
  SetMeta(MVU(name, model));
};

let on_ = (mvu: t, handler, arg) =>
  Effect.Many([
    Effect.Stop_propagation,
    mvu.inject(update(mvu, handler, arg)),
  ]);

let render_attr = (mvu: t, d: DHExp.t): Attr.t => {
  switch (d) {
  | Ap(Constructor("Create"), Tuple([StringLit(name), StringLit(value)])) =>
    Attr.create(name, value)
  | Ap(Constructor("Style"), ListLit(_, _, _, styles)) =>
    render_styles(styles)
  | Ap(Constructor("OnClick"), handler) =>
    Attr.on_click(_evt => on_(mvu, handler, Tuple([])))
  | Ap(Constructor("OnMouseDown"), handler) =>
    Attr.on_mousedown(_evt => on_(mvu, handler, Tuple([])))
  | Ap(Constructor("OnInput"), handler) =>
    Attr.on_input((_evt, input_str) =>
      on_(mvu, handler, StringLit(input_str))
    )
  | _ =>
    //print_endline("ERROR: render_attr: " ++ DHExp.show(d));
    Attr.create("error", "error")
  };
};

let rec render_div = (~elide_errors=false, context: t, d: DHExp.t): Node.t =>
  switch (d) {
  | Ap(Constructor("Text"), StringLit(str)) => Node.text(str)
  | Ap(Constructor("Bool"), BoolLit(b)) => Node.text(string_of_bool(b))
  | Ap(Constructor("Num" | "Int"), IntLit(n)) =>
    Node.text(string_of_int(n))
  | Ap(Constructor("Float"), FloatLit(f)) => Node.text(string_of_float(f))
  | Ap(Constructor("Div"), body) =>
    let (attrs, divs) = attrs_and_divs(context, body);
    Node.div(~attr=Attr.many(attrs), divs);
  | Ap(Constructor("Button"), body) => input_of("button", context, body)
  | Ap(Constructor("Checkbox"), body) => input_of("checkbox", context, body)
  | Ap(Constructor("ColorInput"), body) => input_of("color", context, body)
  | Ap(Constructor("DateInput"), body) => input_of("date", context, body)
  | Ap(Constructor("DateTimeLocal"), body) =>
    input_of("datetime-local", context, body)
  | Ap(Constructor("EmailInput"), body) => input_of("email", context, body)
  | Ap(Constructor("FileInput"), body) => input_of("file", context, body)
  | Ap(Constructor("HiddenInput"), body) =>
    input_of("hidden", context, body)
  | Ap(Constructor("ImageInput"), body) => input_of("image", context, body)
  | Ap(Constructor("MonthInput"), body) => input_of("month", context, body)
  | Ap(Constructor("NumberInput"), body) =>
    input_of("number", context, body)
  | Ap(Constructor("PasswordInput"), body) =>
    input_of("password", context, body)
  | Ap(Constructor("Radio"), body) => input_of("radio", context, body)
  | Ap(Constructor("Range"), body) => input_of("range", context, body)
  | Ap(Constructor("ResetInput"), body) => input_of("reset", context, body)
  | Ap(Constructor("SearchInput"), body) =>
    input_of("search", context, body)
  | Ap(Constructor("SubmitInput"), body) =>
    input_of("submit", context, body)
  | Ap(Constructor("TelInput"), body) => input_of("tel", context, body)
  | Ap(Constructor("TextInput"), body) => input_of("text", context, body)
  | Ap(Constructor("TimeInput"), body) => input_of("time", context, body)
  | Ap(Constructor("UrlInput"), body) => input_of("url", context, body)
  | Ap(Constructor("WeekInput"), body) => input_of("week", context, body)
  | _ =>
    //print_endline("ERROR: render_div: " ++ DHExp.show(d));
    let d = elide_errors ? DHExp.EmptyHole(0, 0) : d;
    dhexp_view(~font_metrics=context.font_metrics, d);
  }
and input_of = (input_type: string, mvu: t, body: DHExp.t) => {
  let (attrs, divs) = attrs_and_divs(mvu, body);
  Node.input(
    ~attr=Attr.many([Attr.create("type", input_type)] @ attrs),
    divs,
  );
}
and attrs_and_divs = (mvu: t, body: DHExp.t): (list(Attr.t), list(Node.t)) =>
  switch (body) {
  | Tuple([ListLit(_, _, _, attrs), ListLit(_, _, _, divs)]) => (
      List.map(render_attr(mvu), attrs),
      List.map(render_div(mvu), divs),
    )
  | _ => ([], [])
  };

let go =
    (~mvu_states, ~init_model, ~name, ~inject, ~view, ~update, ~font_metrics) => {
  let model =
    switch (VarMap.lookup(mvu_states, name)) {
    | Some(d) => d
    | _ => init_model
    };
  let mvu = {name, model, update, inject, font_metrics};
  //TODO(andrew): casting in ap?
  let result = Ap(view, mvu.model) |> Interface.eval_d2d |> DHExp.strip_casts;
  [
    Node.div(~attr=Attr.classes(["mvu-render"]), [render_div(mvu, result)]),
  ];
};
