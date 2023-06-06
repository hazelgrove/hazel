open Virtual_dom.Vdom;
open Haz3lcore;
//open Util;
//open OptUtil.Syntax;

type t = {
  name: string, // key to store model state
  inject: UpdateAction.t => Ui_effect.t(unit),
  update: DHExp.t,
  model: DHExp.t,
  view: DHExp.t,
  font_metrics: FontMetrics.t,
};

let mk = (~name, ~inject, ~update, ~model, ~view, ~font_metrics) => {
  name,
  inject,
  update,
  model,
  view,
  font_metrics,
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

let eval = (d: DHExp.t): DHExp.t => {
  print_endline("MVU: eval: starting");
  switch (Interface.evaluate(d)) {
  | (result, _, _) =>
    print_endline("MVU: eval: done");
    let d = EvaluatorResult.unbox(result);
    //print_endline("MVU: res: " ++ DHExp.show(d));
    d;
  };
};

let render_style_attr: DHExp.t => string =
  fun
  | Ap(Tag("AccentColor"), StringLit(s)) => "accent-color: " ++ s
  | Ap(Tag("AlignContent"), StringLit(s)) => "align-content: " ++ s
  | Ap(Tag("AlignItems"), StringLit(s)) => "align-items: " ++ s
  | Ap(Tag("AlignSelf"), StringLit(s)) => "align-self: " ++ s
  | Ap(Tag("All"), StringLit(s)) => "all: " ++ s
  | Ap(Tag("Animation"), StringLit(s)) => "animation: " ++ s
  | Ap(Tag("AnimationDelay"), StringLit(s)) => "animation-delay: " ++ s
  | Ap(Tag("AnimationDirection"), StringLit(s)) =>
    "animation-direction: " ++ s
  | Ap(Tag("AnimationDuration"), StringLit(s)) => "animation-duration: " ++ s
  | Ap(Tag("AnimationFillMode"), StringLit(s)) =>
    "animation-fill-mode: " ++ s
  | Ap(Tag("AnimationIterationCount"), StringLit(s)) =>
    "animation-iteration-count: " ++ s
  | Ap(Tag("AnimationName"), StringLit(s)) => "animation-name: " ++ s
  | Ap(Tag("AnimationPlayState"), StringLit(s)) =>
    "animation-play-state: " ++ s
  | Ap(Tag("AnimationTimingFunction"), StringLit(s)) =>
    "animation-timing-function: " ++ s
  | Ap(Tag("BackfaceVisibility"), StringLit(s)) =>
    "backface-visibility: " ++ s
  | Ap(Tag("Background"), StringLit(s)) => "background: " ++ s
  | Ap(Tag("BackgroundAttachment"), StringLit(s)) =>
    "background-attachment: " ++ s
  | Ap(Tag("BackgroundClip"), StringLit(s)) => "background-clip: " ++ s
  | Ap(Tag("BackgroundColor"), StringLit(s)) => "background-color: " ++ s
  | Ap(Tag("BackgroundImage"), StringLit(s)) => "background-image: " ++ s
  | Ap(Tag("BackgroundOrigin"), StringLit(s)) => "background-origin: " ++ s
  | Ap(Tag("BackgroundPosition"), StringLit(s)) =>
    "background-position: " ++ s
  | Ap(Tag("BackgroundRepeat"), StringLit(s)) => "background-repeat: " ++ s
  | Ap(Tag("BackgroundSize"), StringLit(s)) => "background-size: " ++ s
  | Ap(Tag("Border"), StringLit(s)) => "border: " ++ s
  | Ap(Tag("BorderBottom"), StringLit(s)) => "border-bottom: " ++ s
  | Ap(Tag("BorderBottomColor"), StringLit(s)) =>
    "border-bottom-color: " ++ s
  | Ap(Tag("BorderBottomLeftRadius"), StringLit(s)) =>
    "border-bottom-left-radius: " ++ s
  | Ap(Tag("BorderBottomRightRadius"), StringLit(s)) =>
    "border-bottom-right-radius: " ++ s
  | Ap(Tag("BorderBottomStyle"), StringLit(s)) =>
    "border-bottom-style: " ++ s
  | Ap(Tag("BorderBottomWidth"), StringLit(s)) =>
    "border-bottom-width: " ++ s
  | Ap(Tag("BorderCollapse"), StringLit(s)) => "border-collapse: " ++ s
  | Ap(Tag("BorderColor"), StringLit(s)) => "border-color: " ++ s
  | Ap(Tag("BorderImage"), StringLit(s)) => "border-image: " ++ s
  | Ap(Tag("BorderImageOutset"), StringLit(s)) =>
    "border-image-outset: " ++ s
  | Ap(Tag("BorderImageRepeat"), StringLit(s)) =>
    "border-image-repeat: " ++ s
  | Ap(Tag("BorderImageSlice"), StringLit(s)) => "border-image-slice: " ++ s
  | Ap(Tag("BorderImageSource"), StringLit(s)) =>
    "border-image-source: " ++ s
  | Ap(Tag("BorderImageWidth"), StringLit(s)) => "border-image-width: " ++ s
  | Ap(Tag("BorderLeft"), StringLit(s)) => "border-left: " ++ s
  | Ap(Tag("BorderLeftColor"), StringLit(s)) => "border-left-color: " ++ s
  | Ap(Tag("BorderLeftStyle"), StringLit(s)) => "border-left-style: " ++ s
  | Ap(Tag("BorderLeftWidth"), StringLit(s)) => "border-left-width: " ++ s
  | Ap(Tag("BorderRadius"), StringLit(s)) => "border-radius: " ++ s
  | Ap(Tag("BorderRight"), StringLit(s)) => "border-right: " ++ s
  | Ap(Tag("BorderRightColor"), StringLit(s)) => "border-right-color: " ++ s
  | Ap(Tag("BorderRightStyle"), StringLit(s)) => "border-right-style: " ++ s
  | Ap(Tag("BorderRightWidth"), StringLit(s)) => "border-right-width: " ++ s
  | Ap(Tag("BorderSpacing"), StringLit(s)) => "border-spacing: " ++ s
  | Ap(Tag("BorderStyle"), StringLit(s)) => "border-style: " ++ s
  | Ap(Tag("BorderTop"), StringLit(s)) => "border-top: " ++ s
  | Ap(Tag("BorderTopColor"), StringLit(s)) => "border-top-color: " ++ s
  | Ap(Tag("BorderTopLeftRadius"), StringLit(s)) =>
    "border-top-left-radius: " ++ s
  | Ap(Tag("BorderTopRightRadius"), StringLit(s)) =>
    "border-top-right-radius: " ++ s
  | Ap(Tag("BorderTopStyle"), StringLit(s)) => "border-top-style: " ++ s
  | Ap(Tag("BorderTopWidth"), StringLit(s)) => "border-top-width: " ++ s
  | Ap(Tag("BorderWidth"), StringLit(s)) => "border-width: " ++ s
  | Ap(Tag("Bottom"), StringLit(s)) => "bottom: " ++ s
  | Ap(Tag("BoxShadow"), StringLit(s)) => "box-shadow: " ++ s
  | Ap(Tag("BoxSizing"), StringLit(s)) => "box-sizing: " ++ s
  | Ap(Tag("CaptionSide"), StringLit(s)) => "caption-side: " ++ s
  | Ap(Tag("Clear"), StringLit(s)) => "clear: " ++ s
  | Ap(Tag("Clip"), StringLit(s)) => "clip: " ++ s
  | Ap(Tag("Color"), StringLit(s)) => "color: " ++ s
  | Ap(Tag("Content"), StringLit(s)) => "content: " ++ s
  | Ap(Tag("CounterIncrement"), StringLit(s)) => "counter-increment: " ++ s
  | Ap(Tag("CounterReset"), StringLit(s)) => "counter-reset: " ++ s
  | Ap(Tag("Cursor"), StringLit(s)) => "cursor: " ++ s
  | Ap(Tag("Direction"), StringLit(s)) => "direction: " ++ s
  | Ap(Tag("Display"), StringLit(s)) => "display: " ++ s
  | Ap(Tag("EmptyCells"), StringLit(s)) => "empty-cells: " ++ s
  | Ap(Tag("Float"), StringLit(s)) => "float: " ++ s
  | Ap(Tag("FlexDirection"), StringLit(s)) => "flex-direction: " ++ s
  | Ap(Tag("Font"), StringLit(s)) => "font: " ++ s
  | Ap(Tag("FontFamily"), StringLit(s)) => "font-family: " ++ s
  | Ap(Tag("FontSize"), StringLit(s)) => "font-size: " ++ s
  | Ap(Tag("FontSizeAdjust"), StringLit(s)) => "font-size-adjust: " ++ s
  | Ap(Tag("FontStretch"), StringLit(s)) => "font-stretch: " ++ s
  | Ap(Tag("FontStyle"), StringLit(s)) => "font-style: " ++ s
  | Ap(Tag("FontVariant"), StringLit(s)) => "font-variant: " ++ s
  | Ap(Tag("FontWeight"), StringLit(s)) => "font-weight: " ++ s
  | Ap(Tag("Gap"), StringLit(s)) => "gap: " ++ s
  | Ap(Tag("Height"), StringLit(s)) => "height: " ++ s
  | Ap(Tag("Left"), StringLit(s)) => "left: " ++ s
  | Ap(Tag("LetterSpacing"), StringLit(s)) => "letter-spacing: " ++ s
  | Ap(Tag("LineHeight"), StringLit(s)) => "line-height: " ++ s
  | Ap(Tag("ListStyle"), StringLit(s)) => "list-style: " ++ s
  | Ap(Tag("ListStyleImage"), StringLit(s)) => "list-style-image: " ++ s
  | Ap(Tag("ListStylePosition"), StringLit(s)) =>
    "list-style-position: " ++ s
  | Ap(Tag("ListStyleType"), StringLit(s)) => "list-style-type: " ++ s
  | Ap(Tag("Margin"), StringLit(s)) => "margin: " ++ s
  | Ap(Tag("MarginBottom"), StringLit(s)) => "margin-bottom: " ++ s
  | Ap(Tag("MarginLeft"), StringLit(s)) => "margin-left: " ++ s
  | Ap(Tag("MarginRight"), StringLit(s)) => "margin-right: " ++ s
  | Ap(Tag("MarginTop"), StringLit(s)) => "margin-top: " ++ s
  | Ap(Tag("MaxHeight"), StringLit(s)) => "max-height: " ++ s
  | Ap(Tag("MaxWidth"), StringLit(s)) => "max-width: " ++ s
  | Ap(Tag("MinHeight"), StringLit(s)) => "min-height: " ++ s
  | Ap(Tag("MinWidth"), StringLit(s)) => "min-width: " ++ s
  | Ap(Tag("Opacity"), StringLit(s)) => "opacity: " ++ s
  | Ap(Tag("Orphans"), StringLit(s)) => "orphans: " ++ s
  | Ap(Tag("Outline"), StringLit(s)) => "outline: " ++ s
  | Ap(Tag("OutlineColor"), StringLit(s)) => "outline-color: " ++ s
  | Ap(Tag("OutlineStyle"), StringLit(s)) => "outline-style: " ++ s
  | Ap(Tag("OutlineWidth"), StringLit(s)) => "outline-width: " ++ s
  | Ap(Tag("Overflow"), StringLit(s)) => "overflow: " ++ s
  | Ap(Tag("OverflowX"), StringLit(s)) => "overflow-x: " ++ s
  | Ap(Tag("OverflowY"), StringLit(s)) => "overflow-y: " ++ s
  | Ap(Tag("Padding"), StringLit(s)) => "padding: " ++ s
  | Ap(Tag("PaddingBottom"), StringLit(s)) => "padding-bottom: " ++ s
  | Ap(Tag("PaddingLeft"), StringLit(s)) => "padding-left: " ++ s
  | Ap(Tag("PaddingRight"), StringLit(s)) => "padding-right: " ++ s
  | Ap(Tag("PaddingTop"), StringLit(s)) => "padding-top: " ++ s
  | Ap(Tag("PageBreakAfter"), StringLit(s)) => "page-break-after: " ++ s
  | Ap(Tag("PageBreakBefore"), StringLit(s)) => "page-break-before: " ++ s
  | Ap(Tag("PageBreakInside"), StringLit(s)) => "page-break-inside: " ++ s
  | Ap(Tag("Position"), StringLit(s)) => "position: " ++ s
  | Ap(Tag("Quotes"), StringLit(s)) => "quotes: " ++ s
  | Ap(Tag("Right"), StringLit(s)) => "right: " ++ s
  | Ap(Tag("TableLayout"), StringLit(s)) => "table-layout: " ++ s
  | Ap(Tag("TextAlign"), StringLit(s)) => "text-align: " ++ s
  | Ap(Tag("TextDecoration"), StringLit(s)) => "text-decoration: " ++ s
  | Ap(Tag("TextIndent"), StringLit(s)) => "text-indent: " ++ s
  | Ap(Tag("TextTransform"), StringLit(s)) => "text-transform: " ++ s
  | Ap(Tag("Top"), StringLit(s)) => "top: " ++ s
  | Ap(Tag("UnicodeBidi"), StringLit(s)) => "unicode-bidi: " ++ s
  | Ap(Tag("VerticalAlign"), StringLit(s)) => "vertical-align: " ++ s
  | Ap(Tag("Visibility"), StringLit(s)) => "visibility: " ++ s
  | Ap(Tag("WhiteSpace"), StringLit(s)) => "white-space: " ++ s
  | Ap(Tag("Widows"), StringLit(s)) => "widows: " ++ s
  | Ap(Tag("Width"), StringLit(s)) => "width: " ++ s
  | Ap(Tag("WordSpacing"), StringLit(s)) => "word-spacing: " ++ s
  | Ap(Tag("ZIndex"), StringLit(s)) => "z-index: " ++ s
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

let render_styles = styles =>
  styles
  |> List.map(render_style_attr)
  |> String.concat(";")
  |> Attr.create("style");

let render_attr = ({name, inject, update, model, _}: t, d: DHExp.t): Attr.t => {
  let on_ = (handler, arg) => {
    let maybe_action = eval(Ap(handler, arg));
    let maybe_model = eval(Ap(update, Tuple([model, maybe_action])));
    Virtual_dom.Vdom.Effect.Many([
      Virtual_dom.Vdom.Effect.Stop_propagation,
      //Virtual_dom.Vdom.Effect.Prevent_default,
      inject(MVUSet(name, maybe_model)) //DHExp.strip_casts(maybe_model),
    ]);
  };
  /* TODO: Handlers to implement:

     on_dblclick
     on_mousedown
     on_mouseup
     on_mousemove

     on_keydown
     on_keyup
     on_keypress
      */
  switch (d) {
  | Ap(Tag("Create"), Tuple([StringLit(name), StringLit(value)])) =>
    Attr.create(name, value)
  | Ap(Tag("Style"), ListLit(_, _, _, _, styles)) => render_styles(styles)
  | Ap(Tag("OnClick"), handler) =>
    Attr.on_click(_evt => on_(handler, Tuple([])))
  | Ap(Tag("OnMouseDown"), handler) =>
    Attr.on_mousedown(_evt => on_(handler, Tuple([])))
  | Ap(Tag("OnInput"), handler) =>
    Attr.on_input((_evt, input_str) => on_(handler, StringLit(input_str)))
  | _ =>
    //print_endline("ERROR: render_attr: " ++ DHExp.show(d));
    Attr.create("error", "error")
  };
};

let rec render_div = (~elide_errors=false, context: t, d: DHExp.t): Node.t =>
  switch (d) {
  | Ap(Tag("Text"), StringLit(str)) => Node.text(str)
  | Ap(Tag("Bool"), BoolLit(b)) => Node.text(string_of_bool(b))
  | Ap(Tag("Num" | "Int"), IntLit(n)) => Node.text(string_of_int(n))
  | Ap(Tag("Float"), FloatLit(f)) => Node.text(string_of_float(f))
  | Ap(Tag("Div"), body) =>
    let (attrs, divs) = attrs_and_divs(context, body);
    Node.div(~attr=Attr.many(attrs), divs);
  | Ap(Tag("Button"), body) => input_of("button", context, body)
  | Ap(Tag("Checkbox"), body) => input_of("checkbox", context, body)
  | Ap(Tag("ColorInput"), body) => input_of("color", context, body)
  | Ap(Tag("DateInput"), body) => input_of("date", context, body)
  | Ap(Tag("DateTimeLocal"), body) =>
    input_of("datetime-local", context, body)
  | Ap(Tag("EmailInput"), body) => input_of("email", context, body)
  | Ap(Tag("FileInput"), body) => input_of("file", context, body)
  | Ap(Tag("HiddenInput"), body) => input_of("hidden", context, body)
  | Ap(Tag("ImageInput"), body) => input_of("image", context, body)
  | Ap(Tag("MonthInput"), body) => input_of("month", context, body)
  | Ap(Tag("NumberInput"), body) => input_of("number", context, body)
  | Ap(Tag("PasswordInput"), body) => input_of("password", context, body)
  | Ap(Tag("Radio"), body) => input_of("radio", context, body)
  | Ap(Tag("Range"), body) => input_of("range", context, body)
  | Ap(Tag("ResetInput"), body) => input_of("reset", context, body)
  | Ap(Tag("SearchInput"), body) => input_of("search", context, body)
  | Ap(Tag("SubmitInput"), body) => input_of("submit", context, body)
  | Ap(Tag("TelInput"), body) => input_of("tel", context, body)
  | Ap(Tag("TextInput"), body) => input_of("text", context, body)
  | Ap(Tag("TimeInput"), body) => input_of("time", context, body)
  | Ap(Tag("UrlInput"), body) => input_of("url", context, body)
  | Ap(Tag("WeekInput"), body) => input_of("week", context, body)
  | _ =>
    //print_endline("ERROR: render_div: " ++ DHExp.show(d));
    let d = elide_errors ? DHExp.EmptyHole(0, 0) : d;
    dhexp_view(~font_metrics=context.font_metrics, d);
  //Some(Node.text("error"))
  }
and input_of = (input_type, context, body) => {
  let (attrs, divs) = attrs_and_divs(context, body);
  Node.input(
    ~attr=Attr.many([Attr.create("type", input_type)] @ attrs),
    divs,
  );
}
and attrs_and_divs =
    (context: t, body: DHExp.t): (list(Attr.t), list(Node.t)) =>
  switch (body) {
  | Tuple([ListLit(_, _, _, _, attrs), ListLit(_, _, _, _, divs)]) =>
    let attrs = attrs |> List.map(render_attr(context));
    let divs = divs |> List.map(render_div(context));
    (attrs, divs);
  | _ => ([], [])
  };

let go = (context: t) => {
  print_endline("MVU: go: starting");
  print_endline("MVU: go: model: " ++ DHExp.show(context.model));
  let result = eval(Ap(context.view, context.model));
  //TODO(andrew): decide if actually should be stripping casts
  [
    Node.div(
      ~attr=Attr.classes(["mvu-render"]),
      [
        Node.text("Rendered Node: "),
        render_div(context, DHExp.strip_casts(result)),
      ],
    ),
  ];
};
