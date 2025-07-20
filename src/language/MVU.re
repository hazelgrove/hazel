open Virtual_dom.Vdom;
open Util;

type t = {
  settings: CoreSettings.t,
  name: string, // key to store model state
  //inject: Action.t => Ui_effect.t(unit),
  update: DHExp.t,
  model: DHExp.t,
  //font_metrics: FontMetrics.t,
};

let default: t = {
  settings: CoreSettings.on,
  name: "",
  model: IdTagged.FreshGrammar.Exp.tuple([]),
  update: IdTagged.FreshGrammar.Exp.tuple([]),
  //inject,
  //font_metrics,
};

let dhexp_view = (~font_metrics as _, _d) => Node.div([Node.text("TODO")]);
// DHCode.view(
//   ~settings=Settings.Evaluation.init,
//   ~selected_hole_instance=None,
//   ~font_metrics,
//   ~width=80,
//   d,
// );

let camel_case_to_kebab_case = (s: string): string =>
  if (String.length(s) == 0) {
    "";
  } else {
    let chars = StringUtil.to_list(s);
    let result =
      List.mapi(
        (i: int, char_str: string) => {
          let c = char_str.[0];
          if (c >= 'A' && c <= 'Z') {
            if (i == 0) {
              String.make(1, Char.lowercase_ascii(c));
            } else {
              "-" ++ String.make(1, Char.lowercase_ascii(c));
            };
          } else {
            char_str;
          };
        },
        chars,
      );
    String.concat("", result);
  };

open IdTagged.FreshGrammar.Typ;
let attrs: list((string, Typ.t)) = [
  ("Create", prod([string(), string()])), //TODO: style attr type
  ("Style", list(string())),
  ("OnClick", unknown(Internal)), //TODO
  ("OnMousedown", unknown(Internal)),
  ("OnInput", unknown(Internal)),
];

let _style_attrs: list(string) = [
  "AccentColor",
  "AlignContent",
  "AlignItems",
  "AlignSelf",
  "All",
  "Animation",
  "AnimationDelay",
  "AnimationDirection",
  "AnimationDuration",
  "AnimationFillMode",
  "AnimationIterationCount",
  "AnimationName",
  "AnimationPlayState",
  "AnimationTimingFunction",
  "BackfaceVisibility",
  "Background",
  "BackgroundAttachment",
  "BackgroundClip",
  "BackgroundColor",
  "BackgroundImage",
  "BackgroundOrigin",
  "BackgroundPosition",
  "BackgroundRepeat",
  "BackgroundSize",
  "Border",
  "BorderBottom",
  "BorderBottomColor",
  "BorderBottomLeftRadius",
  "BorderBottomRightRadius",
  "BorderBottomStyle",
  "BorderBottomWidth",
  "BorderCollapse",
  "BorderColor",
  "BorderImage",
  "BorderImageOutset",
  "BorderImageRepeat",
  "BorderImageSlice",
  "BorderImageSource",
  "BorderImageWidth",
  "BorderLeft",
  "BorderLeftColor",
  "BorderLeftStyle",
  "BorderLeftWidth",
  "BorderRadius",
  "BorderRight",
  "BorderRightColor",
  "BorderRightStyle",
  "BorderRightWidth",
  "BorderSpacing",
  "BorderStyle",
  "BorderTop",
  "BorderTopColor",
  "BorderTopLeftRadius",
  "BorderTopRightRadius",
  "BorderTopStyle",
  "BorderTopWidth",
  "BorderWidth",
  "Bottom",
  "BoxShadow",
  "BoxSizing",
  "CaptionSide",
  "Clear",
  "Clip",
  "Color",
  "Content",
  "CounterIncrement",
  "CounterReset",
  "Cursor",
  "Direction",
  "Display",
  "EmptyCells",
  "Float",
  "FlexDirection",
  "Font",
  "FontFamily",
  "FontSize",
  "FontSizeAdjust",
  "FontStretch",
  "FontStyle",
  "FontVariant",
  "FontWeight",
  "Gap",
  "Height",
  "JustifyContent",
  "Left",
  "LetterSpacing",
  "LineHeight",
  "ListStyle",
  "ListStyleImage",
  "ListStylePosition",
  "ListStyleType",
  "Margin",
  "MarginBottom",
  "MarginLeft",
  "MarginRight",
  "MarginTop",
  "MaxHeight",
  "MaxWidth",
  "MinHeight",
  "MinWidth",
  "Opacity",
  "Orphans",
  "Outline",
  "OutlineColor",
  "OutlineStyle",
  "OutlineWidth",
  "Overflow",
  "OverflowX",
  "OverflowY",
  "Padding",
  "PaddingBottom",
  "PaddingLeft",
  "PaddingRight",
  "PaddingTop",
  "PageBreakAfter",
  "PageBreakBefore",
  "PageBreakInside",
  "Position",
  "Quotes",
  "Right",
  "TableLayout",
  "TextAlign",
  "TextDecoration",
  "TextIndent",
  "TextTransform",
  "Top",
  "UnicodeBidi",
  "VerticalAlign",
  "Visibility",
  "WhiteSpace",
  "Widows",
  "Width",
  "WordSpacing",
  "ZIndex",
];

let render_style_attr = (d: DHExp.t): string =>
  switch (d) {
  | {
      term:
        Ap(
          _,
          {term: Constructor(constructor_name, _), _},
          {term: Atom(String(s)), _},
        ),
      _,
    } =>
    camel_case_to_kebab_case(constructor_name) ++ ": " ++ s
  | _ => ""
  };

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

// let update =
//     ({name, update, model, settings, _}: t, handler, arg): UpdateAction.t =>
//   //TODO(andrew): betterfy this trash
//   if (update == Tuple([]) && model == Tuple([])) {
//     let inj = Interface.eval_d2d(~settings=settings.core, Ap(handler, arg));
//     switch (inj) {
//     | Ap(Constructor("Inject"), Tuple([StringLit(id), update, action])) =>
//       MUVSyntax(id |> Id.of_string |> Option.get, update, action)
//     | _ =>
//       //TODO: better error handling
//       SetMeta(MVU(name, model))
//     };
//     /* TODO:

//         0. let Inject(id, action, update) = Ap(handler, arg)
//         0.1. let target_id = Stage 2nd child id

//         A. IMPL InsertSegment
//         B. IMPL ApplyToSyntax
//         C. IMPL get stage 2nd child id

//        ApplyToSyntax(id, update, action):
//         1. Move to stage 2nd child id
//         1.0. let ci = from current id
//         1.1. let term = ci.term
//         1.2. let model = elab(term, info)
//         1.3. let res = eval(Ap(update, Tuple([model, action]))
//         1.4. let seg = DHExpToSegment(res)
//         2. Select term
//         3. InsertSegment(seg) (clobbering term)

//         wait... step 1.2 makes assumptions about the term being closed
//         could for now make it a static error on the stage if the model
//         child has nonempty co-ctx. or simply require it to be a value
//         Q: how to canonica lly to check if its a value?
//         */
//   } else {
//     let model =
//       Interface.eval_d2d(
//         ~settings=settings.core,
//         Ap(update, Tuple([model, Ap(handler, arg)])),
//       );
//     SetMeta(MVU(name, model));
//   };

let on_ = (_mvu: t, _handler, _arg, _evt) => {
  /*
   TODO(andrew):
    alternatively: do the Ap(handler, arg) eval as above, but if
    the resulting dhext has shape Inject(Int(id), update, action),
    then still do Ap(update, Tuple([model, action])), but instead of
    SetMetating that, do new action ReplaceAtId(id, model) which will
    call RemoteAction and use DHExpToSegment to replace the model
    */
  print_endline("The goggles do nothing");
  Effect.Many([
    Effect.Stop_propagation,
    //mvu.inject(update(mvu, handler, arg)),
  ]);
};

let pre_process_attr = (d: DHExp.t): option((string, DHExp.term)) =>
  switch (d.term) {
  | Ap(_, {term: Constructor(name, _), _}, {term: arg, _}) =>
    Some((name, arg))
  | _ => None
  };

let render_attr = (mvu: t, d: DHExp.t): Attr.t => {
  IdTagged.FreshGrammar.Exp.(
    switch (pre_process_attr(d)) {
    | Some((
        "Create",
        Tuple([
          {term: Atom(String(name)), _},
          {term: Atom(String(value)), _},
        ]),
      )) =>
      Attr.create(name, value)
    | Some(("Style", ListLit(styles))) => render_styles(styles)
    | Some(("OnClick", handler)) =>
      Attr.on_click(on_(mvu, handler, tuple([])))
    | Some(("OnMousedown", handler)) =>
      Attr.on_mousedown(on_(mvu, handler, tuple([])))
    | Some(("OnInput", handler)) =>
      Attr.on_input((evt, input_str) =>
        on_(mvu, handler, string(input_str), evt)
      )
    | _ =>
      print_endline("FALLTHROUGH: render_attr: " ++ DHExp.show(d));
      //print_endline("ERROR: render_attr: " ++ DHExp.show(d));
      Attr.create("error", "error");
    }
  );
};

let rec render_div =
        (~elide_errors as _=false, context: t, d: DHExp.t): Node.t =>
  switch (d.term) {
  | Ap(_, {term: Constructor("Text", _), _}, {term: Atom(String(str)), _}) =>
    Node.text(str)
  | Ap(_, {term: Constructor("Bool", _), _}, {term: Atom(Bool(b)), _}) =>
    Node.text(string_of_bool(b))
  | Ap(_, {term: Constructor("Int", _), _}, {term: Atom(Int(n)), _}) =>
    switch (Bigint.to_int(n)) {
    | Some(n) => Node.text(string_of_int(n))
    | None => Node.text("666") //TODO(andrew): error
    }
  | Ap(_, {term: Constructor("Float", _), _}, {term: Atom(Float(f)), _}) =>
    Node.text(string_of_float(f))
  | Ap(_, {term: Constructor("Div", _), _}, body) =>
    let (attrs, divs) = attrs_and_divs(context, body);
    Node.div(~attrs, divs);
  | Ap(_, {term: Constructor("Span", _), _}, body) =>
    let (attrs, divs) = attrs_and_divs(context, body);
    Node.span(~attrs, divs);
  | Ap(_, {term: Constructor("Button", _), _}, body) =>
    input_of("button", context, body)
  | Ap(_, {term: Constructor("Checkbox", _), _}, body) =>
    input_of("checkbox", context, body)
  | Ap(_, {term: Constructor("ColorInput", _), _}, body) =>
    input_of("color", context, body)
  | Ap(_, {term: Constructor("DateInput", _), _}, body) =>
    input_of("date", context, body)
  | Ap(_, {term: Constructor("DateTimeLocal", _), _}, body) =>
    input_of("datetime-local", context, body)
  | Ap(_, {term: Constructor("EmailInput", _), _}, body) =>
    input_of("email", context, body)
  | Ap(_, {term: Constructor("FileInput", _), _}, body) =>
    input_of("file", context, body)
  | Ap(_, {term: Constructor("HiddenInput", _), _}, body) =>
    input_of("hidden", context, body)
  | Ap(_, {term: Constructor("ImageInput", _), _}, body) =>
    input_of("image", context, body)
  | Ap(_, {term: Constructor("MonthInput", _), _}, body) =>
    input_of("month", context, body)
  | Ap(_, {term: Constructor("NumberInput", _), _}, body) =>
    input_of("number", context, body)
  | Ap(_, {term: Constructor("PasswordInput", _), _}, body) =>
    input_of("password", context, body)
  | Ap(_, {term: Constructor("Radio", _), _}, body) =>
    input_of("radio", context, body)
  | Ap(_, {term: Constructor("Range", _), _}, body) =>
    input_of("range", context, body)
  | Ap(_, {term: Constructor("ResetInput", _), _}, body) =>
    input_of("reset", context, body)
  | Ap(_, {term: Constructor("SearchInput", _), _}, body) =>
    input_of("search", context, body)
  | Ap(_, {term: Constructor("SubmitInput", _), _}, body) =>
    input_of("submit", context, body)
  | Ap(_, {term: Constructor("TelInput", _), _}, body) =>
    input_of("tel", context, body)
  | Ap(_, {term: Constructor("TextInput", _), _}, body) =>
    input_of("text", context, body)
  | Ap(_, {term: Constructor("TimeInput", _), _}, body) =>
    input_of("time", context, body)
  | Ap(_, {term: Constructor("UrlInput", _), _}, body) =>
    input_of("url", context, body)
  | Ap(_, {term: Constructor("WeekInput", _), _}, body) =>
    input_of("week", context, body)
  | _ =>
    //print_endline("ERROR: render_div: " ++ DHExp.show(d));
    //let d = !elide_errors ? d : IdTagged.FreshGrammar.Exp.empty_hole();
    //dhexp_view(~font_metrics=context.font_metrics, d);
    Node.div([Node.text("TODO")])
  }
and input_of = (input_type: string, mvu: t, body: DHExp.t) => {
  let (attrs, _divs) = attrs_and_divs(mvu, body);
  //TODO(andrew): Do I actually need to do this on_focus for every subcomponent?
  Node.input(
    ~attrs=
      [
        Attr.on_focus(_evt => {
          print_endline("focus: MVU.input_of");
          //Effect.Many([mvu.inject(SetMeta(Focus(MVU)))]);
          Effect.Ignore;
        }),
        Attr.create("type", input_type),
      ]
      @ attrs,
    //divs,
    (),
  );
}
and attrs_and_divs = (mvu: t, body: DHExp.t): (list(Attr.t), list(Node.t)) => {
  //TODO(andrew): not sure why other strip casts is necessary here?
  switch (DHExp.strip_ascriptions(body).term) {
  | Tuple([{term: ListLit(attrs), _}, {term: ListLit(divs), _}]) => (
      List.map(render_attr(mvu), attrs),
      List.map(render_div(mvu), divs),
    )
  | _ =>
    print_endline("ERROR: attrs_and_divs");
    print_endline(DHExp.show(body));
    //([], [dhexp_view(~font_metrics=mvu.font_metrics, body)]);
    ([], [Node.div([Node.text("TODO")])]);
  };
};

// copy-pasted from CLI/Run.re
let evaluate = exp =>
  fst(
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      fst(
        Elaborator.elaborate(
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
          exp,
        ),
      ),
    ),
  );

let go =
    (
      ~settings: CoreSettings.t,
      ~mvu_states,
      ~init_model: DHExp.t,
      ~name: string,
      //~inject: Action.t => Ui_effect.t(unit),
      ~view: DHExp.t,
      ~update: DHExp.t,
    ) => {
  //~font_metrics,

  let model =
    switch (Util.VarMap.lookup(mvu_states, name)) {
    | Some(d) => d
    | _ => init_model
    };
  let mvu = {
    settings,
    name,
    model,
    update,
    //inject,
    //font_metrics,
  };
  //TODO(andrew): casting in ap?
  let result =
    IdTagged.FreshGrammar.Exp.ap(Forward, view, mvu.model)
    |> evaluate
    |> DHExp.strip_ascriptions;
  let attrs = [
    Attr.tabindex(2),
    Attr.create("style", "display: inline-block;"),
    Attr.classes(["mvu-render"]),
    Attr.on_focus(_evt => {
      print_endline("focus: mvu.go");
      //Effect.Many([inject(SetMeta(Focus(MVU)))]);
      Effect.Ignore;
    }),
    //TODO(andrew): cleanup
    /*Attr.on_blur(focus_evt => {
        print_endline(
          "BLUR: " ++ string_of_bool(JsUtil.is_refocus_on_child(focus_evt)),
        );
        print_endline("Blur: if true MVU else Editor");
        Effect.Many([
          inject(
            SetMeta(
              Focus(JsUtil.is_refocus_on_child(focus_evt) ? MVU : Editor),
            ),
          ),
        ]);
      }),*/
  ];
  [Node.div(~attrs, [render_div(mvu, result)])];
};

let go2 = (~settings /*~inject, ~font_metrics,*/, ~node) => {
  let mvu = {
    settings,
    name: "",
    model: IdTagged.FreshGrammar.Exp.tuple([]),
    update: IdTagged.FreshGrammar.Exp.tuple([]),
    //inject,
    //font_metrics,
  };
  let attrs = [
    Attr.tabindex(2),
    Attr.create("style", "display: inline-block;"),
    Attr.classes(["mvu-render"]),
    Attr.on_focus(_evt => {
      print_endline("focus: mvu.go");
      //Effect.Many([inject(SetMeta(Focus(MVU)))]);
      Effect.Ignore;
    }),
    //TODO(andrew): cleanup
    /*Attr.on_blur(focus_evt => {
        print_endline(
          "BLUR: " ++ string_of_bool(JsUtil.is_refocus_on_child(focus_evt)),
        );
        print_endline("Blur: if true MVU else Editor");
        Effect.Many([
          inject(
            SetMeta(
              Focus(JsUtil.is_refocus_on_child(focus_evt) ? MVU : Editor),
            ),
          ),
        ]);
      }),*/
  ];
  [Node.div(~attrs, [render_div(mvu, node)])];
};

//TODO(andrew): cleanup, document
// let get_stage_child = (ci: option(Info.t)): option((Id.t, Term.Exp.t)) =>
//   switch (ci) {
//   | Some(
//       InfoExp({
//         term:
//           {
//             term:
//               Ap(
//                 Forward,
//                 {term: Constructor("Stage", None), _}, //TODO(andrew): type arg?
//                 {
//                   term:
//                     Tuple([
//                       _,
//                       {annotation: {ids: [id, ..._]}, _} as model_exp,
//                     ]),
//                   _,
//                 },
//               ),
//             _,
//           },
//         _,
//       }),
//     ) =>
//     Some((id, model_exp))
//   | _ => None
//   };
