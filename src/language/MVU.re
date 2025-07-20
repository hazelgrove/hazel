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

let input_type_mappings: list((string, string)) = [
  ("Button", "button"),
  ("Checkbox", "checkbox"),
  ("ColorInput", "color"),
  ("DateInput", "date"),
  ("DateTimeLocal", "datetime-local"),
  ("EmailInput", "email"),
  ("FileInput", "file"),
  ("HiddenInput", "hidden"),
  ("ImageInput", "image"),
  ("MonthInput", "month"),
  ("NumberInput", "number"),
  ("PasswordInput", "password"),
  ("Radio", "radio"),
  ("Range", "range"),
  ("ResetInput", "reset"),
  ("SearchInput", "search"),
  ("SubmitInput", "submit"),
  ("TelInput", "tel"),
  ("TextInput", "text"),
  ("TimeInput", "time"),
  ("UrlInput", "url"),
  ("WeekInput", "week"),
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
  // "PageBreakAfter",
  // "PageBreakBefore",
  // "PageBreakInside",
  "Position",
  // "Quotes",
  "Right",
  "TableLayout",
  "TextAlign",
  "TextDecoration",
  "TextIndent",
  "TextTransform",
  "Top",
  // "UnicodeBidi",
  "VerticalAlign",
  "Visibility",
  "WhiteSpace",
  "Widows",
  "Width",
  "WordSpacing",
  "ZIndex",
];

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
    | Some(x) =>
      switch (x) {
      | (
          "Create",
          Tuple([
            {term: Atom(String(name)), _},
            {term: Atom(String(value)), _},
          ]),
        ) =>
        Attr.create(name, value)
      | ("Style", ListLit(styles)) => render_styles(styles)
      | ("OnClick", handler) => Attr.on_click(on_(mvu, handler, tuple([])))
      | ("OnMousedown", handler) =>
        Attr.on_mousedown(on_(mvu, handler, tuple([])))
      | ("OnInput", handler) =>
        Attr.on_input((evt, input_str) =>
          on_(mvu, handler, string(input_str), evt)
        )
      | _ =>
        print_endline("FALLTHROUGH: render_attr: " ++ DHExp.show(d));
        //print_endline("ERROR: render_attr: " ++ DHExp.show(d));
        Attr.create("error", "error");
      }
    | None =>
      print_endline("FALLTHROUGH: render_attr: " ++ DHExp.show(d));
      //print_endline("ERROR: render_attr: " ++ DHExp.show(d));
      Attr.create("error", "error");
    }
  );
};

let pre_process_elem = (d: DHExp.t): option((string, DHExp.t)) =>
  switch (d.term) {
  | Ap(_, {term: Constructor(name, _), _}, body) => Some((name, body))
  | _ => None
  };

let dhexp_view = (~font_metrics as _, _d) => Node.div([Node.text("TODO")]);
// DHCode.view(
//   ~settings=Settings.Evaluation.init,
//   ~selected_hole_instance=None,
//   ~font_metrics,
//   ~width=80,
//   d,
// );

let rec render_elem =
        (~elide_errors as _=false, context: t, d: DHExp.t): Node.t =>
  switch (pre_process_elem(d)) {
  | Some(x) =>
    switch (x) {
    | ("Text", {term: Atom(String(str)), _}) => Node.text(str)
    | ("Bool", {term: Atom(Bool(b)), _}) => Node.text(string_of_bool(b))
    | ("Int", {term: Atom(Int(n)), _}) =>
      switch (Bigint.to_int(n)) {
      | Some(n) => Node.text(string_of_int(n))
      | None => Node.div([Node.text("TODO")])
      }
    | ("Float", {term: Atom(Float(f)), _}) =>
      Node.text(string_of_float(f))
    | ("Div", body) =>
      let (attrs, divs) = attrs_and_elems(context, body);
      Node.div(~attrs, divs);
    | ("Span", body) =>
      let (attrs, divs) = attrs_and_elems(context, body);
      Node.span(~attrs, divs);
    | (constructor_name, body) =>
      switch (List.assoc_opt(constructor_name, input_type_mappings)) {
      | Some(input_type) => input_of(input_type, context, body)
      | None =>
        print_endline("ERROR: render_elem: " ++ DHExp.show(d));
        Node.div([Node.text("TODO")]);
      }
    }
  | _ =>
    //print_endline("ERROR: render_elem: " ++ DHExp.show(d));
    //let d = !elide_errors ? d : IdTagged.FreshGrammar.Exp.empty_hole();
    //dhexp_view(~font_metrics=context.font_metrics, d);
    Node.div([Node.text("TODO")])
  }
and input_of = (input_type: string, mvu: t, body: DHExp.t) => {
  let (attrs, _divs) = attrs_and_elems(mvu, body);
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
and attrs_and_elems = (mvu: t, body: DHExp.t): (list(Attr.t), list(Node.t)) => {
  //TODO(andrew): not sure why other strip casts is necessary here?
  switch (DHExp.strip_ascriptions(body).term) {
  | Tuple([{term: ListLit(attrs), _}, {term: ListLit(divs), _}]) => (
      List.map(render_attr(mvu), attrs),
      List.map(render_elem(mvu), divs),
    )
  | _ =>
    print_endline("ERROR: attrs_and_elems");
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
  [Node.div(~attrs, [render_elem(mvu, result)])];
};

let go2 = /*~settings, ~inject, ~font_metrics,*/ (d: DHExp.t) => {
  let mvu = {
    settings: CoreSettings.on,
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
  Node.div(~attrs, [render_elem(mvu, d)]);
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
