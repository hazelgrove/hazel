open Util;
open ProjectorBase;
open Language;
open IdTagged.FreshGrammar;

// All valid HTML constructor names
let html_constructors = [
  // Text/primitive content
  "Text",
  "Bool",
  "Int",
  "Float",
  // Structural elements
  "Div",
  "Span",
  "P",
  "Pre",
  "Code",
  "Blockquote",
  // Headings
  "H1",
  "H2",
  "H3",
  "H4",
  "H5",
  "H6",
  // Lists
  "Ul",
  "Ol",
  "Li",
  // Forms
  "Form",
  "Label",
  "Input",
  "TextArea",
  "Button",
  "Select",
  "Option",
  "Checkbox",
  "Radio",
  "Range",
  // Links and media
  "A",
  "Img",
  // Tables
  "Table",
  "Thead",
  "Tbody",
  "Tr",
  "Th",
  "Td",
  // Semantic sections
  "Header",
  "Footer",
  "Nav",
  "Main",
  "Section",
  "Article",
  "Aside",
  // Utility
  "Br",
  "Hr",
  // Generic
  "Node",
];

// Check if a name is a valid HTML constructor
let is_html_constructor = (name: string): bool =>
  List.mem(name, html_constructors);

// Detect if expression is an App type: ((HTML, Cmd), HTML -> Sub)
// Returns Some((html_model, init_cmd, subscriptions_fn)) or None
let detect_app =
    (exp: DHExp.t): option((DHExp.t, option(DHExp.t), option(DHExp.t))) => {
  switch (exp.term) {
  | Tuple([init, subs_fn])
  | Parens({term: Tuple([init, subs_fn]), _}) =>
    switch (init.term) {
    | Tuple([html_model, init_cmd])
    | Parens({term: Tuple([html_model, init_cmd]), _}) =>
      Some((html_model, Some(init_cmd), Some(subs_fn)))
    | _ => None
    }
  | _ => None
  };
};

// Check if expression looks like an App type (for init detection)
// App = ((HTML, Cmd), HTML -> Sub)
let looks_like_app = (exp: DHExp.t): bool =>
  switch (exp.term) {
  | Tuple([init, _subs_fn])
  | Parens({term: Tuple([init, _subs_fn]), _}) =>
    switch (init.term) {
    | Tuple([_html, _cmd])
    | Parens({term: Tuple([_html, _cmd]), _}) => true
    | _ => false
    }
  | _ => false
  };

// Evaluate directly (skip elaboration/statics). Expressions from
// MVU runtime contain Closures which the elaborator can't handle.
let evaluate = exp => fst(Evaluator.evaluate(~env=Builtins.env_init, exp));

// Refs for resize drag state
let wrapper_ref: ref(option(Js_of_ocaml.Js.Unsafe.any)) = ref(None);
let resize_cols = ref(40);
let resize_rows = ref(12);
// Pixel-per-char ratios computed on pointerdown, used during drag
let px_per_col = ref(10.0);
let px_per_row = ref(18.0);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type ui_state = {
    cols: int,
    rows: int,
  };

  let default_ui: ui_state = {
    cols: 40,
    rows: 12,
  };

  // Shadow derived deserializers to handle migration from old format
  let ui_state_of_sexp = sexp =>
    try(ui_state_of_sexp(sexp)) {
    | _ => default_ui
    };
  let ui_state_of_yojson = json =>
    try(ui_state_of_yojson(json)) {
    | _ => default_ui
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    exp: Grammar.exp_t(IdTagged.IdTag.t),
    ui: ui_state,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetDimensions(int, int)
    | ResetSize;

  let init = (any: Any.t) =>
    switch (any) {
    // HTML constructor applied to arguments: Div(...), Button(...), etc.
    | Exp({term: Ap(_, {term: Constructor(name, _), _}, _), _} as exp)
        when is_html_constructor(name) =>
      Some({
        exp,
        ui: default_ui,
      })
    // Nullary HTML constructor: Br
    | Exp({term: Constructor("Br", _), _} as exp) =>
      Some({
        exp,
        ui: default_ui,
      })
    // App type: ((HTML, Cmd), HTML -> Sub) tuple
    | Exp(exp) when looks_like_app(exp) =>
      Some({
        exp,
        ui: default_ui,
      })
    | _ => None
    };

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (m: model, _) =>
    ProjectorCore.Shape.{
      horizontal: m.ui.cols,
      vertical: Block(m.ui.rows - 1),
    };

  let update = (m: model, _, action: action) => {
    switch (action) {
    | SetDimensions(cols, rows) => {
        ...m,
        ui: {
          cols: max(8, cols),
          rows: max(3, rows),
        },
      }
    | ResetSize => {
        ...m,
        ui: default_ui,
      }
    };
  };

  let view =
      ({model, info, parent, local, view_seg, _}: View.args(model, action)) => {
    open Virtual_dom.Vdom;

    // Get current expression from syntax or fall back to model
    let current_exp =
      switch (info.syntax |> info.utility.seg_to_term) {
      | Some(Exp(term)) => term
      | _ => model.exp
      };

    // Inject updates the underlying syntax (expression only)
    let inject_exp = (new_exp: DHExp.t) =>
      parent(SetSyntax(Exp(new_exp) |> info.utility.term_to_seg));

    // Check if model is an App type vs plain Html
    let (html_model, subscriptions) =
      switch (detect_app(current_exp)) {
      | Some((html, Some(init_cmd), Some(subs_fn))) =>
        // It's an App - run init_cmd and evaluate subscriptions
        let cmd_ctx: CmdRunner.context = {
          model: html,
          inject: inject_exp,
          update_fn: None,
        };
        let cmd_effect = CmdRunner.run(cmd_ctx, init_cmd);
        Bonsai.Effect.Expert.handle(cmd_effect);
        let subs = evaluate(Exp.ap(Forward, subs_fn, html));
        (html, Some(subs));
      | Some((html, None, Some(subs_fn))) =>
        // App with no init cmd
        let subs = evaluate(Exp.ap(Forward, subs_fn, html));
        (html, Some(subs));
      | _ =>
        // Plain Html - no subscriptions
        (current_exp, None)
      };

    let seed: HazelDOM.t = {
      model: html_model,
      inject: inject_exp,
      view_term: term =>
        Exp(term)
        |> info.utility.term_to_seg
        |> view_seg(~background=false, Exp),
      projector_id: Some(info.id),
      subscriptions,
      update_fn: None,
    };

    // Corner resize handle with pointer capture for drag.
    // On pointerdown: compute px-per-char ratios from the .projector container.
    // On mousemove: convert cursor position to char units; dispatch when changed.
    // The framework handles visual resizing via placeholder recomputation.
    let resize_handle =
      Node.div(
        ~attrs=[
          Attr.classes(["html-proj-resize-handle"]),
          Attr.on_pointerdown(
            (evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.pointerEvent)) => {
            resize_cols := model.ui.cols;
            resize_rows := model.ui.rows;
            let target =
              evt##.currentTarget
              |> Js_of_ocaml.Js.Opt.get(_, _ => failwith("no target"));
            JsUtil.setPointerCapture(target, evt##.pointerId);
            // wrapper = .html-proj-wrapper, container = .projector
            let wrapper = Js_of_ocaml.Js.Unsafe.coerce(target)##.parentNode;
            wrapper_ref := Some(wrapper);
            let container = wrapper##.parentNode;
            let cw: float = max(1.0, float_of_int(container##.offsetWidth));
            let ch: float = max(1.0, float_of_int(container##.offsetHeight));
            px_per_col := cw /. float_of_int(model.ui.cols);
            px_per_row := ch /. float_of_int(model.ui.rows);
            Effect.Ignore;
          }),
          Attr.on_mousemove(evt => {
            switch (wrapper_ref^) {
            | Some(wrapper) =>
              let container =
                Js_of_ocaml.Js.Unsafe.coerce(wrapper)##.parentNode;
              let rect = container##getBoundingClientRect();
              let left: float = rect##.left;
              let top: float = rect##.top;
              let e = Js_of_ocaml.Js.Unsafe.coerce(evt);
              let client_x: float = float_of_int(e##.clientX);
              let client_y: float = float_of_int(e##.clientY);
              let new_cols =
                max(
                  8,
                  int_of_float(floor((client_x -. left) /. px_per_col^)),
                );
              let new_rows =
                max(
                  3,
                  int_of_float(floor((client_y -. top) /. px_per_row^)),
                );
              if (new_cols != resize_cols^ || new_rows != resize_rows^) {
                resize_cols := new_cols;
                resize_rows := new_rows;
                local(SetDimensions(new_cols, new_rows));
              } else {
                Effect.Ignore;
              };
            | None => Effect.Ignore
            }
          }),
          Attr.on_pointerup(
            (evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.pointerEvent)) => {
            let target =
              evt##.currentTarget
              |> Js_of_ocaml.Js.Opt.get(_, _ => failwith("no target"));
            if (JsUtil.hasPointerCapture(target, evt##.pointerId)) {
              JsUtil.releasePointerCapture(target, evt##.pointerId);
            };
            wrapper_ref := None;
            // Final dispatch in case last mousemove was skipped
            local(SetDimensions(resize_cols^, resize_rows^));
          }),
        ],
        [],
      );

    // Main content
    let content = HazelDOM.go(seed);
    let wrapper_classes = ["html-proj-wrapper"];
    let wrapped =
      Node.div(
        ~attrs=[Attr.classes(wrapper_classes)],
        [content, resize_handle],
      );

    View.mk(wrapped);
  };
};
