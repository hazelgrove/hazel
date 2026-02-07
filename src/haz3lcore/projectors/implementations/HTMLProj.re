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

// Evaluate a Hazel expression
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

module M: Projector = {
  // UI state for projector sizing
  [@deriving (show({with_path: false}), sexp, yojson)]
  type ui_state = {
    width: option(int),
    height: option(int),
    resizing:
      option(
        [
          | `Width
          | `Height
          | `Both
        ],
      ),
  };

  let default_ui: ui_state = {
    width: None,
    height: None,
    resizing: None,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    exp: Grammar.exp_t(IdTagged.IdTag.t),
    ui: ui_state,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetWidth(int)
    | SetHeight(int)
    | StartResize(
        [
          | `Width
          | `Height
          | `Both
        ],
      )
    | StopResize
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

  // Placeholder shape based on UI state
  let placeholder = (m: model, _) => {
    let width =
      switch (m.ui.width) {
      | Some(w) => w / 8 // Convert pixels to character units (approx)
      | None => 10
      };
    ProjectorCore.Shape.inline(width);
  };

  // Update model based on actions
  let update = (m: model, _, action: action) =>
    switch (action) {
    | SetWidth(w) => {
        ...m,
        ui: {
          ...m.ui,
          width: Some(w),
        },
      }
    | SetHeight(h) => {
        ...m,
        ui: {
          ...m.ui,
          height: Some(h),
        },
      }
    | StartResize(mode) => {
        ...m,
        ui: {
          ...m.ui,
          resizing: Some(mode),
        },
      }
    | StopResize => {
        ...m,
        ui: {
          ...m.ui,
          resizing: None,
        },
      }
    | ResetSize => {
        ...m,
        ui: default_ui,
      }
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

    // Build style string based on UI state
    let size_style = {
      let w =
        switch (model.ui.width) {
        | Some(w) => "width: " ++ string_of_int(w) ++ "px; "
        | None => ""
        };
      let h =
        switch (model.ui.height) {
        | Some(h) => "height: " ++ string_of_int(h) ++ "px; "
        | None => ""
        };
      w ++ h;
    };

    // Resize handle (right edge)
    let resize_handle =
      Node.div(
        ~attrs=[
          Attr.classes(["html-proj-resize-handle"]),
          Attr.create(
            "style",
            "position: absolute; right: 0; top: 0; bottom: 0; width: 6px; cursor: ew-resize; background: transparent;",
          ),
          Attr.on_mousedown(_ => {local(StartResize(`Width))}),
        ],
        [],
      );

    // Main content with optional resize wrapper
    let content = HazelDOM.go(seed);
    let wrapped =
      Node.div(
        ~attrs=[
          Attr.classes(["html-proj-wrapper"]),
          Attr.create(
            "style",
            "position: relative; display: inline-block; " ++ size_style,
          ),
          // Stop resize on mouseup anywhere in the projector
          Attr.on_mouseup(_ => local(StopResize)),
          // Handle resize drag
          Attr.on_mousemove(evt => {
            switch (model.ui.resizing) {
            | Some(`Width) =>
              let rect =
                Js_of_ocaml.Js.Unsafe.coerce(
                  Js_of_ocaml.Dom_html.eventTarget(evt),
                )##getBoundingClientRect();
              let x = Js_of_ocaml.Js.Unsafe.coerce(evt)##.clientX;
              let new_width = max(50, x - rect##.left);
              local(SetWidth(new_width));
            | Some(`Height) =>
              let rect =
                Js_of_ocaml.Js.Unsafe.coerce(
                  Js_of_ocaml.Dom_html.eventTarget(evt),
                )##getBoundingClientRect();
              let y = Js_of_ocaml.Js.Unsafe.coerce(evt)##.clientY;
              let new_height = max(30, y - rect##.top);
              local(SetHeight(new_height));
            | Some(`Both)
            | None => Effect.Ignore
            }
          }),
        ],
        [content, resize_handle],
      );

    View.mk(wrapped);
  };
};
