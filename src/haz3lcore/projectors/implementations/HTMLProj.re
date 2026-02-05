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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = Grammar.exp_t(IdTagged.IdTag.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Any.t) =>
    switch (any) {
    // HTML constructor applied to arguments: Div(...), Button(...), etc.
    | Exp({term: Ap(_, {term: Constructor(name, _), _}, _), _} as exp)
        when is_html_constructor(name) =>
      Some(exp)
    // Nullary HTML constructor: Br
    | Exp({term: Constructor("Br", _), _} as exp) => Some(exp)
    // App type: ((HTML, Cmd), HTML -> Sub) tuple
    | Exp(exp) when looks_like_app(exp) => Some(exp)
    | _ => None
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(10);
  let update = (m, _, _) => m;

  let view = ({model, info, parent, view_seg, _}: View.args(model, action)) => {
    let current_model =
      switch (info.syntax |> info.utility.seg_to_term) {
      | Some(Exp(term)) => term
      | _ => model
      };

    let inject = (new_model: model) =>
      parent(SetSyntax(Exp(new_model) |> info.utility.term_to_seg));

    // Check if model is an App type vs plain Html
    let (html_model, subscriptions) =
      switch (detect_app(current_model)) {
      | Some((html, Some(init_cmd), Some(subs_fn))) =>
        // It's an App - run init_cmd and evaluate subscriptions
        let cmd_ctx: CmdRunner.context = {
          model: html,
          inject,
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
        (current_model, None)
      };

    let seed: HazelDOM.t = {
      model: html_model,
      inject,
      view_term: term =>
        Exp(term)
        |> info.utility.term_to_seg
        |> view_seg(~background=false, Exp),
      projector_id: Some(info.id),
      subscriptions,
    };
    View.mk(HazelDOM.go(seed));
  };
};
