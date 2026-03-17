open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

type patchwork_config = {
  doc_url: string,
  tool_id: string,
};

module Decode = {
  let warn = (warnings: ref(list(string)), message: string): unit =>
    warnings := [message, ...warnings^];

  let rec strip_parens = (exp: Exp.t): Exp.t =>
    switch (exp.term) {
    | Parens(inner) => strip_parens(inner)
    | _ => exp
    };

  let string_literal = (exp: Exp.t): option(string) =>
    switch (exp.term) {
    | Atom(String(text)) => Some(text)
    | _ => None
    };

  let decode_tuple =
      (exp: Exp.t, warnings: ref(list(string))): option(patchwork_config) => {
    let normalized: Exp.t = strip_parens(exp);
    switch (normalized.term) {
    | Tuple([doc_exp, tool_exp]) =>
      let doc_opt: option(string) = string_literal(doc_exp);
      let tool_opt: option(string) = string_literal(tool_exp);
      switch (doc_opt, tool_opt) {
      | (Some(doc_url), Some(tool_id)) =>
        Some({
          doc_url,
          tool_id,
        })
      | _ =>
        if (doc_opt == None) {
          warn(
            warnings,
            "Patchwork projector doc-url argument must be a string literal.",
          );
        };
        if (tool_opt == None) {
          warn(
            warnings,
            "Patchwork projector tool-id argument must be a string literal.",
          );
        };
        None;
      };
    | Tuple(_) =>
      warn(
        warnings,
        "Patchwork projector expects exactly two arguments (doc-url, tool-id).",
      );
      None;
    | _ =>
      warn(
        warnings,
        "Patchwork projector arguments must be provided as a tuple literal.",
      );
      None;
    };
  };

  let decode_any =
      (term: Any.t, warnings: ref(list(string))): option(patchwork_config) =>
    switch (term) {
    | Exp(exp_term) => decode_tuple(exp_term, warnings)
    | _ =>
      warn(
        warnings,
        "Patchwork projector is only supported for expression syntax.",
      );
      None;
    };

  let decode_info = (info: info): (option(patchwork_config), list(string)) => {
    let warnings: ref(list(string)) = ref([]);
    let config_opt: option(patchwork_config) =
      switch (info.syntax |> info.utility.seg_to_term) {
      | Some(term) => decode_any(term, warnings)
      | None =>
        warn(
          warnings,
          "Patchwork projector could not read the underlying syntax.",
        );
        None;
      };
    (config_opt, List.rev(warnings^));
  };

  let decode_for_init = (term: Any.t): option(patchwork_config) => {
    let warnings: ref(list(string)) = ref([]);
    decode_any(term, warnings);
  };
};

let warning_list = (warnings: list(string)): Node.t =>
  Node.div(
    ~attrs=[Attr.classes(["patchwork-warning-list"])],
    List.map(
      (message: string) =>
        Node.div(
          ~attrs=[Attr.classes(["patchwork-warning-item"])],
          [Node.text(message)],
        ),
      warnings,
    ),
  );

let patchwork_element = (config: patchwork_config): Node.t =>
  Node.create(
    "patchwork-view",
    ~attrs=[
      Attr.create("doc-url", config.doc_url),
      Attr.create("tool-id", config.tool_id),
    ],
    [],
  );

let error_node = (warnings: list(string)): Node.t => {
  let extra_children: list(Node.t) =
    switch (warnings) {
    | [] => []
    | _ => [warning_list(warnings)]
    };
  Node.div(
    ~attrs=[Attr.classes(["patchwork-error"])],
    [Node.text("Unable to render Patchwork projector.")] @ extra_children,
  );
};

let content_with_warnings = (content: Node.t, warnings: list(string)): Node.t =>
  switch (warnings) {
  | [] => content
  | _ =>
    Node.div(
      ~attrs=[Attr.classes(["patchwork-container"])],
      [content, warning_list(warnings)],
    )
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let default_model: model = ();

  let init = (term: Language.Any.t): option(model) =>
    switch (Decode.decode_for_init(term)) {
    | Some(_) => Some(default_model)
    | None => None
    };

  let focusable: Focusable.t = Focusable.non;

  let dynamics: bool = false;

  let placeholder_size: ProjectorCore.Shape.t = {
    horizontal: 48,
    vertical: Block(18),
  };

  let placeholder = (_model: model, _info: info): ProjectorCore.Shape.t => placeholder_size;

  let update = (model: model, _info: info, _action: action): model => model;

  let view = ({info, status, _}: View.args(model, action)): View.t => {
    let indicated_class: list(string) =
      switch (status.indication) {
      | Some(_) => ["indicated"]
      | None => []
      };
    let class_list: list(string) =
      ["projector", "Patchwork"] @ indicated_class;
    let (config_opt, warnings) = Decode.decode_info(info);
    let body: Node.t =
      switch (config_opt) {
      | Some(config) =>
        let element: Node.t = patchwork_element(config);
        content_with_warnings(element, warnings);
      | None => error_node(warnings)
      };
    View.mk(Node.div(~attrs=[Attr.classes(class_list)], [body]));
  };
};
