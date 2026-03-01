open Util;
open Haz3lcore;

/* Action Explorer: A developer toolbar for interactive exploration
   of structural edit actions and path/selector resolution.

   Toggled via Nut Menu > Developer > Action Explorer.
   Renders below the top bar. Provides:
   - Action type selector (Update/Insert/Delete)
   - Target selector (Definition/Body/Pattern/BindingClause/TypeAnnotation)
   - Direction selector (Before/After, for Insert only)
   - Path text input with live highlight resolution
   - Code text input (for Update/Insert)
   - Execute button
   - Result/error display */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action_kind =
    | Update
    | Insert
    | Delete
    | Read;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type target =
    | Definition
    | Body
    | Pattern
    | BindingClause
    | TypeAnnotation;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type direction =
    | Before
    | After;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type read_kind =
    | GetSyntax
    | GetStatics
    | GetContext
    | Select
    | GetCompleteness;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    action_kind,
    target,
    direction,
    read_kind,
    path: string,
    code: string,
    highlight_ids: list(Id.t),
    result_msg: option(string),
  };

  let init = {
    action_kind: Read,
    target: Body,
    direction: Before,
    read_kind: GetSyntax,
    path: "",
    code: "",
    highlight_ids: [],
    result_msg: None,
  };

  let to_structural_action = (model: t): option(Action.Structural.t) =>
    switch (model.action_kind) {
    | Update =>
      let target =
        switch (model.target) {
        | Definition => Action.Structural.Definition
        | Body => Body
        | Pattern => Pattern
        | BindingClause => BindingClause
        | TypeAnnotation => TypeAnnotation
        };
      Some(Update(target, model.path, model.code));
    | Insert =>
      let dir =
        switch (model.direction) {
        | Before => Action.Structural.Before
        | After => After
        };
      Some(Insert(dir, model.path, model.code));
    | Delete =>
      let target =
        switch (model.target) {
        | Definition => Action.Structural.Definition
        | Body => Body
        | Pattern => Pattern
        | BindingClause => BindingClause
        | TypeAnnotation => TypeAnnotation
        };
      Some(Delete(target, model.path));
    | Read => None /* Read actions handled separately */
    };

  let to_read_action = (model: t): option(CompositionActions.read_action) =>
    switch (model.action_kind) {
    | Read =>
      switch (model.read_kind) {
      | GetSyntax => Some(GetSyntax(model.path))
      | GetStatics => Some(GetStatics(model.path))
      | GetContext => Some(GetContext(model.path))
      | Select => Some(Select(model.path))
      | GetCompleteness => Some(GetCompleteness)
      }
    | _ => None
    };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetActionKind(Model.action_kind)
    | SetTarget(Model.target)
    | SetDirection(Model.direction)
    | SetReadKind(Model.read_kind)
    | SetPath(string)
    | SetCode(string)
    | SetHighlightIds(list(Id.t))
    | SetResult(option(string))
    | Execute;

  let update = (~action: t, ~model: Model.t): Model.t =>
    switch (action) {
    | SetActionKind(action_kind) => {
        ...model,
        action_kind,
      }
    | SetTarget(target) => {
        ...model,
        target,
      }
    | SetDirection(direction) => {
        ...model,
        direction,
      }
    | SetReadKind(read_kind) => {
        ...model,
        read_kind,
      }
    | SetPath(path) => {
        ...model,
        path,
      }
    | SetCode(code) => {
        ...model,
        code,
      }
    | SetHighlightIds(highlight_ids) => {
        ...model,
        highlight_ids,
      }
    | SetResult(result_msg) => {
        ...model,
        result_msg,
      }
    | Execute => model /* Handled at Page level */
    };
};

module View = {
  open Virtual_dom.Vdom;
  open Node;
  open Util.WebUtil;

  let select_input =
      (
        ~clss: string,
        ~value: string,
        ~options: list((string, string)),
        ~on_change: string => Ui_effect.t(unit),
      ) =>
    select(
      ~attrs=[Attr.class_(clss), Attr.on_change((_, v) => on_change(v))],
      List.map(
        ((v, label)) =>
          option(
            ~attrs=[
              Attr.value(v),
              Attr.bool_property("selected", v == value),
            ],
            [text(label)],
          ),
        options,
      ),
    );

  let text_field =
      (
        ~clss: string,
        ~placeholder: string,
        ~value: string,
        ~on_input: string => Ui_effect.t(unit),
      ) =>
    input(
      ~attrs=[
        Attr.class_(clss),
        Attr.type_("text"),
        Attr.placeholder(placeholder),
        Attr.value(value),
        Attr.on_input((_, v) => on_input(v)),
        /* Stop propagation so typing doesn't trigger editor keybindings */
        Attr.on_keydown(evt => {
          Js_of_ocaml.Dom_html.stopPropagation(evt);
          Effect.Ignore;
        }),
        Attr.on_keypress(evt => {
          Js_of_ocaml.Dom_html.stopPropagation(evt);
          Effect.Ignore;
        }),
        Attr.on_keyup(evt => {
          Js_of_ocaml.Dom_html.stopPropagation(evt);
          Effect.Ignore;
        }),
      ],
      (),
    );

  let view = (~inject: Update.t => Ui_effect.t(unit), model: Model.t) => {
    let action_kind_str =
      switch (model.action_kind) {
      | Update => "update"
      | Insert => "insert"
      | Delete => "delete"
      | Read => "read"
      };

    let target_str =
      switch (model.target) {
      | Definition => "definition"
      | Body => "body"
      | Pattern => "pattern"
      | BindingClause => "binding_clause"
      | TypeAnnotation => "type_annotation"
      };

    let direction_str =
      switch (model.direction) {
      | Before => "before"
      | After => "after"
      };

    let action_select =
      select_input(
        ~clss="ae-action-kind",
        ~value=action_kind_str,
        ~options=[
          ("read", "Read"),
          ("update", "Update"),
          ("insert", "Insert"),
          ("delete", "Delete"),
        ],
        ~on_change=v =>
        inject(
          SetActionKind(
            switch (v) {
            | "update" => Update
            | "insert" => Insert
            | "delete" => Delete
            | _ => Read
            },
          ),
        )
      );

    let target_select =
      select_input(
        ~clss="ae-target",
        ~value=target_str,
        ~options=[
          ("body", "Body"),
          ("definition", "Definition"),
          ("pattern", "Pattern"),
          ("binding_clause", "Binding Clause"),
          ("type_annotation", "Type Annotation"),
        ],
        ~on_change=v =>
        inject(
          SetTarget(
            switch (v) {
            | "definition" => Definition
            | "pattern" => Pattern
            | "binding_clause" => BindingClause
            | "type_annotation" => TypeAnnotation
            | _ => Body
            },
          ),
        )
      );

    let direction_select =
      select_input(
        ~clss="ae-direction",
        ~value=direction_str,
        ~options=[("before", "Before"), ("after", "After")],
        ~on_change=v =>
        inject(
          SetDirection(
            switch (v) {
            | "after" => After
            | _ => Before
            },
          ),
        )
      );

    let read_kind_str =
      switch (model.read_kind) {
      | GetSyntax => "get_syntax"
      | GetStatics => "get_statics"
      | GetContext => "get_context"
      | Select => "select"
      | GetCompleteness => "get_completeness"
      };

    let read_kind_select =
      select_input(
        ~clss="ae-read-kind",
        ~value=read_kind_str,
        ~options=[
          ("get_syntax", "GetSyntax"),
          ("get_statics", "GetStatics"),
          ("get_context", "GetContext"),
          ("select", "Select"),
          ("get_completeness", "GetCompleteness"),
        ],
        ~on_change=v =>
        inject(
          SetReadKind(
            switch (v) {
            | "get_statics" => GetStatics
            | "get_context" => GetContext
            | "select" => Select
            | "get_completeness" => GetCompleteness
            | _ => GetSyntax
            },
          ),
        )
      );

    let path_placeholder =
      switch (model.action_kind, model.read_kind) {
      | (Read, Select) => "selector (e.g. let _ = * in _, _ \\... *)"
      | _ => "path (e.g. x, x/y, #0, |A, [0])"
      };

    let path_input =
      text_field(
        ~clss="ae-path",
        ~placeholder=path_placeholder,
        ~value=model.path,
        ~on_input=v =>
        inject(SetPath(v))
      );

    let code_input =
      text_field(
        ~clss="ae-code", ~placeholder="code", ~value=model.code, ~on_input=v =>
        inject(SetCode(v))
      );

    let execute_button =
      div(
        ~attrs=[
          clss(["ae-execute"]),
          Attr.on_click(_ => inject(Execute)),
          Attr.title("Execute action"),
        ],
        [text("Run")],
      );

    let result_display =
      switch (model.result_msg) {
      | None => none
      | Some(msg) => div(~attrs=[clss(["ae-result"])], [text(msg)])
      };

    let highlight_count =
      switch (model.highlight_ids) {
      | [] => none
      | ids =>
        let n = List.length(ids);
        div(
          ~attrs=[clss(["ae-match-count"])],
          [text(string_of_int(n) ++ (n == 1 ? " match" : " matches"))],
        );
      };

    /* Build the controls row based on action kind */
    let controls =
      switch (model.action_kind) {
      | Update => [
          action_select,
          target_select,
          path_input,
          code_input,
          execute_button,
          highlight_count,
        ]
      | Insert => [
          action_select,
          direction_select,
          path_input,
          code_input,
          execute_button,
          highlight_count,
        ]
      | Delete => [
          action_select,
          target_select,
          path_input,
          execute_button,
          highlight_count,
        ]
      | Read =>
        switch (model.read_kind) {
        | GetCompleteness => [
            action_select,
            read_kind_select,
            execute_button,
            highlight_count,
          ]
        | _ => [
            action_select,
            read_kind_select,
            path_input,
            execute_button,
            highlight_count,
          ]
        }
      };

    div(
      ~attrs=[Attr.id("action-explorer")],
      [div(~attrs=[clss(["ae-controls"])], controls), result_display],
    );
  };
};
