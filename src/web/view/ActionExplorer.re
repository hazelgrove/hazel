open Util;
open Haz3lcore;

/* Action Explorer: A developer toolbar for interactive exploration
   of structural edit actions and path/selector resolution.

   Toggled via Nut Menu > Developer > Action Explorer.
   Renders below the top bar. Provides:
   - Action type selector:
     - Path-based: Update/Insert/Delete (with target/direction sub-selectors)
     - Selector-based: Sel Update/Sel Delete/Sel Insert Before/Sel Insert After
     - Read: GetSyntax/GetStatics/GetContext/Select/GetCanonical/GetCompleteness
   - Path text input with live highlight resolution (for path-based actions)
   - Selector text input with live highlight resolution (for selector-based actions)
   - Code text input (for Update/Insert variants)
   - Execute button
   - Result/error display */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action_kind =
    | Update
    | Insert
    | Delete
    | Read
    | SelectorUpdate
    | SelectorDelete
    | SelectorInsertBefore
    | SelectorInsertAfter;

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
    | GetCanonical
    | GetCompleteness;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    action_kind,
    target,
    direction,
    read_kind,
    path: string,
    selector: string,
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
    selector: "",
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
    | SelectorUpdate => Some(SelectorUpdate(model.selector, model.code))
    | SelectorDelete => Some(SelectorDelete(model.selector))
    | SelectorInsertBefore =>
      Some(SelectorInsertBefore(model.selector, model.code))
    | SelectorInsertAfter =>
      Some(SelectorInsertAfter(model.selector, model.code))
    | Read => None /* Read actions handled separately */
    };

  let to_read_action = (model: t): option(CompositionActions.read_action) =>
    switch (model.action_kind) {
    | Read =>
      switch (model.read_kind) {
      | GetSyntax => Some(GetSyntax(model.path))
      | GetStatics => Some(GetStatics(model.path))
      | GetContext => Some(GetContext(model.path))
      | Select => Some(Select(model.selector))
      | GetCanonical => Some(GetCanonical(model.selector))
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
    | SetSelector(string)
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
    | SetSelector(selector) => {
        ...model,
        selector,
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
      | SelectorUpdate => "sel_update"
      | SelectorDelete => "sel_delete"
      | SelectorInsertBefore => "sel_insert_before"
      | SelectorInsertAfter => "sel_insert_after"
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
          ("sel_update", "Sel Update"),
          ("sel_delete", "Sel Delete"),
          ("sel_insert_before", "Sel Insert Before"),
          ("sel_insert_after", "Sel Insert After"),
        ],
        ~on_change=v =>
        inject(
          SetActionKind(
            switch (v) {
            | "update" => Update
            | "insert" => Insert
            | "delete" => Delete
            | "sel_update" => SelectorUpdate
            | "sel_delete" => SelectorDelete
            | "sel_insert_before" => SelectorInsertBefore
            | "sel_insert_after" => SelectorInsertAfter
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
      | GetCanonical => "get_canonical"
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
          ("get_canonical", "GetCanonical"),
          ("get_completeness", "GetCompleteness"),
        ],
        ~on_change=v =>
        inject(
          SetReadKind(
            switch (v) {
            | "get_statics" => GetStatics
            | "get_context" => GetContext
            | "select" => Select
            | "get_canonical" => GetCanonical
            | "get_completeness" => GetCompleteness
            | _ => GetSyntax
            },
          ),
        )
      );

    let path_input =
      text_field(
        ~clss="ae-path",
        ~placeholder="path (e.g. x, x/y, #0, |A, [0])",
        ~value=model.path,
        ~on_input=v =>
        inject(SetPath(v))
      );

    let selector_input =
      text_field(
        ~clss="ae-selector",
        ~placeholder="selector (e.g. let _ = * in _, _ \\... *)",
        ~value=model.selector,
        ~on_input=v =>
        inject(SetSelector(v))
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
      | SelectorUpdate => [
          action_select,
          selector_input,
          code_input,
          execute_button,
          highlight_count,
        ]
      | SelectorDelete => [
          action_select,
          selector_input,
          execute_button,
          highlight_count,
        ]
      | SelectorInsertBefore
      | SelectorInsertAfter => [
          action_select,
          selector_input,
          code_input,
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
        | Select
        | GetCanonical => [
            action_select,
            read_kind_select,
            selector_input,
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
