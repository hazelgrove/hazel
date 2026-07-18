open Util;
open Haz3lcore;

/* Action Explorer: A developer toolbar for interactive exploration
   of structural edit actions and path/selector resolution.

   Toggled via Nut Menu > Developer > Action Explorer.
   Renders below the top bar. Provides two top-level tiers:

   Selector (default):
     Read (default): Syntax / Statics / Context / Canonical
     Update / Delete / Overwrite ($)

   Original:
     Read: Syntax / Statics / Context / Completeness
     Update / Delete

   With text inputs for path/selector/code as appropriate,
   execute button, and result/error display. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type tier =
    | Selector
    | Original;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action_kind =
    | Update
    | Delete
    | Read
    | SelectorRead
    | SelectorUpdate
    | SelectorDelete
    | Overwrite;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type target =
    | Definition
    | Body
    | Pattern
    | BindingClause
    | TypeAnnotation;

  /* Read sub-kinds for the Original tier (path-based) */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type read_kind =
    | GetSyntax
    | GetStatics
    | GetContext
    | GetCompleteness;

  /* Read sub-kinds for the Selector tier */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type selector_read_kind =
    | SelSyntax
    | SelStatics
    | SelContext
    | SelCanonical;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    tier,
    action_kind,
    target,
    read_kind,
    selector_read_kind,
    path: string,
    selector: string,
    code: string,
    highlight_ids: list(Id.t),
    active_match_index: int,
    active_match_count: int,
    active_match_id: option(Id.t),
    result_msg: option(string),
  };

  let init = {
    tier: Selector,
    action_kind: SelectorRead,
    target: Body,
    read_kind: GetSyntax,
    selector_read_kind: SelSyntax,
    path: "",
    selector: "",
    code: "",
    highlight_ids: [],
    active_match_index: 0,
    active_match_count: 0,
    active_match_id: None,
    result_msg: None,
  };

  /* Returns true when the current action_kind is a read variant */
  let is_read = (model: t): bool =>
    switch (model.action_kind) {
    | Read
    | SelectorRead => true
    | _ => false
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
    | Overwrite => Some(Overwrite(model.selector, model.code))
    | Read
    | SelectorRead => None /* Read actions handled separately */
    };

  let to_read_action = (model: t): option(CompositionActions.read_action) =>
    switch (model.action_kind) {
    | Read =>
      switch (model.read_kind) {
      | GetSyntax => Some(GetSyntax(model.path))
      | GetStatics => Some(GetStatics(model.path))
      | GetContext => Some(GetContext(model.path))
      | GetCompleteness => Some(GetCompleteness)
      }
    | SelectorRead =>
      switch (model.selector_read_kind) {
      | SelSyntax => Some(Select(model.selector))
      | SelStatics => Some(SelectorGetStatics(model.selector))
      | SelContext => Some(SelectorGetContext(model.selector))
      | SelCanonical => Some(GetCanonical(model.selector))
      }
    | _ => None
    };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SetTier(Model.tier)
    | SetActionKind(Model.action_kind)
    | SetTarget(Model.target)
    | SetReadKind(Model.read_kind)
    | SetSelectorReadKind(Model.selector_read_kind)
    | SetPath(string)
    | SetSelector(string)
    | SetCode(string)
    | SetHighlightIds(list(Id.t))
    | SetResult(option(string))
    | PrevMatch
    | NextMatch
    | Execute;

  let update = (~action: t, ~model: Model.t): Model.t =>
    switch (action) {
    | SetTier(tier) =>
      /* When switching tiers, select the default action_kind for that tier */
      let action_kind =
        switch (tier) {
        | Selector => Model.SelectorRead
        | Original => Read
        };
      {
        ...model,
        tier,
        action_kind,
      };
    | SetActionKind(action_kind) => {
        ...model,
        action_kind,
      }
    | SetTarget(target) => {
        ...model,
        target,
      }
    | SetReadKind(read_kind) => {
        ...model,
        read_kind,
      }
    | SetSelectorReadKind(selector_read_kind) => {
        ...model,
        selector_read_kind,
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
    | PrevMatch
    | NextMatch => model /* Handled at Page level */
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
        ~on_keydown,
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
          on_keydown(evt);
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
    /* --- Tier selector (top-level: Selector vs Original) --- */
    let tier_str =
      switch (model.tier) {
      | Selector => "selector"
      | Original => "original"
      };

    let tier_select =
      select_input(
        ~clss="ae-tier",
        ~value=tier_str,
        ~options=[("selector", "Selector"), ("original", "Original")],
        ~on_change=v =>
        inject(
          SetTier(
            switch (v) {
            | "original" => Original
            | _ => Selector
            },
          ),
        )
      );

    /* --- Action kind selector (varies by tier) --- */
    let action_kind_str =
      switch (model.action_kind) {
      | Update => "update"
      | Delete => "delete"
      | Read => "read"
      | SelectorRead => "sel_read"
      | SelectorUpdate => "sel_update"
      | SelectorDelete => "sel_delete"
      | Overwrite => "overwrite"
      };

    let action_options =
      switch (model.tier) {
      | Selector => [
          ("sel_read", "Read"),
          ("sel_update", "Update"),
          ("sel_delete", "Delete"),
          ("overwrite", "Overwrite ($)"),
        ]
      | Original => [
          ("read", "Read"),
          ("update", "Update"),
          ("delete", "Delete"),
        ]
      };

    let action_select =
      select_input(
        ~clss="ae-action-kind",
        ~value=action_kind_str,
        ~options=action_options,
        ~on_change=v =>
        inject(
          SetActionKind(
            switch (v) {
            | "update" => Update
            | "delete" => Delete
            | "read" => Read
            | "sel_read" => SelectorRead
            | "sel_update" => SelectorUpdate
            | "sel_delete" => SelectorDelete
            | "overwrite" => Overwrite
            | _ => Read
            },
          ),
        )
      );

    /* --- Target selector (for Original Update/Delete) --- */
    let target_str =
      switch (model.target) {
      | Definition => "definition"
      | Body => "body"
      | Pattern => "pattern"
      | BindingClause => "binding_clause"
      | TypeAnnotation => "type_annotation"
      };

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

    /* --- Read kind selector (for Original > Read) --- */
    let read_kind_str =
      switch (model.read_kind) {
      | GetSyntax => "get_syntax"
      | GetStatics => "get_statics"
      | GetContext => "get_context"
      | GetCompleteness => "get_completeness"
      };

    let read_kind_select =
      select_input(
        ~clss="ae-read-kind",
        ~value=read_kind_str,
        ~options=[
          ("get_syntax", "Syntax"),
          ("get_statics", "Statics"),
          ("get_context", "Context"),
          ("get_completeness", "Completeness"),
        ],
        ~on_change=v =>
        inject(
          SetReadKind(
            switch (v) {
            | "get_statics" => GetStatics
            | "get_context" => GetContext
            | "get_completeness" => GetCompleteness
            | _ => GetSyntax
            },
          ),
        )
      );

    /* --- Selector read kind selector (for Selector > Read) --- */
    let selector_read_kind_str =
      switch (model.selector_read_kind) {
      | SelSyntax => "sel_syntax"
      | SelStatics => "sel_statics"
      | SelContext => "sel_context"
      | SelCanonical => "sel_canonical"
      };

    let selector_read_kind_select =
      select_input(
        ~clss="ae-selector-read-kind",
        ~value=selector_read_kind_str,
        ~options=[
          ("sel_syntax", "Syntax"),
          ("sel_statics", "Statics"),
          ("sel_context", "Context"),
          ("sel_canonical", "Canonical"),
        ],
        ~on_change=v =>
        inject(
          SetSelectorReadKind(
            switch (v) {
            | "sel_statics" => SelStatics
            | "sel_context" => SelContext
            | "sel_canonical" => SelCanonical
            | _ => SelSyntax
            },
          ),
        )
      );

    /* --- Text inputs --- */
    let path_input =
      text_field(
        ~clss="ae-path",
        ~placeholder="path (e.g. x, x/y, #0, |A, [0])",
        ~value=model.path,
        ~on_input=v => inject(SetPath(v)),
        ~on_keydown=_ => Effect.Ignore,
      );

    let selector_input =
      text_field(
        ~clss="ae-selector",
        ~placeholder="selector (e.g. let _ = * in _, _ \\... *)",
        ~value=model.selector,
        ~on_input=v => inject(SetSelector(v)),
        ~on_keydown=
          evt => {
            let key = Key.get_key(evt);
            switch (key) {
            | "ArrowUp" =>
              Js_of_ocaml.Dom.preventDefault(evt);
              inject(PrevMatch);
            | "ArrowDown" =>
              Js_of_ocaml.Dom.preventDefault(evt);
              inject(NextMatch);
            | "Enter" =>
              Js_of_ocaml.Dom.preventDefault(evt);
              inject(Execute);
            | _ => Effect.Ignore
            };
          },
      );

    let code_input =
      text_field(
        ~clss="ae-code",
        ~placeholder="code",
        ~value=model.code,
        ~on_input=v => inject(SetCode(v)),
        ~on_keydown=_ => Effect.Ignore,
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

    let cycle_disabled = model.active_match_count == 0;
    let cycle_button = (label, title, action) =>
      div(
        ~attrs=[
          clss(["ae-cycle-button"] @ (cycle_disabled ? ["disabled"] : [])),
          Attr.on_click(_ => cycle_disabled ? Effect.Ignore : inject(action)),
          Attr.title(title),
        ],
        [text(label)],
      );
    let cycle_buttons =
      div(
        ~attrs=[clss(["ae-cycle-buttons"])],
        [
          cycle_button("▲", "Previous selector match", PrevMatch),
          cycle_button("▼", "Next selector match", NextMatch),
        ],
      );

    let result_display =
      switch (model.result_msg) {
      | None => none
      | Some(msg) => div(~attrs=[clss(["ae-result"])], [text(msg)])
      };

    let highlight_count =
      if (model.tier == Selector && model.active_match_count > 0) {
        div(
          ~attrs=[clss(["ae-match-count"])],
          [
            text(
              string_of_int(model.active_match_index)
              ++ " of "
              ++ string_of_int(model.active_match_count),
            ),
          ],
        );
      } else {
        switch (model.highlight_ids) {
        | [] => none
        | ids =>
          let n = List.length(ids);
          div(
            ~attrs=[clss(["ae-match-count"])],
            [text(string_of_int(n) ++ (n == 1 ? " match" : " matches"))],
          );
        };
      };

    /* Build the controls row based on tier and action kind */
    let controls =
      switch (model.action_kind) {
      /* --- Selector tier --- */
      | SelectorRead => [
          tier_select,
          action_select,
          selector_read_kind_select,
          selector_input,
          cycle_buttons,
          execute_button,
          highlight_count,
        ]
      | SelectorUpdate => [
          tier_select,
          action_select,
          selector_input,
          cycle_buttons,
          code_input,
          execute_button,
          highlight_count,
        ]
      | SelectorDelete => [
          tier_select,
          action_select,
          selector_input,
          cycle_buttons,
          execute_button,
          highlight_count,
        ]
      | Overwrite => [
          tier_select,
          action_select,
          selector_input,
          cycle_buttons,
          code_input,
          execute_button,
          highlight_count,
        ]
      /* --- Original tier --- */
      | Read =>
        switch (model.read_kind) {
        | GetCompleteness => [
            tier_select,
            action_select,
            read_kind_select,
            execute_button,
            highlight_count,
          ]
        | _ => [
            tier_select,
            action_select,
            read_kind_select,
            path_input,
            execute_button,
            highlight_count,
          ]
        }
      | Update => [
          tier_select,
          action_select,
          target_select,
          path_input,
          code_input,
          execute_button,
          highlight_count,
        ]
      | Delete => [
          tier_select,
          action_select,
          target_select,
          path_input,
          execute_button,
          highlight_count,
        ]
      };

    div(
      ~attrs=[Attr.id("action-explorer")],
      [div(~attrs=[clss(["ae-controls"])], controls), result_display],
    );
  };
};
