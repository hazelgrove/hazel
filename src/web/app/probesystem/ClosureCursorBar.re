open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;
open Language;

/* Extract function info from an application ID by looking up in statics.
 *
 * Returns (name_opt, body_id_opt) where:
 * - name_opt: Display name for the function (None for unknown)
 * - body_id_opt: ID of the function body/definition for "go to definition"
 *
 * Note: Returns (None, None) for applications inside built-in function implementations
 * (e.g., recursive calls within `map`). These app_ids aren't in info_map because
 * statics only runs on user surface syntax, not internalized built-in code.
 */
let get_fn_info =
    (~info_map: Statics.Map.t, app_id: Id.t)
    : (option(string), option(Id.t)) =>
  switch (Statics.Map.lookup(app_id, info_map)) {
  | Some(InfoExp({term: {term: Ap(_, fn_exp, _), _}, _})) =>
    let fn_id = Exp.rep_id(fn_exp);
    switch (fn_exp.term) {
    | Var(name) =>
      /* Look up binding site for the variable */
      let body_id =
        switch (Statics.Map.lookup(fn_id, info_map)) {
        | Some(ci) => Info.get_binding_site(ci)
        | None => None
        };
      (Some(name), body_id);
    | Constructor(name, _) =>
      let body_id =
        switch (Statics.Map.lookup(fn_id, info_map)) {
        | Some(ci) => Info.get_binding_site(ci)
        | None => None
        };
      (Some(name), body_id);
    | Fun(_) =>
      /* Anonymous function - the fn_exp itself is the body */
      (Some({js|λ|js}), Some(fn_id))
    | BuiltinFun(name) =>
      /* Built-in functions have no user-visible body */
      (Some(name), None)
    | _ => (Some("fn"), None)
    };
  | _ => (None, None)
  };

/* Jump to syntax location */
let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

/* Check if any probes exist */
let has_probes = (refractors: Zipper.Refractor.t): bool =>
  !List.is_empty(refractors.manuals)
  || !Id.Map.is_empty(refractors.autos.ids);

/* Render a single breadcrumb entry (the function name - clicks go to definition) */
let breadcrumb_entry =
    (
      ~globals: Globals.t,
      ~info_map: Statics.Map.t,
      ~is_focused: bool,
      ~is_ghost: bool,
      app_id: Id.t,
    ) => {
  let (name_opt, body_id_opt) = get_fn_info(~info_map, app_id);
  let is_unknown = Option.is_none(name_opt);
  let display_text = is_unknown ? {js|○|js} : Option.get(name_opt);
  let classes =
    ["breadcrumb-entry"]
    @ (is_focused ? ["focused"] : [])
    @ (is_ghost ? ["ghost"] : [])
    @ (is_unknown ? ["unknown"] : []);
  let attrs =
    [Attr.classes(classes)]
    @ (
      switch (body_id_opt) {
      | Some(body_id) => [Attr.on_pointerdown(jump_to(~globals, body_id))]
      | None => []
      }
    );
  div(~attrs, [text(display_text)]);
};

/* Render the separator arrow (clicks go to application site) */
let separator = (~globals: Globals.t, ~is_ghost: bool, app_id: Id.t) => {
  let classes = ["breadcrumb-separator"] @ (is_ghost ? ["ghost"] : []);
  span(
    ~attrs=[
      Attr.classes(classes),
      Attr.on_pointerdown(jump_to(~globals, app_id)),
    ],
    [text({js|❯|js})] //⟩❯
  );
};

/* Stack icon at the beginning of the bar */
let stack_icon = () =>
  span(~attrs=[Attr.class_("stack-icon")], [text({js|≡|js})]);

/* Main view function */
let view =
    (
      ~globals: Globals.t,
      ~refractors: Zipper.Refractor.t,
      ~info_map: Statics.Map.t,
    )
    : Node.t =>
  /* Only show when probes exist */
  if (!has_probes(refractors)) {
    div(~attrs=[Attr.id("closure-cursor-bar"), Attr.class_("hidden")], []);
  } else {
    let sample_cursor = refractors.sample_cursor;
    let call_stack = sample_cursor.call_stack |> List.rev;
    let index = sample_cursor.index;
    let stack_length = List.length(call_stack);

    /* Top-level λ entry (always present when bar is shown) */
    let top_level_entry =
      span(~attrs=[Attr.class_("top-level")], [text({js|λ|js})]);

    /* Build breadcrumb entries with separators */
    let entries =
      if (List.is_empty(call_stack)) {
        [
          /* Just λ when at top level */
          top_level_entry,
        ];
      } else {
        /* λ followed by separator and stack entries */
        let stack_entries =
          List.mapi(
            (i, app_id) => {
              let is_focused = i == index;
              /* Ghost entries are those beyond the current index */
              let is_ghost = i > index;
              let entry =
                breadcrumb_entry(
                  ~globals,
                  ~info_map,
                  ~is_focused,
                  ~is_ghost,
                  app_id,
                );
              /* Separator before each entry (clicking separator goes to app site) */
              [separator(~globals, ~is_ghost, app_id), entry];
            },
            call_stack,
          )
          |> List.flatten;
        [top_level_entry] @ stack_entries;
      };

    /* Show indicator if there's more below the index */
    let has_more_below = index < stack_length - 1 && stack_length > 0;
    let more_indicator =
      has_more_below
        ? [
          span(
            ~attrs=[Attr.class_("more-indicator")],
            [text({js|░|js})],
          ),
        ]
        : [];

    div(
      ~attrs=[Attr.id("closure-cursor-bar")],
      [
        //stack_icon(),
        div(~attrs=[Attr.class_("breadcrumbs")], entries @ more_indicator),
      ],
    );
  };
