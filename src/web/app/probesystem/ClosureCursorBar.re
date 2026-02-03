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

/* Set sample cursor to a specific index in the call stack */
let set_cursor_index = (~globals: Globals.t, i: int, _) =>
  globals.inject_global(ActiveEditor(Project(SampleCursor(SetIndex(i)))));

/* Remove a pin by toggling it off */
let unpin = (~globals: Globals.t, pinned_stack: Sample.call_stack, _) =>
  globals.inject_global(ActiveEditor(Project(SampleCursor(TogglePin(pinned_stack)))));

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

/* Render the separator arrow (clicks go to application site and set cursor index) */
let separator =
    (~globals: Globals.t, ~is_ghost: bool, ~index: int, app_id: Id.t) => {
  let classes = ["breadcrumb-separator"] @ (is_ghost ? ["ghost"] : []);
  span(
    ~attrs=[
      Attr.classes(classes),
      Attr.on_pointerdown(evt => {
        Effect.Many([
          jump_to(~globals, app_id, evt),
          set_cursor_index(~globals, index, evt),
        ])
      }),
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

    /* Check if there's a pinned stack and get the head app_id */
    let pinned_stack = sample_cursor.pinned_stack;
    let pinned_head_id =
      Option.bind(pinned_stack, stack =>
        Option.map(fst, Util.ListUtil.hd_opt(stack))
      );

    /* Top-level λ entry (always present when bar is shown) */
    let top_level_entry =
      span(~attrs=[Attr.class_("top-level")], [text({js|λ|js})]);

    /* Build breadcrumb entries with separators */
    let entries =
      if (List.is_empty(call_stack)) {
        [top_level_entry];
      } else {
        /* Build entries with index */
        let rec build_entries = (i, remaining) =>
          switch (remaining) {
          | [] => []
          | [(app_id, stack_name), ...rest] =>
            let is_focused = i == index;
            let is_ghost = i > index;
            /* Position class for color coding */
            let position_class =
              i < index ? "above" : i > index ? "below" : "";

            /* Get function info */
            let (_, body_id_opt) = get_fn_info(~info_map, app_id);
            let display_name =
              switch (stack_name) {
              | Some(name) => Some(name)
              | None =>
                let (name_opt, _) = get_fn_info(~info_map, app_id);
                name_opt;
              };
            let is_unknown = Option.is_none(display_name);
            let display_text =
              is_unknown ? {js|○|js} : Option.get(display_name);

            /* Entry classes */
            let entry_classes =
              ["breadcrumb-entry"]
              @ (is_focused ? ["focused"] : [])
              @ (is_ghost ? ["ghost"] : [])
              @ (is_unknown ? ["unknown"] : [])
              @ (position_class != "" ? [position_class] : []);

            /* Entry click handler */
            let on_entry_click = evt =>
              Effect.Many(
                [set_cursor_index(~globals, i, evt)]
                @ (
                  switch (body_id_opt) {
                  | Some(body_id) => [jump_to(~globals, body_id, evt)]
                  | None => []
                  }
                ),
              );

            /* Check if this entry is pinned */
            let is_pinned = Some(app_id) == pinned_head_id;

            /* Pin icon (shown if this entry is pinned, clicking removes pin) */
            let pin_icon =
              switch (is_pinned, pinned_stack) {
              | (true, Some(ps)) =>
                [
                  span(
                    ~attrs=[
                      Attr.class_("pin-icon"),
                      Attr.on_pointerdown(unpin(~globals, ps)),
                    ],
                    [],
                  ),
                ]
              | _ => []
              };

            let entry =
              div(
                ~attrs=[
                  Attr.classes(entry_classes),
                  Attr.on_pointerdown(on_entry_click),
                ],
                pin_icon @ [text(display_text)],
              );

            /* Separator classes */
            let sep_classes =
              ["breadcrumb-separator"]
              @ (is_ghost ? ["ghost"] : [])
              @ (position_class != "" ? [position_class] : []);

            /* Separator click handler */
            let on_sep_click = evt =>
              Effect.Many([
                jump_to(~globals, app_id, evt),
                set_cursor_index(~globals, i, evt),
              ]);

            let sep =
              span(
                ~attrs=[
                  Attr.classes(sep_classes),
                  Attr.on_pointerdown(on_sep_click),
                ],
                [text({js|❯|js})],
              );

            [sep, entry, ...build_entries(i + 1, rest)];
          };

        [top_level_entry, ...build_entries(0, call_stack)];
      };

    div(
      ~attrs=[Attr.id("closure-cursor-bar")],
      [div(~attrs=[Attr.class_("breadcrumbs")], entries)],
    );
  };
