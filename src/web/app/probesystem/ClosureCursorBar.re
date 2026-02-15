open Virtual_dom.Vdom;
open Node;
open Util;
open Util.WebUtil;
open Haz3lcore;
open Language;

/* Check if an ID is in user code (present in info_map) */
let is_in_user_code = (~info_map: Statics.Map.t, id: Id.t): bool =>
  Statics.Map.lookup(id, info_map) != None;

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
      /* Look up binding site for the variable.
       * Skip for built-in names: their FixF patterns create context entries
       * with fresh IDs that aren't navigable tiles in the user's zipper. */
      let body_id: option(Id.t) =
        if (Environment.lookup(Builtins.env_init, name) != None) {
          None;
        } else {
          switch (Statics.Map.lookup(fn_id, info_map)) {
          | Some(ci) => Info.get_binding_site(ci)
          | None => None
          };
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
  globals.inject_global(
    ActiveEditor(Project(SampleCursor(TogglePin(pinned_stack)))),
  );

/* Check if any probes exist */
let has_probes = (refractors: Zipper.Refractor.t): bool =>
  !List.is_empty(refractors.manuals)
  || !Id.Map.is_empty(refractors.autos.ids);

/* Walk up the call stack from a given index to find the nearest frame
 * whose app_id is in user code. Used as a fallback for separator clicks
 * when the separator's own app_id comes from built-in internal code. */
let find_nearest_user_app =
    (
      ~info_map: Statics.Map.t,
      ~call_stack: Sample.call_stack,
      ~from_index: int,
    )
    : option(Id.t) => {
  let rec search = (i: int): option(Id.t) =>
    if (i < 0) {
      None;
    } else {
      let frame: Sample.stack_frame = List.nth(call_stack, i);
      is_in_user_code(~info_map, frame.id) ? Some(frame.id) : search(i - 1);
    };
  search(from_index);
};

/* Get the call-site jump target for a breadcrumb entry.
 * If the app_id is in user code, use it directly.
 * Otherwise, walk up the call stack to find the nearest user-visible
 * call site (e.g., for built-in internal calls). */
let get_call_site_target =
    (~info_map: Statics.Map.t, ~call_stack: Sample.call_stack, ~index: int)
    : option(Id.t) => {
  let frame: Sample.stack_frame = List.nth(call_stack, index);
  is_in_user_code(~info_map, frame.id)
    ? Some(frame.id)
    : find_nearest_user_app(~info_map, ~call_stack, ~from_index=index - 1);
};

/* Get the definition jump target for a breadcrumb entry.
 * Used for the "body" icon at the end of the bar.
 * Tier 1: body_id from statics. Tier 2: fn_def_id from Closure.
 * Skips built-in names (non-navigable IDs). */
let get_definition_target =
    (
      ~info_map: Statics.Map.t,
      ~app_id: Id.t,
      ~fn_def_id: option(Id.t),
      ~stack_name: option(string),
    )
    : option(Id.t) => {
  let is_builtin_name =
    switch (stack_name) {
    | Some(n) =>
      let base =
        String.ends_with(~suffix="+", n)
          ? String.sub(n, 0, String.length(n) - 1) : n;
      Environment.lookup(Builtins.env_init, base) != None;
    | None => false
    };
  if (is_builtin_name) {
    None;
  } else {
    let (_, body_id_opt) = get_fn_info(~info_map, app_id);
    switch (body_id_opt) {
    | Some(_) => body_id_opt
    | None => fn_def_id
    };
  };
};

/* Stack icon at the beginning of the bar */
let stack_icon = () =>
  span(~attrs=[Attr.class_("stack-icon")], [text({js|≡|js})]);

/* Keyboard handler for navigation */
let key_handler =
    (
      ~globals: Globals.t,
      ~index: int,
      ~max_index: int,
      ~call_stack: Sample.call_stack,
      ~info_map: Statics.Map.t,
      evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.keyboardEvent),
    ) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);
  switch (key.key) {
  | D("ArrowLeft") =>
    /* Move to shallower level (toward top-level) */
    let new_index = max(-1, index - 1);
    Many([set_cursor_index(~globals, new_index, evt), Stop_propagation]);
  | D("ArrowRight") =>
    /* Move to deeper level (toward innermost call) */
    let new_index = min(max_index, index + 1);
    Many([set_cursor_index(~globals, new_index, evt), Stop_propagation]);
  | D("Enter") =>
    /* Jump to call site of current entry, then refocus main editor. */
    JsUtil.focus_clipboard_shim();
    if (index >= 0 && index < List.length(call_stack)) {
      let target = get_call_site_target(~info_map, ~call_stack, ~index);
      switch (target) {
      | Some(target_id) =>
        Many([jump_to(~globals, target_id, evt), Stop_propagation])
      | None => Stop_propagation
      };
    } else {
      Stop_propagation;
    };
  | _ => Ignore
  };
};

/* Main view function */
let view =
    (
      ~globals: Globals.t,
      ~refractors: Zipper.Refractor.t,
      ~info_map: Statics.Map.t,
      ~indicated_id as _: option(Id.t),
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
        Option.map(
          (f: Sample.stack_frame) => f.id,
          Util.ListUtil.hd_opt(stack),
        )
      );

    /* Top-level entry (always present when bar is shown)
     * Clicking resets cursor to top level (index -1) */
    let top_level_entry =
      span(
        ~attrs=[
          Attr.classes(["top-level"] @ (index == (-1) ? ["focused"] : [])),
          Attr.title("Go to top level"),
          Attr.on_pointerdown(set_cursor_index(~globals, -1)),
        ],
        [text({js|○|js})],
      );

    /* Build breadcrumb entries with separators */
    let entries =
      if (List.is_empty(call_stack)) {
        [top_level_entry];
      } else {
        /* Build entries with index */
        let rec build_entries = (i, remaining) =>
          switch (remaining) {
          | [] => []
          | [{Sample.id: app_id, name: stack_name, fn_def_id: _}, ...rest] =>
            let is_focused = i == index;
            let is_ghost = i > index;
            /* Position class for color coding */
            let position_class =
              i < index ? "above" : i > index ? "below" : "";

            /* Call-site target: jump to where this function was called.
             * If app_id is in user code, use it directly. Otherwise
             * fall back to nearest user-visible call site. */
            let call_site_target =
              is_in_user_code(~info_map, app_id)
                ? Some(app_id)
                : find_nearest_user_app(
                    ~info_map,
                    ~call_stack,
                    ~from_index=i - 1,
                  );
            let display_name =
              switch (stack_name) {
              | Some(name) => Some(name)
              | None =>
                let (name_opt, _) = get_fn_info(~info_map, app_id);
                name_opt;
              };
            let is_unknown = Option.is_none(display_name);
            let display_text =
              is_unknown ? {js|λ|js} : Option.get(display_name);

            /* Check if this entry is indicated (syntax cursor on the app) */
            //let is_indicated = Some(app_id) == indicated_id;

            /* Entry classes */
            let entry_classes =
              ["breadcrumb-entry"]
              @ (is_focused ? ["focused"] : [])
              @ (is_ghost ? ["ghost"] : [])
              @ (is_unknown ? ["unknown"] : [])
              //@ (is_indicated ? ["indicated"] : [])
              @ (position_class != "" ? [position_class] : []);

            /* Entry click handler.
             * Always set cursor index to i. Jump to call site if available. */
            let on_entry_click = evt =>
              switch (call_site_target) {
              | Some(target_id) =>
                Effect.Many([
                  set_cursor_index(~globals, i, evt),
                  jump_to(~globals, target_id, evt),
                ])
              | None => set_cursor_index(~globals, i, evt)
              };

            /* Check if this entry is pinned */
            let is_pinned = Some(app_id) == pinned_head_id;

            /* Pin icon (shown if this entry is pinned, clicking removes pin) */
            let pin_icon =
              switch (is_pinned, pinned_stack) {
              | (true, Some(ps)) => [
                  span(
                    ~attrs=[
                      Attr.class_("pin-icon"),
                      Attr.title("Click to unpin"),
                      Attr.on_pointerdown(evt =>
                        Effect.Many([
                          Effect.Stop_propagation,
                          unpin(~globals, ps, evt),
                        ])
                      ),
                    ],
                    [],
                  ),
                ]
              | _ => []
              };

            let entry_tooltip = "Jump to call site";

            let entry =
              div(
                ~attrs=[
                  Attr.classes(entry_classes),
                  Attr.title(entry_tooltip),
                  Attr.on_pointerdown(on_entry_click),
                ],
                pin_icon @ [text(display_text)],
              );

            /* Separator (decorative) */
            let sep_classes =
              ["breadcrumb-separator"]
              @ (is_ghost ? ["ghost"] : [])
              @ (position_class != "" ? [position_class] : []);
            let sep =
              span(
                ~attrs=[Attr.classes(sep_classes)],
                [text({js|❯|js})],
              );

            [sep, entry, ...build_entries(i + 1, rest)];
          };

        [top_level_entry, ...build_entries(0, call_stack)];
      };

    let max_index = List.length(call_stack) - 1;

    /* Body icon (●) at end: jumps to definition of the deepest function.
     * Gives a way to navigate back inside the innermost function after
     * backtracking via name clicks. */
    let body_icon =
      if (List.is_empty(call_stack)) {
        [];
      } else {
        let last_frame: Sample.stack_frame =
          List.nth(call_stack, List.length(call_stack) - 1);
        let def_target =
          get_definition_target(
            ~info_map,
            ~app_id=last_frame.id,
            ~fn_def_id=last_frame.fn_def_id,
            ~stack_name=last_frame.name,
          );
        let on_body_click = evt =>
          switch (def_target) {
          | Some(target_id) =>
            Effect.Many([
              set_cursor_index(~globals, max_index, evt),
              jump_to(~globals, target_id, evt),
            ])
          | None => set_cursor_index(~globals, max_index, evt)
          };
        [
          span(
            ~attrs=[Attr.classes(["breadcrumb-separator"])],
            [text({js|❯|js})],
          ),
          span(
            ~attrs=[
              Attr.classes(["breadcrumb-body"]),
              Attr.title("Jump to function body"),
              Attr.on_pointerdown(on_body_click),
            ],
            [text({js|●|js})],
          ),
        ];
      };

    div(
      ~attrs=[
        Attr.id("closure-cursor-bar"),
        Attr.tabindex(0),
        Attr.on_keydown(
          key_handler(~globals, ~index, ~max_index, ~call_stack, ~info_map),
        ),
      ],
      [div(~attrs=[Attr.class_("breadcrumbs")], entries @ body_icon)],
    );
  };
