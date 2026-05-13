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
  | Some(InfoExp({user_term: {term: Ap(_, fn_exp, _), _}, _})) =>
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

/* Set sample focus to a specific index in the call stack */
let set_focus_index = (~globals: Globals.t, i: int, _) =>
  globals.inject_global(ActiveEditor(Project(SampleFocus(SetIndex(i)))));

/* Remove a pin by toggling it off */
let unpin = (~globals: Globals.t, pinned_stack: Sample.call_stack, _) =>
  globals.inject_global(
    ActiveEditor(Project(SampleFocus(TogglePin(pinned_stack)))),
  );

/* Toggle anti-pin (inner bound) at a depth index */
let toggle_anti_pin = (~globals: Globals.t, depth: int, _) =>
  globals.inject_global(
    ActiveEditor(Project(SampleFocus(ToggleAntiPin(depth)))),
  );

/* Check if any probes exist */
let has_probes = (refractors: Zipper.Refractor.t): bool =>
  !List.is_empty(refractors.manuals)
  || !Id.Map.is_empty(refractors.multis.ids);

/* Tree view mode toggle: single-path (default) or tree */
[@deriving (show({with_path: false}), sexp, yojson)]
type view_mode =
  | SinglePath
  | TreeView;

let view_mode_ref: ref(view_mode) = ref(SinglePath);

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

/* Get the parameter pattern target for a breadcrumb's body icon.
 * Looks up the function literal in info_map and extracts the
 * parameter pattern ID from Fun(pat, _, _, _). */
let get_param_target =
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
    /* First, find the function definition ID */
    let fn_id_opt =
      switch (fn_def_id) {
      | Some(_) => fn_def_id
      | None =>
        let (_, body_id_opt) = get_fn_info(~info_map, app_id);
        body_id_opt;
      };
    switch (fn_id_opt) {
    | None => None
    | Some(fn_id) =>
      switch (Statics.Map.lookup(fn_id, info_map)) {
      | Some(InfoExp({user_term: {term: Fun(pat, _, _, _), _}, _})) =>
        Some(Pat.rep_id(pat))
      | _ => None
      }
    };
  };
};

/* Stack icon at the beginning of the bar */
let stack_icon = () =>
  span(~attrs=[Attr.class_("stack-icon")], [text({js|≡|js})]);

/* Windowed breadcrumb display: when the call stack has more entries than
 * fit in the available space, we show entry 0 (outermost call), a sliding
 * window around the focused entry, and ellipsis markers for collapsed ranges.
 *
 * The window slides as the user navigates with arrow keys or clicks. */
type visible_item =
  | Entry(int)
  | Ellipsis;

/* Resolve the display name for a call stack frame */
let resolve_display_name =
    (~info_map: Statics.Map.t, frame: Sample.stack_frame): string =>
  switch (frame.name) {
  | Some(name) => name
  | None =>
    let (name_opt, _) = get_fn_info(~info_map, frame.id);
    switch (name_opt) {
    | Some(name) => name
    | None => {js|λ|js}
    };
  };

/* Character cost of a separator (❯ + surrounding gaps) */
let separator_chars = 3;

/* Total character cost of a visible item set */
let visible_char_cost =
    (items: list(visible_item), names: array(string)): int =>
  List.fold_left(
    (acc, item) =>
      acc
      + (
        switch (item) {
        | Entry(i) => separator_chars + Unicode.length(names[i])
        | Ellipsis => separator_chars + 1 /* ❯ + ⋯ */
        }
      ),
    0,
    items,
  );

/* Fixed character overhead for non-entry elements in the bar:
 * top-level icon (2) + body icon (3) + clear-all button (~12) + padding (3) */
let bar_overhead_chars = 20;

let compute_visible = (~n: int, ~focus: int, ~cap: int): list(visible_item) =>
  if (n == 0) {
    [];
  } else if (n <= cap) {
    List.init(n, i => Entry(i));
  } else if (cap <= 0) {
    [];
  } else if (cap == 1) {
    [Entry(0)];
  } else if (cap == 2) {
    [Entry(0), Ellipsis];
  } else {
    /* cap >= 3, n > cap */
    let effective_focus = max(focus, 0);
    if (effective_focus <= cap - 2) {
      /* Case A: Focus near start — right ellipsis only */
      List.init(cap - 1, i => Entry(i)) @ [Ellipsis];
    } else if (effective_focus >= n - (cap - 2)) {
      /* Case B: Focus near end — entry 0, left ellipsis, tail */
      let window_start = n - (cap - 2);
      [Entry(0), Ellipsis]
      @ List.init(cap - 2, i => Entry(window_start + i));
    } else if (cap == 3) {
      [
        /* Case C special: cap=3 middle — can only show entry 0 + focus */
        Entry(0),
        Ellipsis,
        Entry(effective_focus),
      ];
    } else {
      /* Case C general: both ellipses, window centered on focus */
      let window_size = cap - 3;
      let half_left = (window_size - 1) / 2;
      let window_start = effective_focus - half_left;
      let window_end = window_start + window_size - 1;
      /* Safety clamps */
      let (window_start, window_end) =
        if (window_start <= 1) {
          (2, 1 + window_size);
        } else if (window_end >= n - 1) {
          (n - 1 - window_size, n - 2);
        } else {
          (window_start, window_end);
        };
      [Entry(0), Ellipsis]
      @ List.init(window_end - window_start + 1, i =>
          Entry(window_start + i)
        )
      @ [Ellipsis];
    };
  };

/* Find the largest cap where the selected entries fit within the budget.
 * Searches from n down to 3 — since higher cap always means more characters,
 * the first cap that fits is optimal. */
let compute_dynamic_cap =
    (~names: array(string), ~focus: int, ~budget: int): int => {
  let n = Array.length(names);
  let rec find = (cap: int): int =>
    if (cap <= 2) {
      max(1, min(3, n));
    } else {
      let items = compute_visible(~n, ~focus, ~cap);
      if (visible_char_cost(items, names) <= budget) {
        cap;
      } else {
        find(cap - 1);
      };
    };
  find(n);
};

/* Navigate to a sibling branch at the focused depth.
 * Replaces the frame at view_index in the (outermost-first) call stack
 * with the new sibling frame and dispatches a Capture to update the sightline. */
let switch_sibling =
    (
      ~globals: Globals.t,
      ~call_stack_rev: Sample.call_stack,
      ~view_index: int,
      new_frame: Sample.stack_frame,
      evt,
    ) => {
  let n = List.length(call_stack_rev);
  if (view_index < 0 || view_index >= n) {
    Effect.Ignore;
  } else {
    /* Build new outermost-first stack with the frame at view_index replaced */
    let new_stack_rev =
      List.mapi(
        (i, f) => i == view_index ? new_frame : f,
        call_stack_rev,
      );
    /* Truncate the stack at the switch point: keep prefix + new frame,
     * drop everything deeper since the subtree may differ */
    let truncated_rev =
      Util.ListUtil.slice(0, view_index + 1, new_stack_rev);
    /* Convert back to innermost-first for the Capture data */
    let new_stack = List.rev(truncated_rev);
    let capture_data: Sample.Capture.t = {
      call_stack: new_stack,
      time: 0.0,
      seq: 0,
      step_start: 0,
      step_end: 0,
    };
    Effect.Many([
      globals.inject_global(
        ActiveEditor(
          Project(SampleFocus(Capture(capture_data, None))),
        ),
      ),
      Effect.Stop_propagation,
    ]);
  };
};

/* Keyboard handler for navigation */
let key_handler =
    (
      ~globals: Globals.t,
      ~index: int,
      ~max_index: int,
      ~call_stack: Sample.call_stack,
      ~call_stack_rev: Sample.call_stack,
      ~probe_map: Language.Dynamics.Map.t,
      ~info_map: Statics.Map.t,
      evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.keyboardEvent),
    ) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);
  /* Jump to the call site at a given index in call_stack_rev (outermost-first).
   * Falls back to nearest user-visible call site if the frame is internal. */
  let jump_to_index = (i: int): Ui_effect.t(unit) =>
    if (i >= 0 && i < List.length(call_stack_rev)) {
      /* call_stack_rev is outermost-first; get_call_site_target expects
       * the same ordering as call_stack (also outermost-first here) */
      let target =
        get_call_site_target(~info_map, ~call_stack=call_stack_rev, ~index=i);
      switch (target) {
      | Some(target_id) => jump_to(~globals, target_id, evt)
      | None => Effect.Ignore
      };
    } else {
      Effect.Ignore;
    };
  switch (key.key) {
  | D("ArrowLeft") =>
    /* Move to shallower level (toward top-level) */
    let new_index = max(-1, index - 1);
    Many([
      set_focus_index(~globals, new_index, evt),
      jump_to_index(new_index),
      Stop_propagation,
    ]);
  | D("ArrowRight") =>
    /* Move to deeper level (toward innermost call) */
    let new_index = min(max_index, index + 1);
    Many([
      set_focus_index(~globals, new_index, evt),
      jump_to_index(new_index),
      Stop_propagation,
    ]);
  | D("ArrowUp") | D("ArrowDown") =>
    /* Navigate between sibling branches at the focused depth. */
    let n = List.length(call_stack_rev);
    let view_index = index;
    if (view_index < 0 || view_index >= n) {
      Stop_propagation;
    } else {
      let siblings =
        Sample.Selection.find_siblings(
          ~call_stack_rev,
          ~view_index,
          probe_map,
        );
      let current_frame = List.nth(call_stack_rev, view_index);
      let current_pos =
        List.find_index(
          f => Sample.equal_stack_frame(f, current_frame),
          siblings,
        );
      let direction = key.key == D("ArrowUp") ? (-1) : 1;
      switch (current_pos) {
      | Some(pos) =>
        let next_pos =
          (pos + direction + List.length(siblings))
          mod List.length(siblings);
        let next_frame = List.nth(siblings, next_pos);
        if (Sample.equal_stack_frame(next_frame, current_frame)) {
          Stop_propagation;
        } else {
          /* switch_sibling does Capture; also jump to the new frame's call site */
          let sibling_effect =
            switch_sibling(
              ~globals,
              ~call_stack_rev,
              ~view_index,
              next_frame,
              evt,
            );
          let jump_target =
            is_in_user_code(~info_map, next_frame.id)
              ? Some(next_frame.id)
              : find_nearest_user_app(
                  ~info_map,
                  ~call_stack=call_stack_rev,
                  ~from_index=view_index - 1,
                );
          switch (jump_target) {
          | Some(target_id) =>
            Many([sibling_effect, jump_to(~globals, target_id, evt)])
          | None => sibling_effect
          };
        };
      | None => Stop_propagation
      };
    };
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
  | D("a" | "A") =>
    /* Toggle anti-pin at current focus depth */
    let n = List.length(call_stack_rev);
    if (index >= 0 && index < n) {
      let original_depth = n - 1 - index;
      Many([
        toggle_anti_pin(~globals, original_depth, evt),
        Stop_propagation,
      ]);
    } else {
      Stop_propagation;
    };
  | _ => Ignore
  };
};

/* Render a single tree-view entry */
let tree_entry =
    (
      ~globals: Globals.t,
      ~info_map: Statics.Map.t,
      ~sightline_rev: Sample.call_stack,
      ~index: int,
      entry: Sample.CallTree.line_entry,
    )
    : Node.t => {
  let frame = entry.frame;
  let display_text = resolve_display_name(~info_map, frame);
  let on_sightline =
    Sample.CallTree.is_on_sightline(~sightline_rev, entry);
  let is_focused = on_sightline && entry.depth == index;
  let classes =
    ["tree-entry"]
    @ (on_sightline ? ["on-sightline"] : [])
    @ (is_focused ? ["focused"] : []);

  /* Build a Capture to switch the sightline to this entry's branch.
   * The path is outermost-first; Capture wants innermost-first. */
  let on_click = evt => {
    let new_stack = List.rev(entry.path);
    let capture_data: Sample.Capture.t = {
      call_stack: new_stack,
      time: 0.0,
      seq: 0,
      step_start: 0,
      step_end: 0,
    };
    let capture_effect =
      globals.inject_global(
        ActiveEditor(
          Project(SampleFocus(Capture(capture_data, None))),
        ),
      );
    let call_site_target =
      is_in_user_code(~info_map, frame.id)
        ? Some(frame.id) : None;
    switch (call_site_target) {
    | Some(target_id) =>
      Effect.Many([capture_effect, jump_to(~globals, target_id, evt)])
    | None => capture_effect
    };
  };

  span(
    ~attrs=[
      Attr.classes(classes),
      Attr.title(display_text),
      Attr.on_pointerdown(on_click),
    ],
    [text(display_text)],
  );
};

/* Compute the maximum depth across all flattened lines */
let max_depth = (lines: list(list(Sample.CallTree.line_entry))): int =>
  List.fold_left(
    (acc, line) =>
      List.fold_left(
        (acc, entry: Sample.CallTree.line_entry) => max(acc, entry.depth),
        acc,
        line,
      ),
    0,
    lines,
  );

/* Render the tree view of the call tree.
 * Uses a single CSS grid for the entire tree so that columns align
 * across all lines. Each depth maps to a pair of grid columns
 * (branch-char + entry name). Each line is a grid row. */
let tree_view =
    (
      ~globals: Globals.t,
      ~info_map: Statics.Map.t,
      ~probe_map: Language.Dynamics.Map.t,
      ~sightline_rev: Sample.call_stack,
      ~index: int,
    )
    : Node.t => {
  let tree = Sample.CallTree.of_probe_map(probe_map);
  let lines = Sample.CallTree.flatten(tree);
  let n_cols = max_depth(lines) + 1;
  /* Each depth gets 2 grid columns: one for branch char, one for name.
   * Total grid columns = n_cols * 2. */
  let grid_template =
    String.concat(
      " ",
      List.init(n_cols, _ => "auto auto"),
    );
  /* Emit all cells for all lines into a single flat list.
   * Each cell is placed by grid-row and grid-column. */
  let all_cells =
    List.mapi(
      (row_idx, line: list(Sample.CallTree.line_entry)) =>
        List.mapi(
          (i, entry: Sample.CallTree.line_entry) => {
            let row = row_idx + 1;
            /* Branch character cell */
            let branch_col = entry.depth * 2 + 1;
            let branch_char =
              if (i == 0 && entry.depth == 0) {
                "";
              } else if (i == 0 && entry.depth > 0) {
                entry.is_last_child ? {js|└|js} : {js|├|js};
              } else {
                {js|─|js};
              };
            let pos = (r, c) =>
              Css_gen.create(
                ~field="grid-area",
                ~value=
                  string_of_int(r)
                  ++ " / "
                  ++ string_of_int(c)
                  ++ " / "
                  ++ string_of_int(r + 1)
                  ++ " / "
                  ++ string_of_int(c + 1),
              );
            let branch_cell =
              span(
                ~attrs=[
                  Attr.classes(["tree-branch"]),
                  Attr.style(pos(row, branch_col)),
                ],
                [text(branch_char)],
              );
            /* Entry name cell */
            let name_col = entry.depth * 2 + 2;
            let entry_cell =
              span(
                ~attrs=[
                  Attr.classes(["tree-name-cell"]),
                  Attr.style(pos(row, name_col)),
                ],
                [
                  tree_entry(
                    ~globals,
                    ~info_map,
                    ~sightline_rev,
                    ~index,
                    entry,
                  ),
                ],
              );
            [branch_cell, entry_cell];
          },
          line,
        )
        |> List.concat,
      lines,
    )
    |> List.concat;
  div(
    ~attrs=[
      Attr.classes(["tree-view"]),
      Attr.style(
        Css_gen.create(
          ~field="grid-template-columns",
          ~value=grid_template,
        ),
      ),
    ],
    all_cells,
  );
};

/* Main view function */
let view =
    (
      ~globals: Globals.t,
      ~refractors: Zipper.Refractor.t,
      ~info_map: Statics.Map.t,
      ~probe_map: Language.Dynamics.Map.t,
      ~indicated_id as _: option(Id.t),
    )
    : Node.t =>
  /* Hide when call stack is empty, unless auto-probe mode keeps bar visible */
  if (refractors.sample_focus.call_stack == []
      && !globals.settings.autoprobe_mode) {
    div(~attrs=[Attr.id("sample-focus-bar"), Attr.class_("hidden")], []);
  } else {
    let sample_focus = refractors.sample_focus;
    let call_stack = sample_focus.call_stack |> List.rev;
    let index = sample_focus.index;

    /* Check if there's a pinned stack and get the head app_id */
    let pinned_stack = sample_focus.pinned_stack;
    let pinned_head_id =
      Option.bind(pinned_stack, stack =>
        Option.map(
          (f: Sample.stack_frame) => f.id,
          Util.ListUtil.hd_opt(stack),
        )
      );

    /* Top-level entry (always present when bar is shown)
     * Clicking resets cursor to top level (index -1) */

    /* Pre-compute display names for width calculation and rendering */
    let names =
      Array.of_list(List.map(resolve_display_name(~info_map), call_stack));

    /* Compute dynamic capacity based on actual bar width and entry names */
    let bar_width_px =
      try(
        Js_of_ocaml.Dom_html.getElementById("sample-focus-bar")##.clientWidth
      ) {
      | _ => 600
      };
    let available_chars =
      int_of_float(
        float_of_int(bar_width_px) /. globals.font_metrics.col_width,
      );
    let budget = available_chars - bar_overhead_chars;

    /* Anti-pin state: convert from original (innermost-first) index
     * to reversed (outermost-first) view index */
    let anti_pin = sample_focus.anti_pin;
    let n = List.length(call_stack);
    let anti_pin_view_index =
      Option.map(depth => n - 1 - depth, anti_pin);
    let has_pin = pinned_stack != None;

    /* Build a single breadcrumb entry (separator + entry node) for stack index i */
    let build_single_entry = (i: int): list(Node.t) => {
      let frame: Sample.stack_frame = List.nth(call_stack, i);
      let app_id = frame.id;
      let display_text = names[i];
      let is_unknown =
        switch (frame.name) {
        | Some(_) => false
        | None =>
          let (name_opt, _) = get_fn_info(~info_map, app_id);
          Option.is_none(name_opt);
        };
      let is_focused = i == index;
      let is_ghost = i > index;
      let position_class = i < index ? "above" : i > index ? "below" : "";

      let call_site_target =
        is_in_user_code(~info_map, app_id)
          ? Some(app_id)
          : find_nearest_user_app(~info_map, ~call_stack, ~from_index=i - 1);

      let is_anti_pinned = anti_pin_view_index == Some(i);
      let entry_classes =
        ["breadcrumb-entry"]
        @ (is_focused ? ["focused"] : [])
        @ (is_ghost ? ["ghost"] : [])
        @ (is_unknown ? ["unknown"] : [])
        @ (is_anti_pinned ? ["anti-pinned"] : [])
        @ (position_class != "" ? [position_class] : []);

      /* Convert view index i to original stack depth for anti-pin */
      let original_depth = n - 1 - i;

      let on_entry_click = evt =>
        switch (call_site_target) {
        | Some(target_id) =>
          Effect.Many([
            set_focus_index(~globals, i, evt),
            jump_to(~globals, target_id, evt),
          ])
        | None => set_focus_index(~globals, i, evt)
        };

      let is_pinned = Some(app_id) == pinned_head_id;
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

      /* Anti-pin icon: shown when this entry is the anti-pin point */
      let anti_pin_icon =
        if (is_anti_pinned) {
          [
            span(
              ~attrs=[
                Attr.class_("anti-pin-icon"),
                Attr.title("Click to remove inner bound"),
                Attr.on_pointerdown(evt =>
                  Effect.Many([
                    Effect.Stop_propagation,
                    toggle_anti_pin(~globals, original_depth, evt),
                  ])
                ),
              ],
              [],
            ),
          ];
        } else {
          [];
        };

      let entry_tooltip =
        if (is_anti_pinned) {
          "Anti-pinned (press A on focus bar to remove)";
        } else if (has_pin) {
          if (is_in_user_code(~info_map, app_id)) {
            "Jump to call site (A key: set inner bound)";
          } else {
            switch (call_site_target) {
            | Some(_) => {js|Internal call — jump to enclosing (A key: set inner bound)|js}
            | None => "Internal call (A key: set inner bound)"
            };
          };
        } else if (is_in_user_code(~info_map, app_id)) {
          "Jump to call site";
        } else {
          switch (call_site_target) {
          | Some(_) => {js|Internal call — jump to enclosing call site|js}
          | None => "Internal call"
          };
        };

      let entry =
        div(
          ~attrs=[
            Attr.classes(entry_classes),
            Attr.title(entry_tooltip),
            Attr.on_pointerdown(on_entry_click),
          ],
          pin_icon @ anti_pin_icon @ [text(display_text)],
        );

      let sep_ghost = i > index + 1;
      let sep_classes =
        ["breadcrumb-separator"]
        @ (sep_ghost ? ["ghost"] : [])
        @ (position_class != "" ? [position_class] : []);
      let sep =
        span(~attrs=[Attr.classes(sep_classes)], [text({js|❯|js})]);

      [sep, entry];
    };

    /* Ellipsis node for collapsed breadcrumb ranges */
    let ellipsis_node = [
      span(
        ~attrs=[Attr.classes(["breadcrumb-separator"])],
        [text({js|❯|js})],
      ),
      span(
        ~attrs=[Attr.classes(["breadcrumb-ellipsis"])],
        [text({js|⋯|js})],
      ),
    ];

    /* Build breadcrumb entries, windowed if the stack is too long */
    let entries =
      if (List.is_empty(call_stack)) {
        [
          span(
            ~attrs=[Attr.classes(["breadcrumb-separator"])],
            [text({js|❯|js})],
          ),
        ];
      } else {
        let n = List.length(call_stack);
        let cap = compute_dynamic_cap(~names, ~focus=index, ~budget);
        let visible = compute_visible(~n, ~focus=index, ~cap);
        List.concat_map(
          item =>
            switch (item) {
            | Entry(i) => build_single_entry(i)
            | Ellipsis => ellipsis_node
            },
          visible,
        );
      };

    let max_index = List.length(call_stack) - 1;

    /* Body icon (●) at end of breadcrumbs: jumps to definition of the
     * deepest function in the call stack. With perspective extension
     * (ap_id prepended to call_stack in capture), this naturally covers
     * both the "inside a function" and "on an application" cases. */
    let body_icon =
      switch (ListUtil.last_opt(call_stack)) {
      | Some(last_frame) =>
        let param_target =
          get_param_target(
            ~info_map,
            ~app_id=last_frame.id,
            ~fn_def_id=last_frame.fn_def_id,
            ~stack_name=last_frame.name,
          );
        let def_target =
          switch (param_target) {
          | Some(_) => param_target
          | None =>
            get_definition_target(
              ~info_map,
              ~app_id=last_frame.id,
              ~fn_def_id=last_frame.fn_def_id,
              ~stack_name=last_frame.name,
            )
          };
        switch (def_target) {
        | Some(target_id) =>
          let on_body_click = evt => jump_to(~globals, target_id, evt);
          let body_sep_ghost = max_index > index;
          let body_sep_classes =
            ["breadcrumb-separator"] @ (body_sep_ghost ? ["ghost"] : []);
          [
            span(
              ~attrs=[Attr.classes(body_sep_classes)],
              [text({js|❯|js})],
            ),
            span(
              ~attrs=[
                Attr.classes(["breadcrumb-body", "ghost"]),
                Attr.title("Jump to function body"),
                Attr.on_pointerdown(on_body_click),
              ],
              [text({js|●|js})],
            ),
          ];
        | None => []
        };
      | None => []
      };

    let clear_all_button =
      span(
        ~attrs=[
          Attr.classes(["clear-all"]),
          Attr.title("Remove all probes"),
          Attr.on_pointerdown(_ =>
            globals.inject_global(ActiveEditor(Probe(RemoveAll)))
          ),
        ],
        [text("Clear all")],
      );

    /* Tree view toggle button */
    let is_tree_mode = view_mode_ref^ == TreeView;
    let toggle_icon = is_tree_mode ? {js|≡|js} : {js|⊞|js};
    let toggle_tooltip =
      is_tree_mode ? "Switch to single path" : "Switch to tree view";
    let tree_toggle =
      span(
        ~attrs=[
          Attr.classes(
            ["tree-toggle"] @ (is_tree_mode ? ["active"] : []),
          ),
          Attr.title(toggle_tooltip),
          Attr.on_pointerdown(_ => {
            view_mode_ref :=
              (is_tree_mode ? SinglePath : TreeView);
            /* Force re-render by dispatching a no-op focus set */
            globals.inject_global(
              ActiveEditor(Project(SampleFocus(SetIndex(index)))),
            );
          }),
        ],
        [text(toggle_icon)],
      );

    /* Render content based on view mode */
    let content =
      if (is_tree_mode) {
        tree_view(
          ~globals,
          ~info_map,
          ~probe_map,
          ~sightline_rev=call_stack,
          ~index,
        );
      } else {
        div(~attrs=[Attr.class_("breadcrumbs")], entries @ body_icon);
      };

    div(
      ~attrs=[
        Attr.id("sample-focus-bar"),
        Attr.classes(
          [] @ (is_tree_mode ? ["tree-mode"] : []),
        ),
        Attr.tabindex(0),
        Attr.on_keydown(
          key_handler(
            ~globals,
            ~index,
            ~max_index,
            ~call_stack,
            ~call_stack_rev=call_stack,
            ~probe_map,
            ~info_map,
          ),
        ),
      ],
      [
        div(
          ~attrs=[
            Attr.class_("title"),
            Attr.title("Call stack of the focused probe sample"),
          ],
          [text("probe focus")],
        ),
        tree_toggle,
        content,
        clear_all_button,
      ],
    );
  };
