open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Haz3lcore;
open Language;

/* Extract function name from an application ID by looking up in statics.
 *
 * Note: This returns "?" for applications inside built-in function implementations
 * (e.g., recursive calls within `map`). These app_ids aren't in info_map because
 * statics only runs on user surface syntax, not internalized built-in code.
 *
 * Future enhancement: Augment RecordStackFrame to carry the function name directly.
 * This would require:
 * - Changing the effect type to RecordStackFrame(option(string))
 * - Extracting the name from `init` (the Ap expression) when emitting the effect
 * - Changing call_stack type from list(Id.t) to list((Id.t, option(string)))
 * - Updating all call_stack consumers
 */
let get_fn_name = (~info_map: Statics.Map.t, app_id: Id.t): string =>
  switch (Statics.Map.lookup(app_id, info_map)) {
  | Some(InfoExp({term: {term: Ap(_, fn_exp, _), _}, _})) =>
    switch (fn_exp.term) {
    | Var(name) => name
    | Constructor(name, _) => name
    | Fun(_) => {js|λ|js}
    | BuiltinFun(name) => name /* Shouldn't happen in surface syntax, but handle it */
    | _ => "fn"
    }
  | _ => "?"
  };

/* Jump to syntax location */
let jump_to = (~globals: Globals.t, id: Id.t, _) =>
  globals.inject_global(ActiveEditor(Move(Goal(TileId(id)))));

/* Check if any probes exist */
let has_probes = (refractors: Zipper.Refractor.t): bool =>
  !List.is_empty(refractors.manuals)
  || !Id.Map.is_empty(refractors.autos.ids);

/* Render a single breadcrumb entry */
let breadcrumb_entry =
    (
      ~globals: Globals.t,
      ~info_map: Statics.Map.t,
      ~is_focused: bool,
      ~is_ghost: bool,
      app_id: Id.t,
    ) => {
  let name = get_fn_name(~info_map, app_id);
  let classes =
    ["breadcrumb-entry"]
    @ (is_focused ? ["focused"] : [])
    @ (is_ghost ? ["ghost"] : []);
  div(
    ~attrs=[
      Attr.classes(classes),
      Attr.on_pointerdown(jump_to(~globals, app_id)),
    ],
    [text(name)],
  );
};

/* Render the separator arrow between breadcrumbs */
let separator = () =>
  span(~attrs=[Attr.class_("breadcrumb-separator")], [text({js|▸|js})]);

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

    /* Build breadcrumb entries with separators */
    let entries =
      if (List.is_empty(call_stack)) {
        [
          /* Empty state - at top level */
          span(~attrs=[Attr.class_("top-level")], [text({js|⌀|js})]),
        ];
      } else {
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
            /* Add separator before all entries except the first */
            i == 0 ? [entry] : [separator(), entry];
          },
          call_stack,
        )
        |> List.flatten;
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
      [div(~attrs=[Attr.class_("breadcrumbs")], entries @ more_indicator)],
    );
  };
