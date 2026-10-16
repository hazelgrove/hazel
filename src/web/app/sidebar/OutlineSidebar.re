open Virtual_dom.Vdom;
open Node;

/* the collapsible module/definition outline. a click jumps; the cell
   button toggles a definition in the focus stack (stacked cells replace
   the master editor), and with a stack open a click adds or moves to that
   cell. without a stack (~stack_controls=false) it only navigates, and
   collapse is the native <details> toggle. With a stack, names are typed
   in text mode (OutlineEdit): the outline as a text area of names */

let clss = cs => Attr.classes(cs);

module TestStatus = Language.TestStatus;
module E = OutlineEdit;

/* the header row: the program and the zoom trail as a breadcrumb, and
   how many pins the current level shows or keeps parked */
type header = {
  h_program: string,
  h_trail: list((Language.Id.t, string)),
  h_open: int,
  h_parked: int,
};

let kind_glyph = (k: OutlineTree.kind): string =>
  switch (k) {
  | KModule => {js|⛁|js}
  | KFn => {js|ƒ|js}
  | KConst => {js|·|js}
  | KType => {js|τ|js}
  | KTest
  | KTests => {js|◦|js} /* overridden by the test's live status */
  | KStmt => {js|;|js}
  | KUse => {js|↓|js}
  | KTrail => {js|⇒|js}
  };

let kind_cls = (k: OutlineTree.kind): string =>
  switch (k) {
  | KModule => "ol-module"
  | KFn => "ol-fn"
  | KConst => "ol-const"
  | KType => "ol-type"
  | KTest => "ol-test"
  | KTests => "ol-tests"
  | KStmt => "ol-stmt"
  | KUse => "ol-use"
  | KTrail => "ol-trail"
  };

/* text mode's state, which the handlers keep (OutlineControl) */
type text_ctl = {
  /* as drawn */
  current: option(E.t),
  /* at the keypress: keys can outrun renders */
  get: unit => option(E.t),
  /* the same line, typed on, or its caret [placed] */
  set: (~placed: bool=?, option(E.t)) => Effect.t(unit),
  /* text mode, or a new definition's line in it: the line the caret
     leaves saved first */
  start: (~pending: string=?, E.line, int) => Effect.t(unit),
  /* the caret to another line, the one it leaves saved (or [discard]ed) */
  go: (~discard: bool=?, E.line, int, option(int)) => Effect.t(unit),
  /* save the caret's line, then as [after] says */
  save: E.after => Effect.t(unit),
  /* row mode, what's typed dropped */
  abandon: Effect.t(unit),
  /* an op on an item: what's typed saved first (a delete drops it) */
  op: (E.def_op, Language.Id.t) => Effect.t(unit),
  flash: Effect.t(unit),
  /* the caret's line drawn elsewhere than its own (which vanished) */
  relocate: E.t => unit,
  /* the line the caret was last drawn on, for when its own vanishes */
  note: int => unit,
  last: unit => option(int),
};

/* what the outline shows */
type props = {
  /* rows fold from an arrow; without, from their sigil */
  arrows: bool,
  /* pins, zoom and item edits: only where a slide has cells */
  stack_controls: bool,
  /* why a row's item opens no cell, if it doesn't: the cell code's rule */
  refusal: Language.Id.t => option(ScratchFocus.refusal),
  is_collapsed: OutlineTree.path => bool,
  header,
  zoom_root: option(Language.Id.t),
  cursor: option(OutlineTree.path),
  created: option((Language.Id.t, string)),
  /* refusals flashed on text mode's line: each new one replays it */
  flash: int,
  /* every pin, shown or not: a hidden one gets a quiet mark */
  pinned: list(Language.Id.t),
  focused_entries: list((Language.Id.t, option(string))),
  error_items: list(Language.Id.t),
  error_subtree: list(Language.Id.t),
  menu: option((E.menu_at, float, float)),
  /* the menu row the keys have selected */
  menu_sel: int,
  test_status: Language.Id.t => option(TestStatus.t),
  /* an item whose definition is only holes: Backspace removes it */
  blank: Language.Id.t => bool,
};

/* what the outline asks for */
type handlers = {
  jump: Language.Id.t => Effect.t(unit),
  focus: Language.Id.t => Effect.t(unit),
  toggle: Language.Id.t => Effect.t(unit),
  toggle_run: Language.Id.t => Effect.t(unit),
  toggle_collapse: OutlineTree.path => Effect.t(unit),
  zoom_to: option(Language.Id.t) => Effect.t(unit),
  zoom_in: Language.Id.t => Effect.t(unit),
  show_whole: bool => Effect.t(unit),
  discard: Effect.t(unit),
  zoom_out: Effect.t(unit),
  get_cursor: unit => option(OutlineTree.path),
  set_cursor: option(OutlineTree.path) => Effect.t(unit),
  focused: Effect.t(unit),
  leave: Effect.t(unit),
  text: text_ctl,
  menu_open: (E.menu_at, float, float) => Effect.t(unit),
  menu_close: Effect.t(unit),
  menu_select: int => Effect.t(unit),
  get_menu: unit => option((E.menu_at, float, float)),
  get_menu_sel: unit => int,
  def_op: (E.def_op, Language.Id.t) => Effect.t(unit),
};

type visible_row =
  E.visible_row = {
    r_path: OutlineTree.path,
    r_node: OutlineTree.node,
    r_name: string,
    r_parent: option(OutlineTree.path),
    r_expanded: bool,
  };

let nameable = E.nameable;

/* whether focus is moving within the row list (into a name's field, or
   back out to the list), not arriving from or leaving for elsewhere */
let within_rows = evt => {
  open Js_of_ocaml;
  let e = Js.Unsafe.coerce(evt);
  let rows = e##.currentTarget##closest(Js.string(".outline-body"));
  let other = e##.relatedTarget;
  Js.Opt.test(rows)
  && Js.Opt.test(other)
  && Js.to_bool(rows##contains(other));
};

/* focus leaving the outline for elsewhere on the page, not for another
   window (it comes back) */
let leaving = evt =>
  !within_rows(evt)
  && Js_of_ocaml.(Js.to_bool(Js.Unsafe.coerce(Dom_html.document)##hasFocus));

let focus_without_scroll = el =>
  Js_of_ocaml.Js.Unsafe.coerce(el)##focus(
    Js_of_ocaml.Js.Unsafe.obj([|
      ("preventScroll", Js_of_ocaml.Js.Unsafe.inject(Js_of_ocaml.Js._true)),
    |]),
  );

/* a line's field holds the keys while it shows: it takes focus whenever
   focus is in the row list (after a save it refused, say) or nowhere (the
   page parks it in the clipboard shim). Its caret goes where the keys put
   it ([col]), as it shows and as they put it there again ([seq]) */
module FieldFocus =
  Attr.Hooks.Make({
    module State = Unit;
    module Input = {
      type t = (int, int);
      let sexp_of_t =
        Sexplib0.Sexp_conv.(sexp_of_pair(sexp_of_int, sexp_of_int));
      let combine = (_, b) => b;
    };
    let take = el => {
      open Js_of_ocaml;
      let active = Js.Unsafe.coerce(Dom_html.document)##.activeElement;
      if (!Js.Opt.test(active)
          || active ==
          Js.Unsafe.coerce(Dom_html.document)##.body
          || Js.to_string(active##.id) == Util.JsUtil.clipboard_shim_id
          || Js.Opt.test(
               active##closest(Js.string("#outline-sidebar .outline-body")),
             )
          && active != el) {
        focus_without_scroll(el);
      };
    };
    let place = (el, col) =>
      Js_of_ocaml.Js.Unsafe.coerce(el)##setSelectionRange(col, col);
    let init = (_, _) => ();
    let on_mount = ((_, col), (), el) => {
      take(el);
      place(el, col);
    };
    let update = (~old_input as (seq, _), ~new_input as (seq', col), (), el) => {
      take(el);
      if (seq != seq') {
        place(el, col);
      };
    };
    let destroy = (_, (), _) => ();
  });

/* the keys to the row list, as a field goes */
let focus_rows = Effect.of_sync_fun(Util.JsUtil.focus_outline, ());

/* the field a line's name is typed in. A new definition's leading
   `type ` or `module ` picks its kind and moves out of the field into a
   label */
let edit_field = (~on: handlers, t: E.t, ~fresh: bool, typed: string) => {
  let kw = fresh ? snd(E.new_kind(typed)) : "";
  let name = E.slice(typed, E.units(kw), E.units(typed));
  [
    /* always there, so the field keeps its place (and its focus) as a
       keyword comes and goes */
    span(~attrs=[clss(["outline-edit-kw"])], kw == "" ? [] : [text(kw)]),
    input(
      ~attrs=[
        clss(["outline-edit-input"]),
        Attr.value_prop(name),
        /* the width where field-sizing isn't supported */
        Attr.create("size", string_of_int(max(3, E.units(name) + 1))),
        Attr.create("spellcheck", "false"),
        Attr.create("autocomplete", "off"),
        Attr.create("autocorrect", "off"),
        Attr.create("autocapitalize", "off"),
        Attr.create_hook(
          "outline-field-focus",
          FieldFocus.create((t.seq, t.col)),
        ),
        /* a click in the field moves its caret, nothing else */
        Attr.on_mousedown(_ => Effect.Stop_propagation),
        Attr.on_click(_ => Effect.Stop_propagation),
        Attr.on_input((evt, s) =>
          switch (on.text.get()) {
          | Some(cur) when E.same_line(cur.line, t.line) =>
            let full = kw ++ s;
            let at: int =
              Js_of_ocaml.Js.Unsafe.coerce(evt)##.target##.selectionStart;
            let kw' = fresh ? snd(E.new_kind(full)) : "";
            on.text.set(
              Some({
                ...cur,
                pending: Some(full),
                col: E.units(kw) + at - E.units(kw'),
                col_target: None,
                error: None,
              }),
            );
          | _ => Effect.Ignore
          }
        ),
        /* leaving the outline saves the name */
        Attr.on_blur(evt =>
          switch (on.text.get()) {
          | Some(cur) when E.same_line(cur.line, t.line) && leaving(evt) =>
            Effect.Many([Effect.Stop_propagation, on.text.save(Leave)])
          | _ => Effect.Stop_propagation
          }
        ),
      ],
      (),
    ),
  ];
};

let edit_hint = (t: E.t): list(Node.t) =>
  switch (t.error) {
  | Some(why) => [div(~attrs=[clss(["outline-edit-hint"])], [text(why)])]
  | None => []
  };

/* a refusal flashes the caret's line; alternating names replay it */
let flash_cls = (n: int): list(string) =>
  n == 0 ? [] : [n mod 2 == 0 ? "outline-flash-b" : "outline-flash-a"];

/* live glyph + status class for test rows; the container joins its
   children's statuses (any fail => ✗) */
let test_glyph =
    (
      ~test_status: Language.Id.t => option(TestStatus.t),
      n: OutlineTree.node,
    )
    : option((string, string)) => {
  let of_status = (s: TestStatus.t) =>
    switch (s) {
    | Pass => ({js|✓|js}, "ol-pass")
    | Fail => ({js|✗|js}, "ol-fail")
    | Indet => ({js|?|js}, "ol-indet")
    };
  switch (n.o_kind) {
  | KTest => Option.map(of_status, Option.bind(n.o_test, test_status))
  | KTests =>
    let sts =
      List.filter_map(
        (c: OutlineTree.node) => Option.bind(c.o_test, test_status),
        n.o_children,
      );
    sts == [] ? None : Some(of_status(TestStatus.join_all(sts)));
  | _ => None
  };
};

/* drag-to-resize: pointerdown attaches DOCUMENT-level move/up
   listeners (pointer events — canceled pointerdown suppresses compat
   mouseup); width rides a ROOT css variable */
let resize_attrs: list(Attr.t) = [
  Attr.on_pointerdown(_ => {
    let listen = (name, f) =>
      Js_of_ocaml.(
        Dom_html.addEventListener(
          Dom_html.document,
          Dom.Event.make(name),
          Dom_html.handler(e => {
            f(e);
            Js._true;
          }),
          Js._false,
        )
      );
    let move =
      listen("pointermove", e =>
        Util.JsUtil.set_css_custom_property(
          "--outline-w",
          string_of_int(
            max(140, min(420, Js_of_ocaml.Js.Unsafe.coerce(e)##.clientX)),
          )
          ++ "px",
        )
      );
    let up = ref(Option.none);
    up :=
      Some(
        listen("pointerup", _ => {
          Js_of_ocaml.Dom_html.removeEventListener(move);
          Option.iter(Js_of_ocaml.Dom_html.removeEventListener, up^);
        }),
      );
    Effect.Prevent_default;
  }),
];

/* the keyboard to the outline: a click anywhere in it takes focus */
let take_focus =
  Effect.of_sync_fun(
    () =>
      if (!Util.JsUtil.outline_has_focus()) {
        Util.JsUtil.focus_outline();
      },
    (),
  );

/* the cursor to [c], with the keys: after the focus event's own cursor
   (the caret's row), so set at the event's end */
let take_cursor = (on: handlers, c: option(OutlineTree.path)) =>
  Effect.Many([
    Effect.Prevent_default,
    take_focus,
    Effect.lazy_(lazy(on.set_cursor(c))),
  ]);

/* this event only: no default action, no other listeners */
let stop = eff =>
  Effect.Many([Effect.Prevent_default, Effect.Stop_propagation, eff]);

let in_stack = (props: props, id: Language.Id.t): bool =>
  List.mem_assoc(id, props.focused_entries);

/* an open cell's row shows its header's live text */
let live_label = (props: props, n: OutlineTree.node): OutlineTree.node =>
  switch (
    Option.bind(n.o_id, id => List.assoc_opt(id, props.focused_entries))
  ) {
  | Some(Some(live)) => {
      ...n,
      o_label: live,
    }
  | _ => n
  };

/* a row's click: with a stack open, add or move to its cell (a jump would
   target the hidden master) */
let show = (props: props, on: handlers, id: Language.Id.t): Effect.t(unit) =>
  props.focused_entries != [] ? on.focus(id) : on.jump(id);

/* why a row opens no cell, if it doesn't; a tests container's run goes by
   its first test */
let refusal =
    (props: props, n: OutlineTree.node): option(ScratchFocus.refusal) =>
  switch (n.o_id, n.o_kind) {
  | (Some(id), _) => props.refusal(id)
  | (None, KTests) =>
    Option.bind(
      List.find_map((c: OutlineTree.node) => c.o_id, n.o_children),
      props.refusal,
    )
  | (None, _) => None
  };

/* the cell button's act: open or close the row's cell; a tests container
   opens its whole run as one cell, at any depth. None: nothing to open */
let open_close =
    (props: props, on: handlers, n: OutlineTree.node)
    : option(unit => Effect.t(unit)) => {
  let opens = refusal(props, n) == None;
  switch (n.o_id, n.o_kind) {
  | (Some(id), _) when in_stack(props, id) || opens =>
    Some(() => on.toggle(id))
  | (None, KTests) =>
    switch (List.filter_map((c: OutlineTree.node) => c.o_id, n.o_children)) {
    | [first, ..._] as kids when opens || List.exists(in_stack(props), kids) =>
      Some(() => on.toggle_run(first))
    | _ => None
    }
  | _ => None
  };
};

/* a tests label's menu: its first and last tests */
let run_menu = (n: OutlineTree.node, ~expanded: bool): option(E.menu_at) =>
  switch (List.filter_map((c: OutlineTree.node) => c.o_id, n.o_children)) {
  | [first, ..._] as ids =>
    Some(On_run(first, Util.ListUtil.last(ids), expanded))
  | [] => None
  };

/* the editor shows the item at a line the caret goes to */
let follow = (e: option(E.entry)) =>
  Effect.of_sync_fun(
    () => OutlineFollow.wanted := Option.bind(e, E.shown),
    (),
  );

/* text mode at a row: at its name's end, or a ghost line's start */
let start_text = (~on: handlers, ~col=?, r: visible_row): Effect.t(unit) =>
  Effect.Many([
    follow(Some(Of_row(r))),
    on.text.start(
      E.line_of(Of_row(r)),
      nameable(r.r_node)
        ? Option.value(col, ~default=E.units(r.r_name)) : 0,
    ),
  ]);

/* the column a click lands at in a row's name: the letter under it, else
   its start or end (-1) */
let offset_at: Js_of_ocaml.Js.Unsafe.any =
  Js_of_ocaml.Js.Unsafe.js_expr(
    {|(function (span, x, y) {
  var n = null, o = 0;
  if (document.caretRangeFromPoint) {
    var r = document.caretRangeFromPoint(x, y);
    if (r) { n = r.startContainer; o = r.startOffset; }
  } else if (document.caretPositionFromPoint) {
    var p = document.caretPositionFromPoint(x, y);
    if (p) { n = p.offsetNode; o = p.offset; }
  }
  if (n && span.contains(n)) return o;
  return x < span.getBoundingClientRect().left ? 0 : -1;
})|},
  );

let click_col = (evt, name: string): int => {
  open Js_of_ocaml;
  let len = E.units(name);
  let e = Js.Unsafe.coerce(evt);
  let label = e##.currentTarget##closest(Js.string(".outline-label"));
  let span =
    Js.Opt.test(label)
      ? label##querySelector(Js.string(".outline-name")) : Js.null;
  if (Js.Opt.test(span)) {
    let at: int =
      Js.Unsafe.fun_call(
        offset_at,
        [|
          Js.Unsafe.inject(span),
          Js.Unsafe.inject(e##.clientX),
          Js.Unsafe.inject(e##.clientY),
        |],
      );
    at < 0 ? len : min(at, len);
  } else {
    len;
  };
};

/* text mode as drawn: its lines, the caret's, and the caret */
type drawn = {
  d_lines: array(E.entry),
  d_at: int,
  d_text: E.t,
};

let caret_entry = (d: drawn): E.entry => d.d_lines[d.d_at];

/* the row a drawn caret is on, if it's on a row */
let caret_path = (d: option(drawn)): option(OutlineTree.path) =>
  switch (Option.map(caret_entry, d)) {
  | Some(Of_row(r)) => Some(r.r_path)
  | _ => None
  };

/* the row a new definition is typed into */
let new_row_view = (~on: handlers, ~flash: int, t: E.t): Node.t => {
  let typed = Option.value(t.pending, ~default="");
  let (k, _) = E.new_kind(typed);
  div(
    ~attrs=[clss(["outline-leaf"])],
    [
      div(
        ~attrs=[
          clss(
            ["outline-label", "outline-editing", "outline-new", kind_cls(k)]
            @ flash_cls(flash),
          ),
          /* a click beside the field leaves the keys in it */
          Attr.on_mousedown(_ => Effect.Prevent_default),
        ],
        [span(~attrs=[clss(["outline-glyph"])], [text(kind_glyph(k))])]
        @ edit_field(~on, t, ~fresh=true, typed),
      ),
    ]
    @ edit_hint(t),
  );
};

let rec node_view =
        (
          ~props: props,
          ~on: handlers,
          ~tm: option(drawn),
          ~path: OutlineTree.path,
          ~seg: OutlineTree.path_seg, /* this row's name among its siblings */
          n: OutlineTree.node,
        )
        : Node.t => {
  let status = test_glyph(~test_status=props.test_status, n);
  /* a subtree's error: badge shown by CSS only while COLLAPSED (the
     deepest visible row owns the error otherwise) */
  let (has_err, has_roll_err, stacked) =
    switch (n.o_id) {
    | Some(id) =>
      let has_err = List.mem(id, props.error_items);
      (
        has_err,
        !has_err && List.mem(id, props.error_subtree),
        in_stack(props, id),
      );
    | None => (false, false, false)
    };
  let name = n.o_label;
  /* the focused row's name tracks its header editor LIVE */
  let n = live_label(props, n);
  /* paths name the row by its outline label, not the live header text
     (`inc(x)` for `inc`): collapse state and the cursor survive pinning */
  let row_path = path @ [seg];
  /* the row as the keys see it, for its line */
  let row: visible_row = {
    r_path: row_path,
    r_node: n,
    r_name: name,
    r_parent: None,
    r_expanded: n.o_children != [] && !props.is_collapsed(row_path),
  };
  /* text mode's caret on this row */
  let caret =
    switch (tm) {
    | Some(d) when caret_path(tm) == Some(row_path) => Some(d)
    | _ => None
    };
  let cursor =
    switch (tm) {
    | Some(_) => caret != None
    | None => props.cursor == Some(row_path)
    };
  /* a double-click on the name types it */
  let name_view = label =>
    span(
      ~attrs=
        [clss(["outline-name"])]
        @ (
          props.stack_controls && nameable(n)
            ? [
              Attr.on_double_click(evt =>
                stop(
                  on.text.get() == None
                    ? start_text(~on, ~col=click_col(evt, name), row)
                    : Effect.Ignore,
                )
              ),
            ]
            : []
        ),
      [text(label)],
    );
  let badge = (cls, title) =>
    span(
      ~attrs=[clss(["outline-err-badge", ...cls]), Attr.title(title)],
      [text({js|●|js})],
    );
  /* an unfinished item's button waits, disabled; a row under a projector
     has none */
  let cell_button = (~cls, ~title) =>
    switch (open_close(props, on, n), refusal(props, n)) {
    | (Some(act), _) => [
        span(
          ~attrs=[
            clss(["outline-focus-btn", ...cls]),
            Attr.title(title),
            Attr.on_click(_ => stop(act())),
          ],
          [],
        ),
      ]
    | (None, Some(Unfinished)) => [
        span(
          ~attrs=[
            clss(["outline-focus-btn", "outline-btn-disabled"]),
            Attr.title("finish the definition to open it"),
          ],
          [],
        ),
      ]
    | (None, _) => []
    };
  /* the row's own menu, if any: every block level but the ⇒ */
  let menu_at: option(E.menu_at) =
    switch (n.o_id, n.o_kind) {
    | (_, KTrail) => None
    | (Some(id), k) => Some(E.On_item(id, k))
    | (None, KTests) => run_menu(n, ~expanded=row.r_expanded)
    | (None, _) => None
    };
  let label =
    div(
      ~attrs=
        [
          clss(
            ["outline-label", kind_cls(n.o_kind)]
            @ (cursor ? ["outline-cursor"] : [])
            @ (caret != None ? ["outline-editing"] : [])
            @ (
              switch (caret) {
              | Some(_) => flash_cls(props.flash)
              | None => []
              }
            )
            @ (stacked ? ["outline-focused"] : [])
            @ (has_err ? ["outline-has-err"] : [])
            @ (
              switch (status) {
              | Some((_, cls)) => [cls]
              | None => []
              }
            ),
          ),
          /* a click takes the keyboard to the outline, at this row: focus
             first, so the focus event's cursor lands before this one.
             without a stack there are no keys, so no cursor. In text mode
             the caret goes to the click, the line it leaves saved */
          Attr.on_mousedown(evt =>
            switch (tm, caret) {
            | (Some(_), Some(_)) => Effect.Prevent_default
            | (Some(_), None) =>
              Effect.Many([
                Effect.Prevent_default,
                focus_rows,
                follow(Some(Of_row(row))),
                on.text.go(
                  E.line_of(Of_row(row)),
                  nameable(n) ? click_col(evt, name) : 0,
                  None,
                ),
              ])
            | (None, _) =>
              props.stack_controls
                ? take_cursor(on, Some(row_path))
                : Effect.Many([Effect.Prevent_default, take_focus])
            }
          ),
        ]
        @ (
          switch (n.o_id) {
          | Some(id) => [
              Attr.create("data-ol-id", Language.Id.to_string(id)),
            ]
          | None => []
          }
        )
        /* Prevent_default: label clicks must not toggle the row's
           <details>. In text mode a click only places the caret */
        @ (
          switch (n.o_id, tm) {
          | (_, Some(_)) => [Attr.on_click(_ => stop(Effect.Ignore))]
          | (Some(id), None) => [
              Attr.on_click(_ => stop(show(props, on, id))),
            ]
          | (None, None) => []
          }
        )
        @ (
          switch (menu_at) {
          | Some(at) when props.stack_controls => [
              Attr.on_contextmenu(evt =>
                Effect.Many([
                  Effect.Prevent_default,
                  on.menu_open(
                    at,
                    float_of_int(evt##.clientX),
                    float_of_int(evt##.clientY),
                  ),
                ])
              ),
            ]
          | _ => []
          }
        ),
      [
        span(
          ~attrs=
            [clss(["outline-glyph"])]
            /* without arrows a branch's sigil folds it */
            @ (
              !props.arrows && n.o_children != []
                ? [
                  Attr.on_click(evt =>
                    stop(
                      props.stack_controls
                        ? fold(~on, ~tm, row)
                        : Effect.of_sync_fun(
                            () => {
                              let details =
                                Js_of_ocaml.Js.Unsafe.meth_call(
                                  Js_of_ocaml.Js.Unsafe.coerce(evt)##.currentTarget,
                                  "closest",
                                  [|
                                    Js_of_ocaml.Js.Unsafe.inject(
                                      Js_of_ocaml.Js.string("details"),
                                    ),
                                  |],
                                );
                              Js_of_ocaml.Js.Unsafe.set(
                                details,
                                "open",
                                Js_of_ocaml.Js.bool(
                                  !
                                    Js_of_ocaml.Js.to_bool(
                                      Js_of_ocaml.Js.Unsafe.get(
                                        details,
                                        "open",
                                      ),
                                    ),
                                ),
                              );
                            },
                            (),
                          ),
                    )
                  ),
                ]
                : []
            ),
          [
            text(
              switch (status) {
              | Some((g, _)) => g
              | None => kind_glyph(n.o_kind)
              },
            ),
          ],
        ),
      ]
      @ (
        switch (caret, props.created) {
        | (Some(d), _) when nameable(n) =>
          edit_field(
            ~on,
            d.d_text,
            ~fresh=false,
            Option.value(d.d_text.pending, ~default=name),
          )
        /* a ghost line's one caret position, at its start */
        | (Some(_), _) => [
            span(~attrs=[clss(["outline-ghost-caret"])], []),
            ...words(n, name_view),
          ]
        | (None, Some((id, kw))) when n.o_id == Some(id) => [
            /* the keyword typed to make it a type or module, leaving */
            span(~attrs=[clss(["outline-kw-leaving"])], [text(kw)]),
            name_view(n.o_label),
          ]
        | _ => words(n, name_view)
        }
      )
      @ (has_err ? [badge([], "contains type errors")] : [])
      @ (
        has_roll_err
          ? [
            badge(["outline-err-roll"], "contains type errors (collapsed)"),
          ]
          : []
      )
      @ (
        switch (n.o_id) {
        | _ when !props.stack_controls => []
        | None when n.o_kind == KTests =>
          let kid_ids =
            List.filter_map((c: OutlineTree.node) => c.o_id, n.o_children);
          let all_pinned =
            kid_ids != [] && List.for_all(in_stack(props), kid_ids);
          cell_button(
            ~cls=all_pinned ? ["outline-btn-on"] : [],
            ~title=
              all_pinned
                ? "close the tests cell" : "open the tests as one cell",
          );
        | Some(id) =>
          cell_button(
            ~cls=
              stacked
                ? ["outline-btn-on"]
                : List.mem(id, props.pinned) ? ["outline-btn-parked"] : [],
            ~title=stacked ? "close this cell" : "open in the editor stack",
          )
        | None => []
        }
      ),
    );
  let hint =
    switch (caret) {
    | Some(d) when nameable(n) => edit_hint(d.d_text)
    | _ => []
    };
  /* a new member being named first or last inside this module: shown
     open, even when collapsed or empty */
  let inside =
    switch (tm) {
    | Some(d) =>
      switch (caret_entry(d)) {
      | Of_new(a, (First | Last) as place) when n.o_id == Some(a) =>
        Some((place, new_row_view(~on, ~flash=props.flash, d.d_text)))
      | _ => None
      }
    | None => None
    };
  switch (n.o_children, inside) {
  | ([], None) => div(~attrs=[clss(["outline-leaf"])], [label, ...hint])
  | (kids, _) =>
    let kids = rows_view(~props, ~on, ~tm, ~path=row_path, kids);
    create(
      "details",
      ~attrs=
        [clss(["outline-branch"])]
        @ (
          props.is_collapsed(row_path) && Option.is_none(inside)
            ? [] : [Attr.create("open", "")]
        ),
      [
        create(
          "summary",
          ~attrs=
            [clss(["outline-summary"])]
            /* with stack controls collapse is MODEL state (per slide,
               persisted): the summary click dispatches the toggle and
               suppresses the native one */
            @ (
              props.stack_controls
                ? [
                  Attr.on_click(_ =>
                    Effect.Many([
                      Effect.Prevent_default,
                      fold(~on, ~tm, row),
                    ])
                  ),
                  /* the chevron too: the summary would take focus
                     itself */
                  Attr.on_mousedown(_ =>
                    tm == None
                      ? take_cursor(on, Some(row_path))
                      : Effect.Prevent_default
                  ),
                ]
                : []
            ),
          [label],
        ),
      ]
      @ hint
      @ [
        div(
          ~attrs=[clss(["outline-kids"])],
          switch (inside) {
          | Some((First, row)) => [row, ...kids]
          | Some((_, row)) => kids @ [row]
          | None => kids
          },
        ),
      ],
    );
  };
}

/* a row's words: its name, or, for rows without one of their own, a word
   for what they are, set apart from real names */
and words = (n: OutlineTree.node, name_view) =>
  switch (n.o_kind) {
  | KTrail when n.o_label == "" => [
      span(~attrs=[clss(["outline-synthetic"])], [text("result")]),
    ]
  | KTests => [
      span(~attrs=[clss(["outline-synthetic"])], [text(n.o_label)]),
    ]
  | _ => [name_view(n.o_label)]
  }

/* a row folded or unfolded by a click: text mode's caret, folded away,
   comes up to the row first */
and fold = (~on: handlers, ~tm: option(drawn), row: visible_row) => {
  let hides =
    row.r_expanded
    && (
      switch (Option.map(caret_entry, tm)) {
      | Some(Of_row(r)) => E.under(row.r_path, r.r_path)
      | Some(Of_new(a, _)) =>
        switch (tm) {
        | Some(d) =>
          Array.exists(
            fun
            | E.Of_row(r) =>
              r.r_node.o_id == Some(a)
              && (r.r_path == row.r_path || E.under(row.r_path, r.r_path))
            | Of_new(_) => false,
            d.d_lines,
          )
        | None => false
        }
      | None => false
      }
    );
  hides
    ? Effect.Many([
        focus_rows,
        follow(Some(Of_row(row))),
        on.text.go(
          E.line_of(Of_row(row)),
          nameable(row.r_node) ? E.units(row.r_name) : 0,
          None,
        ),
        on.toggle_collapse(row.r_path),
      ])
    : on.toggle_collapse(row.r_path);
}

/* sibling rows under [path], each followed by a new definition typed
   below it */
and rows_view =
    (
      ~props: props,
      ~on: handlers,
      ~tm: option(drawn),
      ~path: OutlineTree.path,
      ns,
    )
    : list(Node.t) =>
  List.concat_map(
    ((n: OutlineTree.node, seg)) =>
      [node_view(~props, ~on, ~tm, ~path, ~seg, n)]
      @ new_row_after(~props, ~on, ~tm, n),
    OutlineTree.segs(ns),
  )

/* a new definition typed below [n] */
and new_row_after =
    (~props: props, ~on: handlers, ~tm: option(drawn), n: OutlineTree.node)
    : list(Node.t) =>
  switch (tm) {
  | Some(d) =>
    switch (caret_entry(d)) {
    | Of_new(a, Below) when n.o_id == Some(a) => [
        new_row_view(~on, ~flash=props.flash, d.d_text),
      ]
    | _ => []
    }
  | None => []
  };

/* the context menu's rows, as data: the menu draws them and the keys
   walk them. An item's act is a thunk: [text.start] takes effect when
   called, so drawing the menu must not call it */
type menu_row =
  | Item(string, option(string), unit => Effect.t(unit)) /* label, shortcut, act */
  | Divider;

let menu_rows = (~on: handlers, at: E.menu_at): list(menu_row) => {
  let mac = Util.Os.is_mac^;
  /* a new definition's line, its keyword ([prefix]) picking its kind */
  let new_row = (~place=E.Below, id, label, prefix) =>
    Item(
      label,
      None,
      () =>
        Effect.Many([
          follow(Some(Of_new(id, place))),
          on.text.start(~pending=prefix, New(id, place), 0),
        ]),
    );
  let test = (op, id) =>
    Item("New test below", None, () => on.text.op(op, id));
  switch (at) {
  | On_run(first, last, expanded) => [
      expanded ? test(NewTestAbove, first) : test(NewTestBelow, last),
    ]
  | On_item(id, kind) =>
    let op = (~keys=?, o, label) =>
      Item(label, keys, () => on.text.op(o, id));
    (
      kind == KModule
        ? [
          Item(
            "Zoom in",
            Some(mac ? {js|⌥→|js} : {js|Alt+→|js}),
            () => on.zoom_in(id),
          ),
          new_row(~place=Last, id, "New definition inside", ""),
          Divider,
        ]
        : []
    )
    @ [
      new_row(id, "New definition below", ""),
      new_row(id, "New type below", "type "),
      new_row(id, "New module below", "module "),
    ]
    @ (kind == KTest ? [test(NewTestBelow, id)] : [])
    @ [
      Divider,
      op(~keys=mac ? {js|⌘D|js} : "Ctrl+D", Duplicate, "Duplicate"),
      op(~keys=mac ? {js|⌥↑|js} : {js|Alt+↑|js}, MoveUp, "Move up"),
      op(~keys=mac ? {js|⌥↓|js} : {js|Alt+↓|js}, MoveDown, "Move down"),
      Divider,
      op(~keys=mac ? {js|⌘⌫|js} : "Del", Delete, "Delete"),
    ];
  };
};

/* the rows that act, in order: the keyboard's selection counts these */
let menu_items = (rows: list(menu_row)) =>
  List.filter_map(
    fun
    | Item(_, _, act) => Some(act)
    | Divider => None,
    rows,
  );

/* the right-click menu: the editor's context menu (its classes, flip,
   dividers and key chips), fixed at the click */
let menu_view =
    (
      ~menu_close: Effect.t(unit),
      ~rows: list(menu_row),
      ~selected: int,
      (x: float, y: float),
    )
    : list(Node.t) => {
  let (_, nodes) =
    List.fold_left_map(
      (i, r) =>
        switch (r) {
        | Item(label, keys, act) => (
            i + 1,
            div(
              ~attrs=[
                clss(
                  ["named-menu-item"] @ (i == selected ? ["selected"] : []),
                ),
                Attr.on_click(_ =>
                  Effect.Many([menu_close, focus_rows, act()])
                ),
              ],
              [text(label)]
              @ (
                switch (keys) {
                | Some(k) => [
                    span(~attrs=[clss(["menu-shortcut"])], [text(k)]),
                  ]
                | None => []
                }
              ),
            ),
          )
        | Divider => (i, div(~attrs=[clss(["menu-divider"])], []))
        },
      0,
      rows,
    );
  /* its height, for which way it opens: about 23px a row, 8 a divider */
  let height =
    List.fold_left(
      (h, r) =>
        switch (r) {
        | Item(_) => h +. 23.
        | Divider => h +. 8.
        },
      0.,
      rows,
    );
  let dir =
    Util.Menu.direction_of(
      ~menu_height=height,
      ~menu_width=230.,
      Util.Menu.space_from(
        ~anchor_top=y,
        ~anchor_bot=y,
        ~anchor_left=x,
        ~anchor_right=x,
      ),
    );
  [
    div(
      ~attrs=[
        clss(["outline-menu-backdrop"]),
        /* the keys stay in the outline through the menu */
        Attr.on_mousedown(_ => Effect.Prevent_default),
        Attr.on_click(_ => menu_close),
        Attr.on_wheel(_ => menu_close),
        Attr.on_contextmenu(_ =>
          Effect.Many([Effect.Prevent_default, menu_close])
        ),
      ],
      [],
    ),
    div(
      ~attrs=[
        clss([
          "context-menu",
          "outline-menu",
          ContextMenu.direction_class(dir),
        ]),
        Attr.create(
          "style",
          Printf.sprintf("position: fixed; left: %.0fpx; top: %.0fpx;", x, y),
        ),
        Attr.on_mousedown(_ => Effect.Prevent_default),
      ],
      [
        div(
          ~attrs=[clss(["group"])],
          [div(~attrs=[clss(["contents"])], nodes)],
        ),
      ],
    ),
  ];
};

/* how the outline takes a key: it acts (nothing else sees the key), or
   leaves it to the field it was typed in, or to the page */
type verdict =
  | Act(Effect.t(unit))
  | Own(Effect.t(unit))
  | Pass(Effect.t(unit));

/* text mode's lines and caret, as the keys find them: the caret's line
   drawn elsewhere if its own vanished, with nothing typed carried over */
let text_lines =
    (~on: handlers, ~visible: array(visible_row), t: E.t)
    : option((array(E.entry), int, E.t)) => {
  let lines = E.lines(~text=Some(t), visible);
  Option.map(
    at => {
      let line = E.line_of(lines[at]);
      (
        lines,
        at,
        E.same_line(t.line, line)
          ? t
          : {
            ...E.at(line, ~col=t.col),
            seq: t.seq,
          },
      );
    },
    E.find(~last=?on.text.last(), lines, t.line),
  );
};

/* the outline's keys: arrows move and fold, Page keys move a screenful,
   Enter shows, Space opens as a cell; Alt with arrows moves rows and
   zooms; ⌥F folds; ⌘D duplicates, ⌘⌫ deletes; Esc (or Alt+O) returns;
   ⌘Enter, F2 type names (text mode) */
let keys =
    (~props: props, ~on: handlers, ~visible: array(visible_row), evt)
    : Effect.t(unit) => {
  let n = Array.length(visible);
  let idx =
    Option.bind(on.get_cursor(), p =>
      Array.find_index(r => r.r_path == p, visible)
    )
    |> Option.value(~default=-1);
  let target = (i): option(OutlineTree.path) =>
    i < 0 || n == 0 ? None : Some(visible[min(i, n - 1)].r_path);
  let go = i => on.set_cursor(target(i));
  let cur = idx >= 0 ? Some(visible[idx]) : None;
  let k = Util.Key.mk(KeyDown, evt);
  let key =
    switch (k.key) {
    | D(key)
    | U(key) => key
    };
  let (code, alt, shift) = (k.code, k.alt == Down, k.shift == Down);
  let meta = k.meta == Down || k.ctrl == Down;
  /* mid-composition (an IME's, a dead key's) the field has the key */
  let composing =
    Js_of_ocaml.Js.Optdef.case(
      Js_of_ocaml.Js.Unsafe.coerce(evt)##.isComposing,
      () => false,
      Js_of_ocaml.Js.to_bool,
    );
  let id_of = (r: visible_row) => r.r_node.o_id;
  let is_module = (r: visible_row) => r.r_node.o_kind == OutlineTree.KModule;
  /* the context menu at a row, as the editor opens its own */
  let open_menu = (r: visible_row) => {
    let at: option(E.menu_at) =
      switch (id_of(r), r.r_node.o_kind) {
      | (_, KTrail) => None
      | (Some(id), k) => Some(E.On_item(id, k))
      | (None, KTests) => run_menu(r.r_node, ~expanded=r.r_expanded)
      | (None, _) => None
      };
    Option.map(
      at => {
        let (x, y) =
          Option.value(
            Util.JsUtil.outline_cursor_anchor(),
            ~default=(0., 0.),
          );
        on.menu_open(at, x, y);
      },
      at,
    );
  };
  let nav_key = () =>
    switch (key, alt, meta, cur) {
    | (".", false, true, Some(r)) => open_menu(r)
    | ("F10", false, false, Some(r)) when shift => open_menu(r)
    | ("Escape", _, _, _) => Some(on.leave)
    | _ when alt && code == "KeyO" => Some(on.leave)
    /* ⌥F folds, as the editor's Fold does */
    | (_, true, false, _) when code == "KeyF" =>
      Some(
        switch (cur) {
        | Some(r) when r.r_node.o_children != [] =>
          on.toggle_collapse(r.r_path)
        | _ => Effect.Ignore
        },
      )
    | ("ArrowDown", false, false, _) =>
      Some(idx + 1 < n ? go(idx + 1) : Effect.Ignore)
    | ("ArrowUp", false, false, _) =>
      Some(idx >= 0 ? go(idx - 1) : Effect.Ignore)
    | ("Home", false, false, _) => Some(go(-1))
    | ("End", false, false, _) => Some(go(n - 1))
    | ("PageDown", false, false, _) =>
      Some(go(min(n - 1, idx + Util.JsUtil.outline_page_rows())))
    | ("PageUp", false, false, _) =>
      Some(
        go(idx > 0 ? max(0, idx - Util.JsUtil.outline_page_rows()) : (-1)),
      )
    | ("ArrowRight", false, false, None) => Some(go(0))
    | ("ArrowRight", false, false, Some(r)) =>
      Some(
        r.r_node.o_children == []
          ? Effect.Ignore
          : r.r_expanded ? go(idx + 1) : on.toggle_collapse(r.r_path),
      )
    | ("ArrowLeft", false, false, Some(r)) =>
      Some(
        r.r_expanded
          ? on.toggle_collapse(r.r_path) : on.set_cursor(r.r_parent),
      )
    | ("Enter", false, false, None) =>
      Some(
        props.header.h_open > 0
          ? on.show_whole(true)
          : props.header.h_parked > 0 ? on.show_whole(false) : Effect.Ignore,
      )
    | ("Enter", false, false, Some(r)) =>
      Some(
        switch (id_of(r)) {
        | Some(id) => show(props, on, id)
        | None => on.toggle_collapse(r.r_path)
        },
      )
    | (" ", false, false, Some(r)) =>
      Some(
        switch (open_close(props, on, r.r_node)) {
        | Some(act) => act()
        | None => Effect.Ignore
        },
      )
    | ("ArrowUp", true, false, Some(r)) =>
      Option.map(id => on.def_op(MoveUp, id), id_of(r))
    | ("ArrowDown", true, false, Some(r)) =>
      Option.map(id => on.def_op(MoveDown, id), id_of(r))
    | ("ArrowRight", true, false, Some(r)) when is_module(r) =>
      Option.map(on.zoom_in, id_of(r))
    | ("ArrowLeft", true, false, _) => Some(on.zoom_out)
    | (_, false, true, Some(r)) when code == "KeyD" =>
      Option.map(id => on.def_op(Duplicate, id), id_of(r))
    /* the cursor moves only once the delete lands (DefOp) */
    | ("Backspace", false, true, Some(r))
    | ("Delete", false, false, Some(r)) =>
      Option.map(id => on.def_op(Delete, id), id_of(r))
    | _ => None
    };
  /* text mode: the outline as a text area of names (OutlineEdit) */
  let text_key = (t: E.t): verdict =>
    switch (text_lines(~on, ~visible, t)) {
    | None => Pass(Effect.Ignore)
    | Some((lines, at, t')) =>
      let field = Js_of_ocaml.Js.Unsafe.coerce(evt)##.target;
      let in_field =
        Js_of_ocaml.Js.to_bool(
          field##.classList##contains(
            Js_of_ocaml.Js.string("outline-edit-input"),
          ),
        );
      let (start, stop): (int, int) =
        in_field
          ? (field##.selectionStart, field##.selectionEnd) : (t'.col, t'.col);
      let mac = Util.Os.is_mac^;
      let press: E.press = {
        key,
        code,
        alt,
        meta: mac ? k.meta == Down : k.ctrl == Down,
        ctrl: mac && k.ctrl == Down,
        shift,
        composing,
      };
      let outcome =
        E.decide(
          ~lines,
          ~at,
          ~t=t',
          ~caret={
            in_field,
            c_col: start,
            c_sel: start != stop,
          },
          ~blank=props.blank,
          ~page=Util.JsUtil.outline_page_rows,
          press,
        );
      let entry_of = (line: E.line) =>
        switch (line) {
        | New(a, p) => Some(E.Of_new(a, p))
        | Row(_) =>
          Array.find_opt(e => E.same_line(E.line_of(e), line), lines)
        };
      /* the caret's line, where its own vanished, takes its place */
      let relocated = t' === t ? Effect.Ignore : on.text.set(Some(t'));
      /* any key but ↑↓ forgets the column they aim for */
      let forget =
        t'.col_target == None
          ? Effect.Ignore
          : on.text.set(
              Some({
                ...t',
                col_target: None,
              }),
            );
      let go = (~discard=false, line, col, target) =>
        Effect.Many([
          focus_rows,
          follow(entry_of(line)),
          on.text.go(~discard, line, col, target),
        ]);
      switch (outcome) {
      /* what the field types arrives as input */
      | Field => Own(Effect.Many([relocated, forget]))
      | Page =>
        Pass(
          List.mem(key, ["Shift", "Alt", "Meta", "Control", "CapsLock"])
            ? Effect.Ignore : Effect.Many([relocated, forget, focus_rows]),
        )
      | Swallow => Act(Effect.Many([relocated, forget]))
      | Flash => Act(Effect.Many([relocated, forget, on.text.flash]))
      | Go(line, col, target) =>
        Act(Effect.Many([relocated, go(line, col, target)]))
      | Place(col) =>
        Act(
          on.text.set(
            ~placed=true,
            Some({
              ...t',
              col,
              col_target: None,
            }),
          ),
        )
      | Edit(text, col) =>
        Act(
          on.text.set(
            ~placed=true,
            Some({
              ...t',
              pending: Some(text),
              col,
              col_target: None,
              error: None,
            }),
          ),
        )
      | Revert =>
        let saved =
          switch (lines[at]) {
          | Of_row(r) => E.units(r.r_name)
          | Of_new(_) => 0
          };
        Act(
          on.text.set(
            ~placed=true,
            Some({
              ...t',
              pending: None,
              error: None,
              col: saved,
              col_target: None,
            }),
          ),
        );
      | Save(after) =>
        Act(Effect.Many([relocated, focus_rows, on.text.save(after)]))
      | Abandon => Act(Effect.Many([focus_rows, on.text.abandon]))
      | NewTest(op, id)
      | Op(op, id) =>
        Act(Effect.Many([relocated, focus_rows, on.text.op(op, id)]))
      | Remove(id, to_) =>
        Act(
          Effect.Many([
            relocated,
            switch (to_) {
            | Some((line, col)) => go(~discard=true, line, col, None)
            | None => Effect.Many([focus_rows, on.text.abandon])
            },
            on.def_op(Delete, id),
          ]),
        )
      | Fold(path) =>
        Act(Effect.Many([relocated, forget, on.toggle_collapse(path)]))
      | Menu =>
        Act(
          switch (lines[at]) {
          | Of_row(r) => Option.value(open_menu(r), ~default=Effect.Ignore)
          | Of_new(_) => Effect.Ignore
          },
        )
      | Exit => Act(Effect.Many([relocated, on.text.save(Leave), on.leave]))
      };
    };
  /* a thunk: building a key's effect can move the cursor at once, so
     keys the menu takes must not build it */
  let act = (): verdict =>
    switch (on.text.get(), key, meta, cur) {
    | (Some(t), _, _, _) => text_key(t)
    | (None, "Enter", true, _)
    | (None, "F2", _, _) =>
      switch (cur, n) {
      | (Some(r), _) => Act(start_text(~on, r))
      | (None, 0) => Pass(Effect.Ignore)
      | (None, _) => Act(start_text(~on, visible[0]))
      }
    | (None, _, _, _) =>
      switch (nav_key()) {
      | Some(eff) => Act(eff)
      | None => Pass(Effect.Ignore)
      }
    };
  /* an open menu takes ↑↓, Enter and Esc; any other key closes it, then
     acts */
  let menu = on.get_menu();
  let menu_sel = on.get_menu_sel();
  let items =
    switch (menu) {
    | Some((at, _, _)) => menu_items(menu_rows(~on, at))
    | None => []
    };
  let n_items = List.length(items);
  switch (menu, key) {
  | (Some(_), "Escape") => stop(on.menu_close)
  | (Some(_), "ArrowDown") when n_items > 0 =>
    stop(on.menu_select((menu_sel + 1) mod n_items))
  | (Some(_), "ArrowUp") when n_items > 0 =>
    stop(on.menu_select((menu_sel + n_items - 1) mod n_items))
  | (Some(_), "Enter") =>
    switch (List.nth_opt(items, menu_sel)) {
    | Some(item) => stop(Effect.Many([on.menu_close, focus_rows, item()]))
    | None => stop(on.menu_close)
    }
  | (Some(_), "Shift" | "Alt" | "Meta" | "Control") => Effect.Ignore
  | (Some(_), _) =>
    switch (act()) {
    | Act(eff) => stop(Effect.Many([on.menu_close, eff]))
    | Own(eff) => Effect.Many([Effect.Stop_propagation, on.menu_close, eff])
    | Pass(eff) => Effect.Many([on.menu_close, eff])
    }
  | (None, _) =>
    switch (act()) {
    | Act(eff) => stop(eff)
    | Own(eff) => Effect.Many([Effect.Stop_propagation, eff])
    | Pass(eff) => eff
    }
  };
};

/* the breadcrumb: the program, then each zoomed module. An ancestor
   crumb zooms out to it; the last one shows its level's whole */
let header_view =
    (~on: handlers, ~typing: bool, header: header): list(Node.t) => {
  /* documentation slides are named "Folder / Name" */
  let program = {
    let s = header.h_program;
    let n = String.length(s);
    let rec last = (i, found) =>
      i + 3 > n
        ? found
        : last(i + 1, String.sub(s, i, 3) == " / " ? Some(i) : found);
    switch (last(0, None)) {
    | Some(i) => String.sub(s, i + 3, n - i - 3)
    | None => s
    };
  };
  let crumbs =
    [(Option.none, program)]
    @ List.map(((id, l)) => (Option.some(id), l), header.h_trail);
  let n = List.length(crumbs);
  let sep =
    span(~attrs=[clss(["outline-crumb-sep"])], [text({js|›|js})]);
  let crumb_nodes =
    List.concat(
      List.mapi(
        (i, (id, label)) => {
          let last = i == n - 1;
          let node =
            span(
              ~attrs=[
                clss(
                  ["outline-crumb"] @ (last ? ["outline-crumb-here"] : []),
                ),
                Attr.title(
                  last
                    ? header.h_open > 0 ? "show all of it" : label
                    : "zoom out to " ++ label,
                ),
                Attr.on_click(_ =>
                  stop(
                    last
                      ? header.h_open > 0
                          ? on.show_whole(true) : Effect.Ignore
                      : on.zoom_to(id),
                  )
                ),
              ],
              [text(label)],
            );
          i == 0 ? [node] : [sep, node];
        },
        crumbs,
      ),
    );
  let tail =
    header.h_open > 0
      ? [
        sep,
        span(
          ~attrs=[clss(["outline-crumb-count"])],
          [text(string_of_int(header.h_open) ++ " open")],
        ),
        span(
          ~attrs=[
            clss(["outline-crumb-btn"]),
            Attr.title("close these cells"),
            Attr.on_click(_ => stop(on.discard)),
          ],
          [text({js|×|js})],
        ),
      ]
      : header.h_parked > 0
          ? [
            span(
              ~attrs=[
                clss(["outline-crumb-chip"]),
                Attr.title("back to the cells"),
                Attr.on_click(_ => stop(on.show_whole(false))),
              ],
              [
                text(
                  {js|↩ |js} ++ string_of_int(header.h_parked) ++ " open",
                ),
              ],
            ),
          ]
          : [];
  [
    span(~attrs=[clss(["outline-menu-glyph"])], [text({js|☰|js})]),
    span(~attrs=[clss(["outline-word"])], [text("outline")]),
    span(~attrs=[clss(["outline-crumbs"])], crumb_nodes @ tail),
  ]
  /* text mode, said quietly */
  @ (
    typing
      ? [
        span(
          ~attrs=[clss(["outline-mode-hint"])],
          [text({js|editing names · esc|js})],
        ),
      ]
      : []
  );
};

let view = (~props: props, ~on: handlers, term: Language.Exp.t): Node.t => {
  /* zoomed, the module's members are the top level; collapse paths
     stay rooted at the program */
  let (roots, root_path) =
    switch (props.zoom_root) {
    | Some(m) =>
      switch (OutlineTree.node_of(m, term), OutlineTree.label_path(m, term)) {
      | (Some(n), Some(path)) => (n.o_children, path)
      | _ => (OutlineTree.of_term(term), [])
      }
    | None => (OutlineTree.of_term(term), [])
    };
  /* a new member typed first or last inside a module shows it open */
  let opened =
    switch (on.text.current) {
    | Some({line: New(a, First | Last), _}) => Some(a)
    | _ => None
    };
  /* the rows on screen, in order: the keyboard's list */
  let visible: array(visible_row) = {
    let rec walk = (parent, prefix, ns: list(OutlineTree.node)) =>
      List.concat_map(
        ((n: OutlineTree.node, seg)) => {
          let path = prefix @ [seg];
          let name = n.o_label;
          let n = live_label(props, n);
          let branch = n.o_children != [];
          let expanded =
            branch && (!props.is_collapsed(path) || n.o_id == opened);
          [
            {
              r_path: path,
              r_node: n,
              r_name: name,
              r_parent: parent,
              r_expanded: expanded,
            },
          ]
          @ (expanded ? walk(Some(path), path, n.o_children) : []);
        },
        OutlineTree.segs(ns),
      );
    Array.of_list(walk(None, root_path, roots));
  };
  /* text mode, where its caret is drawn */
  let tm =
    switch (on.text.current) {
    | Some(t) when props.stack_controls =>
      Option.map(
        ((lines, at, t')) => {
          if (!(t' === t)) {
            on.text.relocate(t');
          };
          on.text.note(at);
          {
            d_lines: lines,
            d_at: at,
            d_text: t',
          };
        },
        text_lines(~on, ~visible, t),
      )
    | _ => None
    };
  create(
    "details",
    ~attrs=
      [Attr.id("outline-sidebar"), Attr.create("open", "")]
      @ (props.arrows ? [] : [clss(["no-arrows"])]),
    [
      create(
        "summary",
        ~attrs=
          [
            clss(
              ["outline-title"]
              @ (tm == None && props.cursor == None ? ["outline-cursor"] : []),
            ),
          ]
          @ (
            props.stack_controls
              ? [
                Attr.on_mousedown(_ =>
                  tm == None ? take_cursor(on, None) : Effect.Prevent_default
                ),
              ]
              : []
          ),
        props.stack_controls
          ? header_view(~on, ~typing=tm != None, props.header)
          : [text({js|☰ outline|js})],
      ),
      div(~attrs=[clss(["outline-resize"]), ...resize_attrs], []),
      div(
        ~attrs=
          [clss(["outline-body"] @ (tm == None ? [] : ["text-mode"]))]
          @ (
            props.stack_controls
              ? [
                Attr.tabindex(0),
                Attr.on_keydown(keys(~props, ~on, ~visible)),
                Attr.on_focus(evt =>
                  within_rows(evt)
                    ? Effect.Stop_propagation
                    : Effect.Many([Effect.Stop_propagation, on.focused])
                ),
                /* leaving the outline saves a name being typed */
                Attr.on_blur(evt =>
                  switch (on.text.get()) {
                  | Some(_) when leaving(evt) =>
                    Effect.Many([
                      Effect.Stop_propagation,
                      on.text.save(Leave),
                    ])
                  | _ => Effect.Stop_propagation
                  }
                ),
                /* in text mode a click on the list's background (not
                   its scrollbar) leaves the keys where they are */
                Attr.on_mousedown(evt => {
                  let e = Js_of_ocaml.Js.Unsafe.coerce(evt);
                  on.text.get() != None
                  &&
                  e##.target ===
                  e##.currentTarget
                  &&
                  e##.offsetX <
                  e##.currentTarget##.clientWidth
                    ? Effect.Prevent_default : Effect.Ignore;
                }),
              ]
              : []
          ),
        roots == []
          ? [
            div(
              ~attrs=[clss(["outline-empty"])],
              [text("no definitions")],
            ),
          ]
          : rows_view(~props, ~on, ~tm, ~path=root_path, roots),
      ),
    ]
    @ (
      switch (props.menu) {
      | Some((at, x, y)) when props.stack_controls =>
        menu_view(
          ~menu_close=on.menu_close,
          ~rows=menu_rows(~on, at),
          ~selected=props.menu_sel,
          (x, y),
        )
      | _ => []
      }
    ),
  );
};
