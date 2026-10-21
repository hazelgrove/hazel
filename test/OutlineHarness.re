open Haz3lcore;

/* the outline sidebar driven without a page: its rows for a program, its
   handlers and props (nothing happens unless a test says), and keys as
   the browser sends them */

module S = Web.OutlineSidebar;
module E = Web.OutlineEdit;
module O = Web.OutlineTree;

let ignore = Virtual_dom.Vdom.Effect.Ignore;
let none = _ => ignore;

/* [src]'s rows as the sidebar lists them: every one shown unless
   [collapsed] (by label), or, given [folds], as a slide folds them */
let visible =
    (~collapsed: list(string)=[], ~folds=?, term: Language.Exp.t)
    : array(S.visible_row) =>
  S.visible_rows(
    ~folded=
      (path, n) =>
        switch (folds) {
        | Some(folds) => O.folded(~folds, path, n.o_kind)
        | None => List.mem(n.o_label, collapsed)
        },
    O.of_term(term),
  );

/* text mode's state kept as OutlineControl keeps it, minus the program;
   the ops it asks for go to [ops], a new definition's keys to [edits] */
let text_ctl =
    (
      ~ops: ref(list((E.def_op, Id.t)))=ref([]),
      ~edits: ref(list(E.edit))=ref([]),
      state: ref(option(E.t)),
    )
    : S.text_ctl => {
  current: state^,
  get: () => state^,
  set: (~placed as _=false, t) => {
    state := t;
    ignore;
  },
  start: (~kw as _="", line, col) => {
    state := Some(E.at(line, ~col));
    ignore;
  },
  go: (~discard as _=false, line, col, col_target) => {
    state :=
      Some({
        ...E.at(line, ~col),
        col_target,
      });
    ignore;
  },
  save: _ => ignore,
  abandon: () => ignore,
  discard: _ => ignore,
  type_: edit => {
    edits := [edit, ...edits^];
    ignore;
  },
  paste: () => ignore,
  rekind: _ => ignore,
  shift: _ => ignore,
  op: (op, id) => {
    ops := [(op, id), ...ops^];
    ignore;
  },
  flash: ignore,
  relocate: t => state := Some(t),
  note: _ => (),
  last: () => None,
};

let handlers =
    (
      ~cursor: ref(option(O.path))=ref(None),
      ~text: ref(option(E.t))=ref(None),
      ~ops: ref(list((E.def_op, Id.t)))=ref([]),
      ~edits: ref(list(E.edit))=ref([]),
      ~toggle: Id.t => Virtual_dom.Vdom.Effect.t(unit)=none,
      ~toggle_collapse: O.path => Virtual_dom.Vdom.Effect.t(unit)=none,
      ~def_op: (E.def_op, Id.t) => Virtual_dom.Vdom.Effect.t(unit)=(_, _) =>
                                                                    ignore,
      ~went: ref(list(bool))=ref([]),
      ~menus: ref(int)=ref(0),
      (),
    )
    : S.handlers => {
  jump: none,
  focus: none,
  toggle,
  toggle_run: none,
  toggle_collapse,
  zoom_to: none,
  go_above: up => {
    went := [up, ...went^];
    ignore;
  },
  above_chip: ignore,
  zoom_in: none,
  show_whole: none,
  discard: ignore,
  zoom_out: ignore,
  get_cursor: () => cursor^,
  set_cursor: c => {
    cursor := c;
    ignore;
  },
  focused: ignore,
  leave: ignore,
  text: text_ctl(~ops, ~edits, text),
  menu_open: (_, _, _) => {
    incr(menus);
    ignore;
  },
  menu_close: ignore,
  menu_select: none,
  get_menu: () => None,
  get_menu_sel: () => 0,
  def_op,
};

let props =
    (
      ~cursor: option(O.path)=None,
      ~refusal: Id.t => option(Web.ScratchFocus.refusal)=_ => None,
      ~check: E.t => option(string)=_ => None,
      ~level: S.level=Plain,
      (),
    )
    : S.props => {
  arrows: true,
  /* the level above has no stack */
  stack_controls:
    switch (level) {
    | Enclosing(_) => false
    | Plain
    | InSlide(_) => true
    },
  refusal,
  folded: (_, _) => false,
  header: {
    h_level: level,
    h_program: "",
    h_trail: [],
    h_open: 0,
    h_parked: 0,
  },
  zoom_root: None,
  cursor,
  created: None,
  flash: 0,
  pinned: [],
  focused_entries: [],
  error_items: [],
  error_subtree: [],
  menu: None,
  menu_sel: 0,
  test_status: _ => None,
  blank: _ => false,
  live: None,
  check,
  ux: None,
};

/* a keydown as the browser sends it, on the row list */
let keydown = (~meta=false, ~alt=false, ~shift=false, ~code=?, key: string) =>
  Js_of_ocaml.Js.(
    Unsafe.coerce(
      Unsafe.obj([|
        ("key", Unsafe.inject(string(key))),
        ("code", Unsafe.inject(string(Option.value(code, ~default=key)))),
        ("metaKey", Unsafe.inject(bool(meta))),
        ("ctrlKey", Unsafe.inject(_false)),
        ("altKey", Unsafe.inject(bool(alt))),
        ("shiftKey", Unsafe.inject(bool(shift))),
        (
          "target",
          Unsafe.inject(
            Unsafe.obj([|
              (
                "classList",
                Unsafe.inject(
                  Unsafe.obj([|
                    ("contains", Unsafe.inject(wrap_callback(_ => _false))),
                  |]),
                ),
              ),
            |]),
          ),
        ),
      |]),
    )
  );
