open Haz3lcore;

/* the outline sidebar driven without a page: its rows for a program, its
   handlers and props (nothing happens unless a test says), and keys as
   the browser sends them */

module S = Web.OutlineSidebar;
module E = Web.OutlineEdit;
module O = Web.OutlineTree;

let ignore = Virtual_dom.Vdom.Effect.Ignore;
let none = _ => ignore;

/* [src]'s rows, every one shown unless [collapsed], as the sidebar lists
   them */
let visible =
    (~collapsed: list(string)=[], term: Language.Exp.t)
    : array(S.visible_row) => {
  let rec walk = (parent, prefix, ns: list(O.node)) =>
    List.concat_map(
      ((n: O.node, seg)) => {
        let path = prefix @ [seg];
        let expanded = n.o_children != [] && !List.mem(n.o_label, collapsed);
        [
          S.{
            r_path: path,
            r_node: n,
            r_name: n.o_label,
            r_parent: parent,
            r_expanded: expanded,
          },
        ]
        @ (expanded ? walk(Some(path), path, n.o_children) : []);
      },
      O.segs(ns),
    );
  Array.of_list(walk(None, [], O.of_term(term)));
};

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
      (),
    )
    : S.handlers => {
  jump: none,
  focus: none,
  toggle,
  toggle_run: none,
  toggle_collapse,
  zoom_to: none,
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
  menu_open: (_, _, _) => ignore,
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
      (),
    )
    : S.props => {
  arrows: true,
  stack_controls: true,
  refusal,
  is_collapsed: _ => false,
  header: {
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
