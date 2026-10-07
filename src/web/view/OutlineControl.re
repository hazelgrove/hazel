open Haz3lcore;
open Util;
open Virtual_dom.Vdom;

/* The outline sidebar. Its transient UI state lives in the refs below;
   its collapse state is saved with the slide. */

module Scratchpad = ScratchModel.Scratchpad;
module Persist = ScratchPersist;

/* the context menu: (row id, whether a module, screen x, y) */
let menu: ref(option((Id.t, bool, float, float))) = ref(None);
/* its row the keys have selected */
let menu_sel: ref(int) = ref(0);
/* for handlers, where [menu] is shadowed */
let set_menu = m => menu := m;
let read_menu = () => menu^;

/* the keyboard cursor: a row path, None for the header row */
let cursor: ref(option(OutlineTree.path)) = ref(None);

/* a name being typed, and the row just created from a `type ` or
   `module ` name (its keyword animates away) */
let edit: ref(option(OutlineEdit.t)) = ref(None);
let created: ref(option((Id.t, string))) = ref(None);

/* the slide the state above belongs to */
let owner: ref(option(string)) = ref(None);

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* toggle a branch's collapse */
    | Collapse(OutlineTree.path)
    | Menu(option((Id.t, bool, float, float)))
    | MenuSel(int)
    | DefOp(OutlineSidebar.def_op, Id.t)
    | Cursor(option(OutlineTree.path))
    | Edit(option(OutlineEdit.t))
    /* true: then a new one */
    | Commit(OutlineEdit.t, bool)
    /* the outline took keyboard focus */
    | Focused
    /* give the outline keyboard focus */
    | Focus;
};

open Action;

/* the scratch or documentation slides, with the prefix their saved
   state is keyed by */
type deck = (string, ScratchModel.Model.t);

let current = ((_, m): deck): option(Scratchpad.t) =>
  List.nth_opt(m.scratchpads, m.current);

let slide_name = (deck: deck): string =>
  switch (current(deck)) {
  | Some(sp) => sp.name
  | None => ""
  };

let program_of = (deck: option(deck)): option(Program.t) =>
  switch (Option.bind(deck, current)) {
  | Some({kind: Code({program, _}), _}) => Some(program)
  | _ => None
  };

let collapsed = (deck: option(deck)): list(OutlineTree.path) =>
  switch (deck) {
  | Some((prefix, _) as d) => Persist.collapse_paths(prefix, slide_name(d))
  | None => []
  };

let update =
    (
      ~settings: Settings.t,
      ~schedule_workspace: Workspace.Action.t => unit,
      ~prefix: string,
      ~name: string,
      action: Action.t,
      code: Scratchpad.code,
    )
    : Updated.t(Scratchpad.code) =>
  switch (action) {
  | Menu(m) =>
    menu := m;
    menu_sel := 0;
    code |> Updated.return_quiet;
  | MenuSel(i) =>
    menu_sel := i;
    code |> Updated.return_quiet;
  | Collapse(path) =>
    let ck = Persist.content_key(prefix, name);
    let cur = Persist.collapse_paths(prefix, name);
    let next =
      List.mem(path, cur)
        ? List.filter(p => p != path, cur) : [path, ...cur];
    next == []
      ? Hashtbl.remove(Persist.slide_collapse, ck)
      : Hashtbl.replace(Persist.slide_collapse, ck, next);
    Persist.write_collapse(prefix, name);
    code |> Updated.return_quiet;
  | DefOp(op, fid) =>
    menu := None;
    let program = code.program;
    /* a delete leaves the cursor where the row was: its old path names
       another row when names repeat */
    let successor =
      op == Delete
        ? OutlineTree.successor(fid, Program.statics(program).term) : None;
    switch (
      ItemEdit.edit(
        Workspace.item_ctx(
          ~settings,
          ~collapsed=Persist.collapse_paths(prefix, name),
          program,
        ),
        Op(op, fid),
        Program.document(program),
      )
    ) {
    | Error(_) => code |> Updated.return_quiet
    | Ok((new_seg, target)) =>
      let code = Workspace.with_segment(~settings, code, new_seg);
      if (op == Delete) {
        cursor :=
          Option.bind(successor, id =>
            OutlineTree.label_path(id, Program.statics(code.program).term)
          );
      };
      switch (op, target, code.program) {
      | (MoveUp | MoveDown, Some(id), _) =>
        cursor :=
          OutlineTree.label_path(id, Program.statics(code.program).term)
      | (_, Some(id), Whole(_)) => schedule_workspace(FocusToggle(id))
      | (_, Some(id), Divided(d)) when Divided.owner(id, d) == None =>
        schedule_workspace(FocusEnsure(id))
      | _ => ()
      };
      code |> Updated.return;
    };
  | Cursor(c) =>
    cursor := c;
    created := None;
    code |> Updated.return_quiet;
  | Edit(e) =>
    edit := e;
    created := None;
    code |> Updated.return_quiet;
  | Commit(ed, then_new) =>
    /* nothing reaches the program until here: a rename is one
       refactoring, a new definition one insertion */
    let fresh_below = (id: Id.t): OutlineEdit.t => {
      ed_row: None,
      ed_anchor: Some(id),
      ed_inside: false,
      ed_text: "",
      ed_caret: 0,
      ed_error: None,
    };
    let refuse = why => {
      edit :=
        Some({
          ...ed,
          ed_error: Some(why),
        });
      code |> Updated.return_quiet;
    };
    let program = code.program;
    let text = String.trim(ed.ed_text);
    let ctx = Workspace.item_ctx(~settings, ~collapsed=[], program);
    let term = ctx.term;
    let apply = e => ItemEdit.edit(ctx, e, Program.document(program));
    switch (ed.ed_row, ed.ed_anchor) {
    | (Some(row), _) =>
      let old =
        Option.map(
          (n: OutlineTree.node) => n.o_label,
          OutlineTree.node_of(row, term),
        );
      if (old == Some(text)) {
        edit := then_new ? Some(fresh_below(row)) : None;
        code |> Updated.return_quiet;
      } else if (!settings.core.statics) {
        refuse("renaming needs statics on");
      } else {
        switch (apply(Rename(row, text))) {
        | Error(why) => refuse(why)
        | Ok((new_seg, _)) =>
          let code = Workspace.with_segment(~settings, code, new_seg);
          let new_term = Program.statics(code.program).term;
          cursor := OutlineTree.label_path(row, new_term);
          edit := then_new ? Some(fresh_below(row)) : None;
          code |> Updated.return;
        };
      };
    | (None, Some(anchor)) when text == "" =>
      edit := None;
      cursor := OutlineTree.label_path(anchor, term);
      code |> Updated.return_quiet;
    | (None, Some(anchor)) =>
      switch (
        apply(
          ed.ed_inside ? InsertInside(anchor, text) : Insert(anchor, text),
        )
      ) {
      | Error(why) => refuse(why)
      | Ok((new_seg, target)) =>
        let code = Workspace.with_segment(~settings, code, new_seg);
        let new_term = Program.statics(code.program).term;
        let prefix = snd(OutlineSidebar.new_kind(text));
        switch (target) {
        | Some(id) =>
          cursor := OutlineTree.label_path(id, new_term);
          edit := then_new ? Some(fresh_below(id)) : None;
          created := prefix == "" ? None : Some((id, prefix));
        | None => edit := None
        };
        code |> Updated.return;
      }
    | (None, None) =>
      edit := None;
      code |> Updated.return_quiet;
    };
  | Focused =>
    /* the outline starts at the row holding the caret */
    switch (OutlineFollow.mark^) {
    | Some(id) =>
      cursor :=
        OutlineTree.label_path(id, Program.statics(code.program).term)
    | None => ()
    };
    code |> Updated.return_quiet;
  | Focus =>
    JsUtil.focus_outline();
    code |> Updated.return_quiet;
  };

/* outside the scratch and documentation decks the outline only
   navigates: its view state moves, and nothing reaches a program */
let update_view = (a: Action.t): unit =>
  switch (a) {
  | Cursor(c) => cursor := c
  | Focus => JsUtil.focus_outline()
  | Collapse(_)
  | Menu(_)
  | MenuSel(_)
  | DefOp(_)
  | Edit(_)
  | Commit(_)
  | Focused => ()
  };

/* the row holding the editor's caret, or its nearest visible ancestor
   when collapsed away (OutlineFollow) */
let mark = (~deck: option(deck), ~zipper: Zipper.t): option(Id.t) =>
  switch (program_of(deck)) {
  | None => None
  | Some(p) =>
    let statics = Program.statics(p);
    let term = statics.term;
    let rows = OutlineTree.row_ids(term);
    let row_of = id =>
      switch (Id.Map.find_opt(id, statics.info_map)) {
      | Some(info) =>
        List.find_opt(
          x => Id.Map.mem(x, rows),
          [id, ...Language.Info.ancestors_of(info)],
        )
      | None => None
      };
    /* the indicated term, else the enclosing tiles, else the
       neighbours (a caret in a comment or between items) */
    let candidates =
      Option.to_list(Indicated.index(zipper))
      @ List.map(((a: Ancestor.t, _)) => a.id, zipper.relatives.ancestors)
      @ (
        switch (Siblings.neighbors(zipper.relatives.siblings)) {
        | (l, r) => List.filter_map(x => Option.map(Piece.id, x), [r, l])
        }
      );
    let row =
      switch (List.find_map(row_of, candidates), p) {
      | (Some(r), _) => Some(r)
      | (None, Divided(d)) => Option.map(fst, Divided.active(d))
      | (None, Whole(_)) => None
      };
    let collapsed = collapsed(deck);
    /* a folded ancestor shows the mark instead; with nothing folded the
       row is its own, so skip the walks (this runs every render) */
    Option.map(
      r =>
        switch (collapsed == [] ? None : OutlineTree.trail_of(r, term)) {
        | Some(trail) =>
          List.find_opt(
            id =>
              id != r
              && (
                switch (OutlineTree.label_path(id, term)) {
                | Some(path) => List.mem(path, collapsed)
                | None => false
                }
              ),
            trail,
          )
          |> Option.value(~default=r)
        | None => r
        },
      row,
    );
  };

/* single-slot vdom memo: building the outline is O(program), but its
   inputs change on statics frames and outline use, not per keystroke */
type memo_key = {
  k_statics: CachedStatics.t,
  k_focused: list((Id.t, option(string))),
  k_deck: bool,
  k_name: string,
  k_collapsed: list(OutlineTree.path),
  k_menu: option((Id.t, bool, float, float)),
  k_menu_sel: int,
  k_results: option(Language.TestResults.t),
  k_view: option(SlideView.t),
  k_cursor: option(OutlineTree.path),
  k_edit: option(OutlineEdit.t),
  k_created: option((Id.t, string)),
};

let memo: ref(option((memo_key, Node.t))) = ref(None);

let same = (a: memo_key, b: memo_key): bool =>
  a.k_statics === b.k_statics
  && a.k_focused == b.k_focused
  && a.k_deck == b.k_deck
  && a.k_name == b.k_name
  && a.k_collapsed == b.k_collapsed
  && a.k_menu == b.k_menu
  && a.k_menu_sel == b.k_menu_sel
  && a.k_view == b.k_view
  && a.k_cursor == b.k_cursor
  && a.k_edit == b.k_edit
  && a.k_created == b.k_created
  && (
    switch (a.k_results, b.k_results) {
    | (Some(x), Some(y)) => x === y
    | (None, None) => true
    | _ => false
    }
  );

/* the outline of the slide's whole program in Scratch and Documentation,
   else of the current editor's [statics] and [segment] */
let view =
    (
      ~deck: option(deck),
      ~statics: CachedStatics.t,
      ~segment: Segment.t,
      ~inject: Action.t => Effect.t(unit),
      ~inject_workspace: Workspace.Action.t => Effect.t(unit),
      ~jump: Id.t => Effect.t(unit),
      ~leave: Effect.t(unit),
    )
    : Node.t => {
  let program = program_of(deck);
  let statics =
    switch (program) {
    | Some(p) => Program.statics(p)
    | None => statics
    };
  let is_deck = deck != None;
  let name = Option.fold(~none="", ~some=slide_name, deck);
  /* a menu, cursor or name edit doesn't follow you to another slide */
  let slide =
    Option.map(
      ((prefix, _) as d) => Persist.content_key(prefix, slide_name(d)),
      deck,
    );
  if (slide != owner^) {
    owner := slide;
    menu := None;
    cursor := None;
    edit := None;
    created := None;
  };
  let collapsed = collapsed(deck);
  let focused =
    switch (deck) {
    | Some((_, m)) => ScratchModel.Model.focused_names(m)
    | None => []
    };
  let slide_view =
    switch (deck) {
    | Some((_, m)) => ScratchModel.Model.current_view(m)
    | None => None
    };
  let results =
    Option.bind(program, p =>
      EvalResult.Model.test_results(Program.result(p))
    );
  let menu = is_deck ? menu^ : None;
  let key = {
    k_statics: statics,
    k_focused: focused,
    k_deck: is_deck,
    k_name: name,
    k_collapsed: collapsed,
    k_menu: menu,
    k_menu_sel: menu_sel^,
    k_results: results,
    k_view: slide_view,
    k_cursor: cursor^,
    k_edit: edit^,
    k_created: created^,
  };
  switch (memo^) {
  | Some((k, node)) when same(k, key) => node
  | _ =>
    /* statics compacted by undo have no term until they recompute: parse
       this slide's program instead (never another document's) */
    let term =
      switch (program) {
      | Some(p)
          when
            !
              List.exists(
                (n: OutlineTree.node) => n.o_label != "",
                OutlineTree.of_term(statics.term),
              ) =>
        let seg = Program.document(p);
        Program.root(p) == Sort.Mod
          ? MakeTerm.Incr.term_of_mod(seg) : MakeTerm.Incr.term_of(seg);
      | _ => statics.term
      };
    /* each error badges the deepest row containing it; ancestor rows
       get a roll-up badge that CSS shows only while collapsed */
    let (error_items, error_subtree) = {
      let outline_ids = {
        let rec go = (acc, ns: list(OutlineTree.node)) =>
          List.fold_left(
            (acc, n: OutlineTree.node) =>
              go(
                switch (n.o_id) {
                | Some(id) => [id, ...acc]
                | None => acc
                },
                n.o_children,
              ),
            acc,
            ns,
          );
        go([], OutlineTree.of_term(term));
      };
      let in_outline = id => List.mem(id, outline_ids);
      List.fold_left(
        ((direct, roll), err_id) => {
          let path =
            switch (Id.Map.find_opt(err_id, statics.info_map)) {
            | Some(info) => [err_id, ...Language.Info.ancestors_of(info)]
            | None => [err_id]
            };
          switch (List.filter(in_outline, path)) {
          | [] => (direct, roll)
          | [deepest, ...above] => ([deepest, ...direct], above @ roll)
          };
        },
        ([], []),
        statics.error_ids,
      );
    };
    let label = id =>
      switch (OutlineTree.node_of(id, term)) {
      | Some(n) => n.o_label
      | None => ""
      };
    let (h_open, h_parked) =
      switch (slide_view) {
      | Some(v) =>
        let n = List.length(SlideView.visible(~term, v));
        v.parked ? (0, n) : (n, 0);
      | None => (0, 0)
      };
    let props: OutlineSidebar.props = {
      stack_controls: is_deck,
      can_open: {
        let incomplete =
          Segment.incomplete_tiles_deep(
            switch (program) {
            | Some(Divided(d)) => Divided.document(d)
            | _ => segment
            },
          )
          |> List.map((t: Tile.t) => t.id);
        (id => !List.mem(id, incomplete));
      },
      is_collapsed: path => List.mem(path, collapsed),
      header: {
        h_program: name,
        h_trail:
          switch (slide_view) {
          | Some(v) => List.map(id => (id, label(id)), v.zoom)
          | None => []
          },
        h_open,
        h_parked,
      },
      zoom_root: Option.bind(slide_view, SlideView.zoom_root),
      cursor: cursor^,
      created: created^,
      pinned:
        switch (slide_view) {
        | Some(v) => List.map((p: SlideView.pin) => p.p_id, v.pins)
        | None => []
        },
      focused_entries: focused,
      error_items,
      error_subtree,
      menu,
      menu_sel: menu_sel^,
      /* live ✓/✗ for test rows, from the whole program's result */
      test_status: id =>
        Option.bind(results, (tr: Language.TestResults.t) =>
          Language.TestMap.lookup(id, tr.test_map)
          |> Option.map(Language.TestMap.joint_status)
        ),
    };
    /* a row inside an open cell can't open on its own: go to it there */
    let nested = id =>
      switch (program) {
      | Some(Divided(d)) =>
        switch (Divided.owner(id, d)) {
        | Some(e) => e.e_id != id
        | None => false
        }
      | _ => false
      };
    let on: OutlineSidebar.handlers = {
      jump,
      /* a plain click with cells open adds (or moves to) that cell; it
         never replaces them */
      focus: id =>
        nested(id) ? jump(id) : inject_workspace(FocusEnsure(id)),
      toggle: id =>
        nested(id) ? jump(id) : inject_workspace(FocusToggle(id)),
      toggle_run: id => inject_workspace(FocusToggleRun(id)),
      toggle_collapse: path => inject(Collapse(path)),
      zoom_to: m => inject_workspace(ZoomTo(m)),
      zoom_in: id => inject_workspace(ZoomIn(id)),
      show_whole: b => inject_workspace(ShowWhole(b)),
      discard: inject_workspace(UnfocusDef),
      zoom_out: inject_workspace(ZoomOut),
      get_cursor: () => cursor^,
      /* the ref moves at the keypress; the action re-renders */
      set_cursor: c => {
        cursor := c;
        inject(Cursor(c));
      },
      focused: inject(Focused),
      leave,
      edit: {
        current: edit^,
        get: () => edit^,
        set: e => {
          edit := e;
          inject(Edit(e));
        },
        commit: (ed, then_new) => {
          edit := None;
          inject(Commit(ed, then_new));
        },
      },
      /* like the cursor, the menu moves at the event */
      menu_open: (id, is_module, x, y) =>
        if (is_deck) {
          set_menu(Some((id, is_module, x, y)));
          menu_sel := 0;
          inject(Menu(Some((id, is_module, x, y))));
        } else {
          Effect.Ignore;
        },
      menu_close: inject(Menu(None)),
      menu_select: i => {
        menu_sel := i;
        inject(MenuSel(i));
      },
      get_menu: () => is_deck ? read_menu() : None,
      get_menu_sel: () => menu_sel^,
      def_op: (op, id) => inject(DefOp(op, id)),
    };
    let node = OutlineSidebar.view(~props, ~on, term);
    memo := Some((key, node));
    node;
  };
};
