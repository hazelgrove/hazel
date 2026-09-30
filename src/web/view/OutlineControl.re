open Haz3lcore;
open Util;
open Virtual_dom.Vdom;

/* The outline sidebar: what it shows and what it does. Its UI state
   (context menu, keyboard cursor, a name being typed) lives here; its
   collapse state is saved with the slide. */

module Scratchpad = ScratchModel.Scratchpad;
module Persist = ScratchPersist;

/* context-menu state (row id, whether a module, screen position):
   transient UI, not model data */
let menu: ref(option((Id.t, bool, float, float))) = ref(None);

/* the keyboard cursor: a row path, None for the header row */
let cursor: ref(option(OutlineTree.path)) = ref(None);

/* a name being typed, and the row just created from a `type ` or
   `module ` name (its keyword animates away) */
let edit: ref(option(OutlineEdit.t)) = ref(None);
let created: ref(option((Id.t, string))) = ref(None);

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* toggle a branch's collapse */
    | Collapse(OutlineTree.path)
    | Menu(option((Id.t, bool, float, float)))
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
      switch (op, target, code.program) {
      | (MoveUp | MoveDown, Some(id), _) =>
        /* the cursor follows the moved item */
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
    Option.map(
      r =>
        switch (OutlineTree.trail_of(r, term)) {
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

/* single-slot vdom memo: the roll-up walk, row construction and diff
   are O(program) per render, and the inputs change on statics frames
   and outline interaction, not per keystroke. Parts rebuilt on change
   compare physically, small ones structurally. */
type memo_key = {
  k_statics: CachedStatics.t,
  k_focused: list((Id.t, option(string))),
  k_deck: bool,
  k_name: string,
  k_collapsed: list(OutlineTree.path),
  k_menu: option((Id.t, bool, float, float)),
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

/* the outline of the current program: the slide's whole program in
   Scratch and Documentation, else the current editor's ([statics],
   [segment]) */
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
    k_results: results,
    k_view: slide_view,
    k_cursor: cursor^,
    k_edit: edit^,
    k_created: created^,
  };
  switch (memo^) {
  | Some((k, node)) when same(k, key) => node
  | _ =>
    /* statics compacted by undo carry no term until they recompute:
       the outline parses this slide's program instead (never another
       document's) */
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
      /* live ✓/✗ for test rows, from the whole program's result */
      test_status: id =>
        Option.bind(results, (tr: Language.TestResults.t) =>
          Language.TestMap.lookup(id, tr.test_map)
          |> Option.map(Language.TestMap.joint_status)
        ),
    };
    let on: OutlineSidebar.handlers = {
      jump,
      /* a plain click with cells open adds (or moves to) that cell; it
         never replaces them */
      focus: id => inject_workspace(FocusEnsure(id)),
      toggle: id => inject_workspace(FocusToggle(id)),
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
      menu_open: (id, is_module, x, y) =>
        is_deck ? inject(Menu(Some((id, is_module, x, y)))) : Effect.Ignore,
      menu_close: inject(Menu(None)),
      def_op: (op, id) => inject(DefOp(op, id)),
    };
    let node = OutlineSidebar.view(~props, ~on, term);
    memo := Some((key, node));
    node;
  };
};
