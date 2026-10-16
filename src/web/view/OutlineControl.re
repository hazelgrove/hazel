open Haz3lcore;
open Util;
open Virtual_dom.Vdom;

/* The outline sidebar. Its transient UI state lives in the refs below,
   which the view's handlers write at the event (keys can outrun renders);
   its collapse state is saved with the slide. */

module Scratchpad = ScratchModel.Scratchpad;
module Persist = ScratchPersist;
module E = OutlineEdit;

/* the context menu: its row, and where (screen x, y) */
let menu: ref(option((E.menu_at, float, float))) = ref(None);
/* its row the keys have selected */
let menu_sel: ref(int) = ref(0);

/* the keyboard cursor: a row path, None for the header row */
let cursor: ref(option(OutlineTree.path)) = ref(None);

/* text mode: the caret and what's typed on its line (OutlineEdit), the
   line it was last drawn on, and how many refusals its line has flashed */
let text: ref(option(E.t)) = ref(None);
let drawn: ref(option(int)) = ref(None);
let flash: ref(int) = ref(0);
/* the caret's placements, so a field still shown moves its own caret */
let seq: ref(int) = ref(0);
let next_seq = () => {
  incr(seq);
  seq^;
};

/* the row just created from a `type ` or `module ` name (its keyword
   animates away) */
let created: ref(option((Id.t, string))) = ref(None);

/* the slide the state above belongs to */
let owner: ref(option(string)) = ref(None);

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    /* toggle a branch's collapse */
    | Collapse(OutlineTree.path)
    | DefOp(E.def_op, Id.t)
    | Save(E.save, E.after)
    /* the refs above moved: draw them */
    | Redraw
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

/* an item whose definition holds only holes (or an empty module): its
   name erased, Backspace removes it */
let blank_body = (program: Program.t, id: Id.t): bool =>
  switch (ScratchFocus.find_def(id, Program.document(program))) {
  | Some(def) =>
    List.for_all(
      (p: Piece.t) =>
        switch (p) {
        | Secondary(w) => !Secondary.is_comment(w)
        | Grout(_) => true
        | Tile(t) =>
          ItemEdit.is_empty_body(t)
          || (
            switch (Tile.label(t)) {
            | [tok] => Token.is_explicit_hole(tok)
            | _ => false
            }
          )
        | Projector(_) => false
        },
      def,
    )
  | None => false
  };

/* where the editor shows item [id]: lines of the editor holding it, or
   its open cell */
let spot = (program: Program.t, id: Id.t): option(OutlineFollow.spot) => {
  let lines = (cell: CellEditor.Model.t) => {
    let m = cell.editor.editor.syntax.measured;
    Measured.chunk_for_id(id, m) == None
      ? None
      : Measured.find_by_id(id, m)
        |> Option.map((m: Measured.measurement) =>
             (m.origin.row, m.last.row)
           );
  };
  switch (program) {
  | Whole(e) =>
    Option.map(rows => OutlineFollow.Lines(None, rows), lines(e))
  | Divided(d) =>
    switch (Divided.owner(id, d)) {
    | None => None
    | Some(c) =>
      let i =
        Option.value(
          List.find_index(
            (e: ScratchCell.t) => e.e_id == c.e_id,
            Divided.cells(d),
          ),
          ~default=0,
        );
      switch (c.e_id == id ? None : lines(c.e_body)) {
      | Some(rows) => Some(Lines(Some(i), rows))
      | None => Some(Cell(i))
      };
    }
  };
};

/* what the editor shows for row [id] in [term] */
let shown = (id: Id.t, term: Language.Exp.t): option(Id.t) =>
  Option.bind(OutlineTree.node_of(id, term), E.shown_of_node);

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
  | Redraw => code |> Updated.return_quiet
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
    /* text mode's caret on the op's row */
    let on_row =
      switch (text^) {
      | Some({line: Row(_, Some(Item(id))), _}) => id == fid
      | _ => false
      };
    let typing = text^ != None;
    let ctx =
      Workspace.item_ctx(
        ~settings,
        ~collapsed=Persist.collapse_paths(prefix, name),
        program,
      );
    /* a delete leaves the cursor where the row was: its old path names
       another row when names repeat */
    let successor =
      op == Delete ? OutlineTree.successor(fid, ctx.term) : None;
    switch (ItemEdit.edit(ctx, Op(op, fid), Program.document(program))) {
    | Error(_) =>
      if (typing) {
        incr(flash);
      };
      code |> Updated.return_quiet;
    | Ok((new_seg, target)) =>
      let code = Workspace.with_segment(~settings, code, new_seg);
      let new_term = Program.statics(code.program).term;
      let col = Option.fold(~none=0, ~some=(t: E.t) => t.col, text^);
      /* the caret, or the cursor, onto row [id]: what's typed moves with
         it, or ([fresh]) a column on a line of its own */
      let put = (~fresh=?, id: option(Id.t)) => {
        let path =
          Option.bind(id, id => OutlineTree.label_path(id, new_term));
        switch (text^, path, id) {
        | (Some(t), Some(path), Some(id)) =>
          let line = E.Row(path, Some(Item(id)));
          text :=
            Some(
              switch (fresh) {
              | Some(col) => E.at(line, ~col, ~seq=next_seq())
              | None => {
                  ...t,
                  line,
                  seq: next_seq(),
                }
              },
            );
          OutlineFollow.wanted := shown(id, new_term);
        /* the last row gone */
        | (Some(_), None, _) when op == Delete => text := None
        | (Some(_), _, _) => ()
        | (None, _, _) => cursor := path
        };
      };
      switch (op, target) {
      | (Delete, _) when !typing || on_row => put(~fresh=col, successor)
      | (MoveUp | MoveDown, Some(_)) when !typing || on_row => put(target)
      | (NewTestBelow | NewTestAbove, Some(_)) => put(~fresh=0, target)
      | (Duplicate, Some(_)) when on_row => put(~fresh=col, target)
      | _ => ()
      };
      /* a new item opens as a cell, as new definitions do */
      switch (op, target, code.program) {
      | (MoveUp | MoveDown, _, _) => ()
      | (_, Some(id), Whole(_)) => schedule_workspace(FocusToggle(id))
      | (_, Some(id), Divided(d)) when Divided.owner(id, d) == None =>
        schedule_workspace(FocusEnsure(id))
      | _ => ()
      };
      code |> Updated.return;
    };
  | Save(sv, after) =>
    /* nothing reaches the program until here: a rename is one
       refactoring, a new definition one insertion */
    let refuse = why => {
      text :=
        Some({
          ...E.at(sv.sv_line, ~col=sv.sv_col, ~pending=sv.sv_text),
          error: Some(why),
          seq: next_seq(),
        });
      code |> Updated.return_quiet;
    };
    let program = code.program;
    let ctx = Workspace.item_ctx(~settings, ~collapsed=[], program);
    let apply = e => ItemEdit.edit(ctx, e, Program.document(program));
    let txt = String.trim(sv.sv_text);
    /* the save landed on row [id]: what follows it */
    let landed = (code: Scratchpad.code, id: Id.t) => {
      let term = Program.statics(code.program).term;
      switch (after, text^) {
      /* unless the keys moved the caret on meanwhile */
      | (Next, Some(t)) when E.same_line(t.line, sv.sv_line) =>
        text := Some(E.at(New(id, Below), ~seq=next_seq()))
      | (Leave, _) => cursor := OutlineTree.label_path(id, term)
      | _ => ()
      };
    };
    switch (sv.sv_line) {
    | Row(_, Some(Item(row))) =>
      switch (OutlineTree.node_of(row, ctx.term)) {
      | None => code |> Updated.return_quiet
      | Some(n) when n.o_label == txt =>
        landed(code, row);
        code |> Updated.return_quiet;
      | Some(_) when !settings.core.statics =>
        refuse("renaming needs statics on")
      | Some(_) =>
        switch (apply(Rename(row, txt))) {
        | Error(why) => refuse(why)
        | Ok((new_seg, _)) =>
          let code = Workspace.with_segment(~settings, code, new_seg);
          landed(code, row);
          code |> Updated.return;
        }
      }
    | New(anchor, _) when txt == "" =>
      if (after == Leave) {
        cursor := OutlineTree.label_path(anchor, ctx.term);
      };
      code |> Updated.return_quiet;
    | New(anchor, place) =>
      switch (
        apply(
          switch (place) {
          | Below => Insert(anchor, txt)
          | First => InsertFirst(anchor, txt)
          | Last => InsertInside(anchor, txt)
          },
        )
      ) {
      | Error(why) => refuse(why)
      | Ok((new_seg, target)) =>
        let code = Workspace.with_segment(~settings, code, new_seg);
        switch (target) {
        | Some(id) =>
          let kw = snd(E.new_kind(txt));
          created := kw == "" ? None : Some((id, kw));
          landed(code, id);
        | None => ()
        };
        code |> Updated.return;
      }
    | Row(_) => code |> Updated.return_quiet
    };
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
    let row_of = id =>
      OutlineTree.enclosing_row(~info_map=statics.info_map, id, term);
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
        switch (collapsed == [] ? None : OutlineTree.find_path(r, term)) {
        | Some(chain) =>
          /* the outermost folded row above */
          let rec go = (path, chain) =>
            switch (chain) {
            | [(n: OutlineTree.node, seg), ...rest] =>
              let path = path @ [seg];
              switch (n.o_id) {
              | Some(id) when id != r && List.mem(path, collapsed) => id
              | _ => go(path, rest)
              };
            | [] => r
            };
          go([], chain);
        | None => r
        },
      row,
    );
  };

/* single-slot vdom memo: building the outline is O(program), but its
   inputs change on statics frames and outline use, not per keystroke.
   statics and test results compare by identity, the rest by value */
let memo = Util.Slot.mk();
/* the cell code's refusals of the rows: per program, not per frame */
let refusals = Util.Slot.mk();
let same = ((statics, results, rest), (statics', results', rest')) =>
  statics === statics'
  && Option.equal((===), results, results')
  && rest == rest';

/* the outline of the slide's whole program in Scratch and Documentation,
   else of the current editor's [statics] and [segment] */
let view =
    (
      ~arrows: bool,
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
  /* a menu, cursor or name being typed doesn't follow you to another
     slide */
  let slide =
    Option.map(
      ((prefix, _) as d) => Persist.content_key(prefix, slide_name(d)),
      deck,
    );
  if (slide != owner^) {
    owner := slide;
    menu := None;
    cursor := None;
    text := None;
    drawn := None;
    created := None;
  };
  /* the item text mode's caret went to, found in the editor */
  switch (OutlineFollow.wanted^, program) {
  | (Some(id), Some(p)) =>
    OutlineFollow.wanted := None;
    OutlineFollow.spot := spot(p, id);
  | _ => ()
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
  let key = (
    statics,
    results,
    (
      arrows,
      focused,
      is_deck,
      name,
      collapsed,
      menu^,
      menu_sel^,
      slide_view,
      cursor^,
      text^,
      flash^,
      created^,
    ),
  );
  Util.Slot.get(
    ~same,
    memo,
    key,
    () => {
      /* statics compacted by undo have no term until they recompute: parse
         this slide's program instead (never another document's) */
      let term =
        switch (program) {
        | Some(p) when !Workspace.named_outline(statics.term) =>
          Workspace.fresh_term(p)
        | _ => statics.term
        };
      /* each error badges the deepest row containing it; ancestor rows
         get a roll-up badge that CSS shows only while collapsed */
      let (error_items, error_subtree) = {
        let outline_ids = OutlineTree.collect(OutlineTree.of_term(term));
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
        arrows,
        stack_controls: is_deck,
        refusal: {
          let refused =
            lazy(
              Util.Slot.get(
                ~same=
                  ((p, t), (p', t')) =>
                    Option.equal((===), p, p') && t === t',
                refusals,
                (program, term),
                () =>
                  ScratchFocus.refusals(
                    ~want=id => Id.Map.mem(id, OutlineTree.row_ids(term)),
                    switch (program) {
                    | Some(Divided(d)) => Divided.document(d)
                    | _ => segment
                    },
                  ),
              )
            );
          id => Id.Map.find_opt(id, Lazy.force(refused));
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
        flash: flash^,
        pinned:
          switch (slide_view) {
          | Some(v) => List.map((p: SlideView.pin) => p.p_id, v.pins)
          | None => []
          },
        focused_entries: focused,
        error_items,
        error_subtree,
        menu: menu^,
        menu_sel: menu_sel^,
        /* live ✓/✗ for test rows, from the whole program's result */
        test_status: id =>
          Option.bind(results, (tr: Language.TestResults.t) =>
            Language.TestMap.lookup(id, tr.test_map)
            |> Option.map(Language.TestMap.joint_status)
          ),
        blank: id =>
          switch (program) {
          | Some(p) => blank_body(p, id)
          | None => false
          },
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
      let redraw = inject(Redraw);
      /* row mode's cursor for a line: its row's, or a new definition's
         anchor */
      let path_of = (line: E.line) =>
        switch (line) {
        | Row(path, key) =>
          switch (
            Option.bind(E.item_of(key), id =>
              OutlineTree.label_path(id, term)
            )
          ) {
          | Some(_) as p => p
          | None => Some(path)
          }
        | New(a, _) => OutlineTree.label_path(a, term)
        };
      /* what's typed on [t]'s line, to save */
      let save_of = (t: E.t): option(E.save) =>
        Option.map(
          s =>
            E.{
              sv_line: t.line,
              sv_text: s,
              sv_col: t.col,
            },
          t.pending,
        );
      let save = (req, after) =>
        switch (req) {
        | Some(sv) => inject(Save(sv, after))
        | None => Effect.Ignore
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
        set_cursor: c => {
          cursor := c;
          created := None;
          redraw;
        },
        /* the outline starts at the row holding the caret */
        focused:
          Effect.Many([
            Effect.of_sync_fun(
              () =>
                switch (OutlineFollow.mark^) {
                | Some(id) =>
                  cursor := OutlineTree.label_path(id, statics.term)
                | None => ()
                },
              (),
            ),
            redraw,
          ]),
        leave,
        text: {
          current: text^,
          get: () => text^,
          set: (~placed=false, t) => {
            text :=
              placed
                ? Option.map(
                    (t: E.t) =>
                      {
                        ...t,
                        seq: next_seq(),
                      },
                    t,
                  )
                : t;
            created := None;
            flash := 0;
            redraw;
          },
          start: (~pending=?, line, col) => {
            let req = Option.bind(text^, save_of);
            text := Some(E.at(line, ~col, ~pending?, ~seq=next_seq()));
            created := None;
            flash := 0;
            /* a new member's line unfolds its module, where it lands */
            let unfold =
              switch (line) {
              | New(m, First | Last) =>
                switch (OutlineTree.label_path(m, term)) {
                | Some(p) when List.mem(p, collapsed) => inject(Collapse(p))
                | _ => Effect.Ignore
                }
              | _ => Effect.Ignore
              };
            Effect.Many([save(req, Stay), unfold, redraw]);
          },
          go: (~discard=false, line, col, col_target) => {
            let req = discard ? None : Option.bind(text^, save_of);
            text :=
              Some({
                ...E.at(line, ~col, ~seq=next_seq()),
                col_target,
              });
            created := None;
            flash := 0;
            Effect.Many([save(req, Stay), redraw]);
          },
          save: after =>
            switch (text^) {
            | None => Effect.Ignore
            | Some(t) =>
              let req = save_of(t);
              switch (after) {
              | Leave =>
                text := None;
                cursor := path_of(t.line);
              | Stay
              | Next =>
                /* the save takes what's typed; a refusal brings it back */
                text :=
                  Some({
                    ...t,
                    pending: None,
                    error: None,
                  })
              };
              flash := 0;
              Effect.Many([save(req, after), redraw]);
            },
          abandon:
            Effect.Many([
              Effect.of_sync_fun(
                () => {
                  Option.iter((t: E.t) => cursor := path_of(t.line), text^);
                  text := None;
                },
                (),
              ),
              redraw,
            ]),
          op: (op, id) =>
            switch (text^) {
            | None => inject(DefOp(op, id))
            | Some(t) =>
              /* a delete drops what's typed; other ops save it first */
              let req = op == Delete ? None : save_of(t);
              text :=
                Some({
                  ...t,
                  pending: None,
                  error: None,
                });
              Effect.Many([save(req, Stay), inject(DefOp(op, id))]);
            },
          flash:
            Effect.Many([
              Effect.of_sync_fun(() => incr(flash), ()),
              redraw,
            ]),
          relocate: t => text := Some(t),
          note: i => drawn := Some(i),
          last: () => drawn^,
        },
        menu_open: (at, x, y) => {
          menu := Some((at, x, y));
          menu_sel := 0;
          redraw;
        },
        menu_close:
          Effect.Many([
            Effect.of_sync_fun(
              () => {
                menu := None;
                menu_sel := 0;
              },
              (),
            ),
            redraw,
          ]),
        menu_select: i => {
          menu_sel := i;
          redraw;
        },
        get_menu: () => menu^,
        get_menu_sel: () => menu_sel^,
        def_op: (op, id) => inject(DefOp(op, id)),
      };
      OutlineSidebar.view(~props, ~on, term);
    },
  );
};
