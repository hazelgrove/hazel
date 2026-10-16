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
    /* a New line's definition, its keyword given and a hole for its
       name, the editor's caret in that */
    | Create(E.line, string)
    /* a key on a new definition's line, as the editor takes it there */
    | Type(E.line, E.edit)
    /* a new definition's line left: a blank name takes its definition
       with it, a name that isn't one is refused */
    | Settle(E.line, E.fresh, E.after)
    /* a new definition remade as a keyword's kind (none: a value) */
    | Rekind(E.line, string)
    /* a new definition's line moved a level in or out, from a line to
       another, the caret there already: its definition remade there, its
       name kept */
    | Shift(E.line, E.line)
    /* a new definition dropped with its line */
    | Discard(E.line, Id.t)
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

/* the editor whose caret is in new definition [d]'s name: how its actions
   go, its zipper, and [d] (none: the zipper is the name, a cell's header) */
let live_pane =
    (program: Program.t, d: Id.t)
    : option(
        (CellEditor.Update.t => Workspace.Action.t, Zipper.t, option(Id.t)),
      ) =>
  switch (program) {
  | Whole(e) =>
    Some((
      (a => Workspace.Action.CellAction(a)),
      e.editor.editor.state.zipper,
      Some(d),
    ))
  | Divided(dv) =>
    Option.map(
      ((cell: ScratchCell.t, side: ScratchCell.side)) =>
        (
          a => Workspace.Action.Stack(cell.e_id, side, a),
          ScratchCell.pane(side, cell).editor.editor.state.zipper,
          cell.e_id == d && side == Header ? None : Some(d),
        ),
      Divided.focused(dv),
    )
  };

/* new definition [d]'s name, the caret's place in it and the selection's,
   from the editor */
let live_name = (program: Program.t, d: Id.t): option(E.live) =>
  Option.bind(live_pane(program, d), ((_, z, def)) =>
    ItemEdit.name_at(~def, z)
    |> Option.map(((l_name, l_at, l_anchor)) =>
         E.{
           l_name,
           l_at,
           l_anchor,
         }
       )
  );

/* one action by the editor whose caret is in new definition [d]'s name,
   the caret kept there: None when it would leave */
let live_step =
    (
      ~settings: Settings.t,
      ~schedule_workspace,
      ~slide_key: string,
      d: Id.t,
      code: Scratchpad.code,
      a: Haz3lcore.Action.t,
    )
    : option(Updated.t(Scratchpad.code)) =>
  switch (live_pane(code.program, d)) {
  | None => None
  | Some((route, _, _)) =>
    switch (
      Workspace.update(
        ~settings,
        ~schedule_action=schedule_workspace,
        ~slide_key,
        route(MainEditor(Perform(a))),
        code,
      )
    ) {
    | u when live_name(u.model.program, d) != None => Some(u)
    | _ => None
    | exception (Haz3lcore.Action.Failure.Exception(_)) => None
    }
  };

/* [a] by that editor until the caret is at [target] in [d]'s name,
   toward [side] (a character can span units): as far as it gets */
let live_until =
    (
      ~settings: Settings.t,
      ~schedule_workspace,
      ~slide_key: string,
      d: Id.t,
      a: Haz3lcore.Action.t,
      side: Direction.t,
      target: int,
      u: Updated.t(Scratchpad.code),
    )
    : Updated.t(Scratchpad.code) => {
  let name = (code: Scratchpad.code) => live_name(code.program, d);
  let rec go = (u: Updated.t(Scratchpad.code), n) =>
    switch (name(u.model)) {
    | Some({l_at, _})
        when n > 0 && (side == Left ? l_at > target : l_at < target) =>
      switch (
        live_step(~settings, ~schedule_workspace, ~slide_key, d, u.model, a)
      ) {
      | Some(u) => go(u, n - 1)
      | None => u
      }
    | _ => u
    };
  go(
    u,
    Option.fold(
      ~none=0,
      ~some=(l: E.live) => E.units(l.l_name),
      name(u.model),
    ),
  );
};

let live_move = (side: Direction.t) =>
  Haz3lcore.Action.Move(Local(side, ByChar));

/* [edit] on new definition [d]'s name, applied as the editor applies
   typing: None when no editor has its caret in the name, or the edit
   would take the caret (or the selection) out of it, or a paste isn't a
   name's text. Words and the name's ends are reached a character at a
   time: the editor's word keys go by whole tokens */
let live_edit =
    (
      ~settings: Settings.t,
      ~schedule_workspace,
      ~slide_key: string,
      d: Id.t,
      edit: E.edit,
      code: Scratchpad.code,
    )
    : option(Updated.t(Scratchpad.code)) => {
  let name = (code: Scratchpad.code) => live_name(code.program, d);
  let step = live_step(~settings, ~schedule_workspace, ~slide_key, d);
  let until = live_until(~settings, ~schedule_workspace, ~slide_key, d);
  let move = live_move;
  let select = side => Haz3lcore.Action.Select(Resize(Local(side, ByChar)));
  let erase = side => Haz3lcore.Action.Destruct(Local(side, ByChar));
  switch (name(code)) {
  | None => None
  | Some({l_name, l_at, l_anchor}) =>
    let len = E.units(l_name);
    let sel = l_anchor != l_at;
    let inside = side => side == Direction.Left ? l_at > 0 : l_at < len;
    let quiet = code |> Updated.return_quiet;
    let target = (side, r) => E.reach_to(l_name, l_at, side, r);
    switch (edit) {
    | Insert(s) => step(code, Insert(s))
    | Paste(s) => Option.bind(E.fragment(s), s => step(code, Paste(s)))
    | Erase(side, _) when sel => step(code, erase(side))
    /* nothing there: refused */
    | Erase(side, _) when !inside(side) => None
    | Erase(side, Char) => step(code, erase(side))
    | Erase(side, r) =>
      step(
        until(select(side), side, target(side, r), quiet).model,
        erase(side),
      )
    | Step(side, Char) =>
      sel || inside(side) ? step(code, move(side)) : None
    /* a selection collapses toward [side] first, as a character step's
       does, and the reach is from there */
    | Step(side, r) =>
      Option.map(
        (u: Updated.t(Scratchpad.code)) =>
          switch (name(u.model)) {
          | Some(l) =>
            until(
              move(side),
              side,
              E.reach_to(l.l_name, l.l_at, side, r),
              u,
            )
          | None => u
          },
        sel ? step(code, move(side)) : Some(quiet),
      )
    /* at the name's end: no further */
    | Select(side, r) =>
      Some(until(select(side), side, target(side, r), quiet))
    | Cut => sel ? step(code, erase(Left)) : Some(quiet)
    /* from the name's start: a selection collapses there first */
    | SelectAll =>
      Option.map(
        u =>
          until(select(Right), Right, len, until(move(Left), Left, 0, u)),
        sel ? step(code, move(Left)) : Some(quiet),
      )
    };
  };
};

/* the program as [seg], new definition [d] in it with the editor's caret
   in its name. A stack whose cells hold none of it gives it a cell of its
   own, pinned once it's named (Settle): it has no row to pin till then */
let with_fresh =
    (~settings: Settings.t, d: Id.t, seg: Segment.t, code: Scratchpad.code)
    : Scratchpad.code => {
  let caret =
    Option.map(id => (Direction.Left, id), ItemEdit.name_slot(d, seg));
  let code = Workspace.with_segment(~settings, ~caret?, code, seg);
  switch (code.program) {
  | Divided(dv) when Divided.owner(d, dv) == None =>
    let st = Program.statics(code.program);
    switch (Divided.open_(~info_map=st.info_map, ~term=st.term, d, dv)) {
    | Some(dv) => {
        ...code,
        program:
          Divided(
            Option.fold(
              ~none=dv,
              ~some=c => Divided.place_caret(c, dv),
              caret,
            ),
          ),
      }
    | None => code
    };
  | _ => code
  };
};

/* new line [line]'s definition, a hole for its name, the editor's caret
   in it: unless the caret has moved on from the line meanwhile. One undo
   step, with the name typed after it; a tentative one, taken back if the
   name is left blank */
let create =
    (~settings: Settings.t, line: E.line, kw: string, code: Scratchpad.code)
    : Updated.t(Scratchpad.code) =>
  switch (line, text^) {
  | (New(anchor, place), Some(t))
      when E.same_line(t.line, line) && t.fresh == None =>
    let program = code.program;
    let ctx = Workspace.item_ctx(~settings, ~collapsed=[], program);
    switch (
      ItemEdit.edit(
        ctx,
        Blank(place, anchor, fst(E.new_kind(kw))),
        Program.document(program),
      )
    ) {
    | Ok((seg, Some(d))) =>
      let code = with_fresh(~settings, d, seg, code);
      text :=
        Some({
          ...t,
          fresh:
            Some({
              f_id: d,
              f_kw: kw,
            }),
          seq: next_seq(),
        });
      code |> Updated.return(~tentative=true);
    | Ok((_, None))
    | Error(_) =>
      /* refused: the caret back on its row */
      incr(flash);
      text :=
        Option.map(
          path =>
            E.at(
              Row(path, Some(Item(anchor))),
              ~col=
                Option.fold(
                  ~none=0,
                  ~some=(n: OutlineTree.node) => E.units(n.o_label),
                  OutlineTree.node_of(anchor, ctx.term),
                ),
              ~seq=next_seq(),
            ),
          OutlineTree.label_path(anchor, ctx.term),
        );
      code |> Updated.return_quiet;
    };
  | _ => code |> Updated.return_quiet
  };

/* new definition [f] remade as [kw]'s kind on line [to_]: its own line,
   or one a level in or out, where its definition then goes. A keyword
   typed before the name (or erased) remakes it blank; a moved line
   [keep_name]s, the editor's caret where it was in it. Still the step
   that made it. None: refused */
let remake =
    (
      ~settings: Settings.t,
      ~schedule_workspace,
      ~slide_key: string,
      ~keep_name: bool,
      to_: E.line,
      f: E.fresh,
      kw: string,
      code: Scratchpad.code,
    )
    : option(Updated.t(Scratchpad.code)) =>
  switch (to_) {
  | New(anchor, place) =>
    let program = code.program;
    let doc = Program.document(program);
    let ctx = Workspace.item_ctx(~settings, ~collapsed=[], program);
    let (pat, col) =
      switch (keep_name, live_name(program, f.f_id)) {
      | (true, Some({l_name, l_at, _})) when String.trim(l_name) != "" => (
          ScratchFocus.find_pat(f.f_id, doc),
          l_at,
        )
      | _ => (None, 0)
      };
    let made =
      Result.bind(
        ItemEdit.edit(ctx, Op(Delete, f.f_id), doc), ~f=((seg, _)) =>
        ItemEdit.edit(ctx, Blank(place, anchor, fst(E.new_kind(kw))), seg)
      );
    switch (made, text^) {
    | (Ok((seg, Some(d))), Some(t)) =>
      let seg =
        Option.fold(
          ~none=seg,
          ~some=p => ScratchFocus.splice_pat(d, p, seg),
          pat,
        );
      let code =
        live_until(
          ~settings,
          ~schedule_workspace,
          ~slide_key,
          d,
          live_move(Right),
          Right,
          col,
          with_fresh(~settings, d, seg, code) |> Updated.return_quiet,
        ).
          model;
      text :=
        Some({
          ...t,
          line: to_,
          fresh:
            Some({
              f_id: d,
              f_kw: kw,
            }),
          seq: next_seq(),
        });
      Some(code |> Updated.return(~historic=false));
    | _ => None
    };
  | Row(_) => None
  };

/* row [path] folded, or not, as the slide keeps it */
let set_folded =
    (~prefix: string, ~name: string, path: OutlineTree.path, folded: bool) => {
  let cur = Persist.collapse_paths(prefix, name);
  if (List.mem(path, cur) != folded) {
    let ck = Persist.content_key(prefix, name);
    let next = folded ? [path, ...cur] : List.filter(p => p != path, cur);
    next == []
      ? Hashtbl.remove(Persist.slide_collapse, ck)
      : Hashtbl.replace(Persist.slide_collapse, ck, next);
    Persist.write_collapse(prefix, name);
  };
};

/* row [id] unfolded where [term] has it: an item went into it */
let unfold = (~prefix, ~name, id: Id.t, term: Language.Exp.t) =>
  Option.iter(
    path => set_folded(~prefix, ~name, path, false),
    OutlineTree.label_path(id, term),
  );

/* new definition [d] gone with its line [line], its insertion's step
   taken back from the history: as if it had never been made. The
   editor's caret goes beside the line's anchor */
let drop =
    (~settings: Settings.t, line: E.line, d: Id.t, code: Scratchpad.code)
    : Updated.t(Scratchpad.code) => {
  let program = code.program;
  let ctx = Workspace.item_ctx(~settings, ~collapsed=[], program);
  switch (ItemEdit.edit(ctx, Op(Delete, d), Program.document(program))) {
  | Ok((seg, _)) =>
    let caret =
      switch (line) {
      | New(anchor, _) => Some((Direction.Right, anchor))
      | Row(_) => None
      };
    Workspace.with_segment(~settings, ~caret?, code, seg)
    |> Updated.return(~historic=false, ~retract=true);
  | Error(_) => code |> Updated.return_quiet
  };
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
  | Redraw => code |> Updated.return_quiet
  | Collapse(path) =>
    set_folded(
      ~prefix,
      ~name,
      path,
      !List.mem(path, Persist.collapse_paths(prefix, name)),
    );
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
      | (MoveUp | MoveDown | Indent | Outdent, Some(_)) when !typing || on_row =>
        put(target)
      | (NewTestBelow | NewTestAbove, Some(_)) => put(~fresh=0, target)
      | (Duplicate, Some(_)) when on_row => put(~fresh=col, target)
      | _ => ()
      };
      /* Tab into a folded row unfolds it, as the item lands there */
      switch (op, Option.bind(target, ItemEdit.parent_row(new_term))) {
      | (Indent, Some((parent, _))) =>
        unfold(~prefix, ~name, parent, new_term)
      | _ => ()
      };
      /* a new item opens as a cell, as new definitions do */
      switch (op, target, code.program) {
      | (MoveUp | MoveDown | Indent | Outdent, _, _) => ()
      | (_, Some(id), Whole(_)) => schedule_workspace(FocusToggle(id))
      | (_, Some(id), Divided(d)) when Divided.owner(id, d) == None =>
        schedule_workspace(FocusEnsure(id))
      | _ => ()
      };
      code |> Updated.return;
    };
  | Save(sv, after) =>
    /* nothing reaches the program until here: a rename is one
       refactoring */
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
    let txt = String.trim(sv.sv_text);
    /* the save landed: row mode's cursor on the row, if it was left */
    let landed = (code: Scratchpad.code, id: Id.t) =>
      if (after == Leave) {
        cursor :=
          OutlineTree.label_path(id, Program.statics(code.program).term);
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
        switch (
          ItemEdit.edit(ctx, Rename(row, txt), Program.document(program))
        ) {
        | Error(why) => refuse(why)
        | Ok((new_seg, _)) =>
          let code = Workspace.with_segment(~settings, code, new_seg);
          landed(code, row);
          code |> Updated.return;
        }
      }
    /* a new definition's name is the program's already (Settle) */
    | Row(_)
    | New(_) => code |> Updated.return_quiet
    };
  | Create(line, kw) => create(~settings, line, kw, code)
  | Type(line, edit) =>
    switch (text^) {
    | Some({line: on, fresh: Some(f), _}) when E.same_line(on, line) =>
      switch (
        live_edit(
          ~settings,
          ~schedule_workspace,
          ~slide_key=Persist.content_key(prefix, name),
          f.f_id,
          edit,
          code,
        )
      ) {
      /* one undo step with its definition's insertion */
      | Some(u) => {
          ...u,
          historic: false,
        }
      | None =>
        incr(flash);
        code |> Updated.return_quiet;
      }
    /* typed toward a line that never came, or has gone */
    | _ => code |> Updated.return_quiet
    }
  | Settle(line, f, after) =>
    let program = code.program;
    let name =
      Option.map(
        pat => String.trim(ItemEdit.text(pat)),
        ScratchFocus.find_pat(f.f_id, Program.document(program)),
      );
    let kind = ItemEdit.rename_kind(fst(E.new_kind(f.f_kw)));
    switch (name) {
    /* gone already (undone) */
    | None => code |> Updated.return_quiet
    | Some("") => drop(~settings, line, f.f_id, code)
    | Some(n) =>
      switch (OutlineRename.check_name(kind, n)) {
      | Some(why) =>
        text :=
          Some({
            ...E.at(line, ~fresh=f, ~seq=next_seq()),
            error: Some(why),
          });
        code |> Updated.return_quiet;
      | None =>
        /* a type or module's keyword leaves as its row comes */
        created := f.f_kw == "" ? None : Some((f.f_id, f.f_kw));
        let term = lazy(Workspace.fresh_term(program));
        /* a cell of its own stays open, pinned now it has a row */
        let code =
          switch (program) {
          | Divided(dv)
              when
                Divided.cell(f.f_id, dv) != None
                && !
                     List.exists(
                       (p: SlideView.pin) => p.p_id == f.f_id,
                       code.view.pins,
                     ) =>
            let term = Lazy.force(term);
            Workspace.realize(
              ~info_map=Workspace.fresh_info_map(~settings, program, term),
              ~term,
              SlideView.pin(~term, f.f_id, code.view),
              code,
            );
          | _ => code
          };
        switch (after, text^) {
        /* the next line, below it or first inside it, unless the keys
           moved on meanwhile */
        | (Next, Some({line: New(a, _) as next, fresh: None, _}))
            when a == f.f_id =>
          create(~settings, next, "", code)
        | _ =>
          if (after == Leave) {
            cursor := OutlineTree.label_path(f.f_id, Lazy.force(term));
          };
          /* its row shows the name now, not when the statics catch up */
          switch (OutlineTree.node_of(f.f_id, Program.statics(program).term)) {
          | Some(n') when n'.o_label == n => code |> Updated.return_quiet
          | _ =>
            CodeWithStatics.StaticsDebounce.force_on_next := true;
            code |> Updated.return_quiet(~recalculate=true);
          };
        };
      }
    };
  | Rekind(line, kw) =>
    switch (text^) {
    | Some({line: on, fresh: Some(f), _})
        when E.same_line(on, line) && f.f_kw != kw =>
      /* the name still the keyword typed, or blank to go back */
      switch (live_name(code.program, f.f_id)) {
      | Some({l_name, _}) when kw == "" ? l_name == "" : l_name ++ " " == kw =>
        switch (
          remake(
            ~settings,
            ~schedule_workspace,
            ~slide_key=Persist.content_key(prefix, name),
            ~keep_name=false,
            line,
            f,
            kw,
            code,
          )
        ) {
        | Some(u) => u
        | None =>
          incr(flash);
          code |> Updated.return_quiet;
        }
      | _ => code |> Updated.return_quiet
      }
    | _ => code |> Updated.return_quiet
    }
  | Shift(from, to_) =>
    switch (text^) {
    /* the caret went there with the key */
    | Some({line: on, fresh: Some(f), _} as t) when E.same_line(on, to_) =>
      switch (
        remake(
          ~settings,
          ~schedule_workspace,
          ~slide_key=Persist.content_key(prefix, name),
          ~keep_name=true,
          to_,
          f,
          f.f_kw,
          code,
        )
      ) {
      | Some(u) =>
        /* into a folded row: unfolded, for the definition it then shows */
        switch (to_) {
        | New(m, Last) =>
          unfold(~prefix, ~name, m, Workspace.fresh_term(u.model.program))
        | _ => ()
        };
        u;
      /* refused: the caret back where its definition still is */
      | None =>
        text :=
          Some({
            ...t,
            line: from,
          });
        incr(flash);
        code |> Updated.return_quiet;
      }
    | _ => code |> Updated.return_quiet
    }
  | Discard(line, d) => drop(~settings, line, d, code)
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
  /* a new definition's name, as the editor has it: it changes as the
     program does, not the statics */
  let live =
    switch (text^, program) {
    | (Some({fresh: Some(f), _}), Some(p)) => live_name(p, f.f_id)
    | _ => None
    };
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
      live,
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
        live,
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
      /* what saves [t]'s line as the caret leaves it ([after]): the name
         typed there, or a new definition's check */
      let save = (t: E.t, after: E.after) =>
        switch (t.fresh, t.pending) {
        | (Some(f), _) => inject(Settle(t.line, f, after))
        | (None, Some(s)) =>
          inject(
            Save(
              {
                sv_line: t.line,
                sv_text: s,
                sv_col: t.col,
              },
              after,
            ),
          )
        | (None, None) => Effect.Ignore
        };
      let save_current = (after: E.after) =>
        Option.fold(~none=Effect.Ignore, ~some=t => save(t, after), text^);
      /* a New line's definition, made as the line starts */
      let create = (line: E.line, kw: string) =>
        switch (line) {
        | New(_) => inject(Create(line, kw))
        | Row(_) => Effect.Ignore
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
          start: (~kw="", line, col) => {
            let req = save_current(Stay);
            text := Some(E.at(line, ~col, ~seq=next_seq()));
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
            Effect.Many([req, unfold, create(line, kw), redraw]);
          },
          go: (~discard=false, line, col, col_target) => {
            let req = discard ? Effect.Ignore : save_current(Stay);
            text :=
              Some({
                ...E.at(line, ~col, ~seq=next_seq()),
                col_target,
              });
            created := None;
            flash := 0;
            Effect.Many([req, create(line, ""), redraw]);
          },
          save: after =>
            switch (text^) {
            | None => Effect.Ignore
            | Some(t) =>
              let req = save(t, after);
              switch (after, t.fresh) {
              | (Leave, _) =>
                text := None;
                cursor := path_of(t.line);
              /* the next line waits for its definition */
              | (Next, Some(f)) =>
                text := Some(E.at(E.next_line(f), ~seq=next_seq()))
              | (Stay | Next, _) =>
                /* the save takes what's typed; a refusal brings it back */
                text :=
                  Some({
                    ...t,
                    pending: None,
                    error: None,
                  })
              };
              flash := 0;
              Effect.Many([req, redraw]);
            },
          abandon: () => {
            let drop =
              switch (text^) {
              | Some({fresh: Some(f), line, _}) =>
                inject(Discard(line, f.f_id))
              | _ => Effect.Ignore
              };
            Option.iter((t: E.t) => cursor := path_of(t.line), text^);
            text := None;
            Effect.Many([drop, redraw]);
          },
          discard: d =>
            switch (text^) {
            | Some(t) => inject(Discard(t.line, d))
            | None => Effect.Ignore
            },
          type_: edit =>
            switch (text^) {
            | Some(t) => inject(Type(t.line, edit))
            | None => Effect.Ignore
            },
          /* read as the editor's ⌘V reads it. The read is async: the text
             is for the line it was pasted on, if the caret is still there */
          paste: () =>
            switch (text^) {
            | Some(t) =>
              Effect.bind(JsUtil.read_clipboard(), ~f=s =>
                inject(Type(t.line, Paste(s)))
              )
            | None => Effect.Ignore
            },
          rekind: kw =>
            switch (text^) {
            | Some(t) => inject(Rekind(t.line, kw))
            | None => Effect.Ignore
            },
          /* the caret on its line there at once, for the keys after */
          shift: to_ =>
            switch (text^) {
            | Some(t) =>
              text :=
                Some({
                  ...t,
                  line: to_,
                });
              Effect.Many([inject(Shift(t.line, to_)), redraw]);
            | None => Effect.Ignore
            },
          op: (op, id) =>
            switch (text^) {
            | None => inject(DefOp(op, id))
            | Some(t) =>
              /* a delete drops what's typed; other ops save it first */
              let req = op == Delete ? Effect.Ignore : save(t, Stay);
              text :=
                Some({
                  ...t,
                  pending: None,
                  error: None,
                });
              Effect.Many([req, inject(DefOp(op, id))]);
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
