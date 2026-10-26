/* The outline's edits: its row ops and typed names as transactions of the
   editing kernel (EntryEdit), and the planners that pick them: which way
   Alt↑↓ and Tab go, and a rename's tokens */
open Haz3lcore;
open Util;

module K = EntryEdit;
module L = EntryLayout;
module Nav = SpineNav;

let root_of = L.root_of;

/* a kind's keyword */
let kind_kw = (kind: OutlineRename.kind): string =>
  switch (kind) {
  | KType => "type"
  | KModule => "module"
  | KValue => "let"
  };

/* a new definition of [kind], a hole for its name */
let blank = (kind: OutlineRename.kind): K.template =>
  switch (kind) {
  | KType => Type(None)
  | KModule => Module(None)
  | KValue => Value(None)
  };

/* the program and the row an edit lands on, or why not */
type landed = result((Segment.t, option(Id.t)), K.reason);

let run = (~mod_root: bool, seg: Segment.t, ops: list(K.op)): landed =>
  K.transaction(~root=root_of(mod_root), seg, ops)
  |> Result.map(~f=(c: K.change) => (c.program, c.focus))
  |> Result.map_error(~f=(r: K.refusal) => r.reason);

/* the first that lands */
let rec first_of = (tries: list(unit => landed)): landed =>
  switch (tries) {
  | [] => Error(K.NoPlace)
  | [t] => t()
  | [t, ...rest] =>
    switch (t()) {
    | Ok(_) as r => r
    | Error(_) => first_of(rest)
    }
  };

/* the new definition's template for op [op] */
let new_template = (op: OutlineEdit.def_op): option(K.template) =>
  switch (op) {
  | NewBelow => Some(Value(Some("new_def")))
  | NewTypeBelow => Some(Type(Some("NewType")))
  | NewModuleBelow => Some(Module(Some("NewModule")))
  | NewTestBelow
  | NewTestAbove => Some(Test)
  | Duplicate
  | MoveUp
  | MoveDown
  | Indent
  | Outdent
  | Delete => None
  };

/* beside entry [j] of block [b], row [fid]: below it, or above it, as
   above a block's result, which stays last */
let beside = (~above, b: L.block, j: int, fid: Id.t): K.place =>
  above || !L.movable(b, j) && !b.in_module ? Before(fid) : After(fid);

/* [op] on row [fid]'s entry, in its own block: a move swaps it with the
   entry beside it, never leaves the block. New entries go below, or above
   a block's result */
let plan =
    (~mod_root, op: OutlineEdit.def_op, fid: Id.t, seg)
    : result(list(K.op), K.reason) =>
  switch (L.locate(~mod_root, fid, seg)) {
  | None => Error(Missing)
  | Some((b, j, _)) =>
    let n = Array.length(b.sl);
    switch (op) {
    | Delete => Ok([Delete(fid)])
    | MoveUp when j > 0 => Ok([Move(fid, Before(K.anchor(b, j - 1)))])
    | MoveDown when j + 1 < n => Ok([Move(fid, After(K.anchor(b, j + 1)))])
    | MoveUp
    | MoveDown => Error(Edge)
    | Indent
    | Outdent => Error(NoPlace)
    | Duplicate => Ok([Insert(After(fid), Copy(fid))])
    | _ =>
      switch (new_template(op)) {
      | Some(t) =>
        Ok([
          Insert(beside(~above=op == NewTestAbove, b, j, fid), Template(t)),
        ])
      | None => Error(NoPlace)
      }
    };
  };

/* where the item holding [fid] sits, seen from its own block */
type spot = {
  /* the item is a 2-shard member */
  s_member: bool,
  s_item: Segment.t,
  s_trail: Segment.t,
  /* the neighbour in the move's direction, if it defines a module, or a
     value */
  s_module: option(Id.t),
  s_value: option(Id.t),
  s_edge: bool,
};

let spot = (~mod_root, ~up: bool, fid: Id.t, seg: Segment.t): option(spot) =>
  Option.map(
    ((b: L.block, j, _)) => {
      let k = up ? j - 1 : j + 1;
      {
        s_member: b.in_module,
        s_item: L.bare(b, j),
        s_trail: L.trail(b, j),
        s_module: L.movable(b, k) ? L.module_at(b, k) : None,
        s_value: L.movable(b, k) ? L.value_at(b, k) : None,
        s_edge: !L.movable(b, k),
      };
    },
    L.locate(~mod_root, fid, seg),
  );

/* no comment set apart lies between row [fid]'s item and the item above
   ([up]) or below: it can go into that item and come back */
let near = (~mod_root, ~up: bool, fid: Id.t, seg: Segment.t): bool =>
  switch (L.locate(~mod_root, fid, seg)) {
  | Some((b, j, _)) =>
    up
      ? L.under_item(b, j)
      : j + 1 < Array.length(b.sl) && L.under_item(b, j + 1)
  | None => false
  };

/* Alt↑↓: into an expanded module beside the item, bound by `module` or
   `let` (its end going up, its start going down); from a module's first
   or last member, out to just above or below it. Modules not [is_open]
   are stepped over, as is any module by an item with no member form (a
   `use`) or across a comment set apart, and function bodies keep their
   items. [owner]: the item's module */
let move_why =
    (
      ~mod_root: bool,
      ~is_open: Id.t => bool,
      ~owner: option(Id.t),
      ~up: bool,
      fid: Id.t,
      seg: Segment.t,
    )
    : landed =>
  switch (spot(~mod_root, ~up, fid, seg)) {
  | None => Error(Missing)
  | Some(s) =>
    let step = () =>
      Result.bind(
        plan(~mod_root, up ? MoveUp : MoveDown, fid, seg),
        ~f=run(~mod_root, seg),
      );
    switch (s.s_module) {
    | Some(m) when is_open(m) =>
      first_of([
        () =>
          near(~mod_root, ~up, fid, seg)
            ? run(
                ~mod_root,
                seg,
                [
                  Move(fid, up ? LastIn(Members(m)) : FirstIn(Members(m))),
                ],
              )
            : Error(Apart),
        step,
      ])
    | _ when !s.s_edge => step()
    | _ =>
      switch (owner) {
      | Some(m) =>
        run(~mod_root, seg, [Move(fid, up ? Before(m) : After(m))])
      | None => Error(Edge)
      }
    };
  };

/* Tab: the item holding [fid] into the item just above it, in place, a
   level deeper: a module's last member, or a value's last local
   definition. Only into a row the outline [shown]s, with no comment set
   apart between them */
let indent =
    (~mod_root: bool, ~shown: Id.t => bool, fid: Id.t, seg: Segment.t): landed => {
  let into = (block: K.block) =>
    near(~mod_root, ~up=true, fid, seg)
      ? run(~mod_root, seg, [Move(fid, LastIn(block))]) : Error(K.Apart);
  switch (spot(~mod_root, ~up=true, fid, seg)) {
  | Some({s_module: Some(m), _}) when shown(m) => into(Members(m))
  | Some({s_value: Some(d), _}) when shown(d) => into(Body(d))
  | Some(_) => Error(Edge)
  | None => Error(Missing)
  };
};

/* ⇧Tab: the item holding [fid], the last of [owner]'s members or local
   definitions, out to just after [owner], in place, a level out */
let outdent =
    (~mod_root: bool, ~owner: Id.t, fid: Id.t, seg: Segment.t): landed =>
  switch (
    spot(~mod_root, ~up=false, fid, seg),
    Nav.find_def(~root=root_of(mod_root), owner, seg),
  ) {
  | (Some({s_edge: true, _}), Some(def)) when Nav.seg_contains_id(fid, def) =>
    run(~mod_root, seg, [Move(fid, After(owner))])
  | (Some(_), _) => Error(Edge)
  | (None, _) => Error(Missing)
  };

/* how the outline edits a program's items, addressed by id */
type t =
  | Op(OutlineEdit.def_op, Id.t)
  /* a new definition with a hole for its name, below the row or first or
     last inside it, its name to be typed in the editor */
  | Blank(OutlineEdit.place, Id.t, OutlineTree.kind);

let rename_kind = (kind: OutlineTree.kind): OutlineRename.kind =>
  switch (kind) {
  | KType => KType
  | KModule => KModule
  | _ => KValue
  };

/* where item [id]'s name is typed: its pattern's first piece */
let name_slot = (~mod_root: bool, id: Id.t, seg: Segment.t): option(Id.t) =>
  Option.bind(Nav.find_pat(~root=root_of(mod_root), id, seg), pat =>
    List.find_opt(p => !Piece.is_secondary(p), pat) |> Option.map(Piece.id)
  );

/* a name as the editor holds it: the text of item [def]'s pattern (or,
   with no [def], of the zipper's whole segment, a cell's header), the
   caret's offset in it and the selection's other end (the caret's offset
   when nothing is selected), in JS string units: when the caret and the
   selection are in the name */
let name_at = (~def: option(Id.t), z: Zipper.t): option((string, int, int)) => {
  let (pre, suf) = z.relatives.siblings;
  /* the caret's text before and after it, from the innermost level out
     to the name's level: None when the caret isn't in the name */
  let rec out = (before, after, ancestors: Ancestors.t) =>
    switch (ancestors, def) {
    | ([], None) => Some((before, after))
    | ([], Some(_)) => None
    | ([(a, (gpre, gsuf)), ...rest], _) =>
      switch (def) {
      | Some(d) when a.id == d =>
        fst(a.children) == [] ? Some((before, after)) : None
      | _ =>
        let (l, r) = Ancestor.disassemble(a);
        out(
          L.text(gpre) ++ L.text(l) ++ before,
          after ++ L.text(r) ++ L.text(gsuf),
          rest,
        );
      }
    };
  let selecting = z.selection.content != [];
  let selected = L.text(z.selection.content);
  /* an inner caret splits the token to its right; a selection's is in the
     selection's own token, which sits between the siblings */
  let (before, after) =
    switch (z.caret, suf) {
    | (Inner(n), [p, ...rest]) when !selecting =>
      let t = L.piece_text(p);
      (
        L.text(pre) ++ OutlineEdit.slice(t, 0, n + 1),
        OutlineEdit.slice(t, n + 1, OutlineEdit.units(t)) ++ L.text(rest),
      );
    | _ => (L.text(pre), L.text(suf))
    };
  /* what's selected of it: a selection's end can be inside a token */
  let (skipped, picked) =
    !selecting
      ? (0, "")
      : (
        OutlineEdit.units(
          fst(
            Unicode.split_nth(
              selected,
              min(
                fst(Zipper.selection_trim_offsets(z)),
                Unicode.length(selected),
              ),
            ),
          ),
        ),
        Zipper.trim_selected_text(z, selected),
      );
  Option.bind(
    out(before, after, z.relatives.ancestors),
    ((before, after)) => {
      let all = before ++ selected ++ after;
      let rec lead = i =>
        i < String.length(all) && String.contains(" \t\n\r", all.[i])
          ? lead(i + 1) : i;
      let name = String.trim(all);
      let from =
        OutlineEdit.units(before)
        + skipped
        - OutlineEdit.units(String.sub(all, 0, lead(0)));
      let clamp = i => max(0, min(OutlineEdit.units(name), i));
      let (lo, hi) = (
        clamp(from),
        clamp(from + OutlineEdit.units(picked)),
      );
      /* a selection of the space around the name isn't in it */
      OutlineEdit.slice(name, lo, hi) != picked
        ? None
        : z.selection.focus == Left
            ? Some((name, lo, hi)) : Some((name, hi, lo));
    },
  );
};

/* whether item [it] (row [row]) can be a [kind]'s, keeping its definition
   (else it's remade blank): a definition of only holes can be any kind's,
   a `{…}` a value's or a module's. Only a plain name changes kind, or one
   under an annotation, which stays with its definition */
let refit =
    (
      ~mod_root: bool,
      it: OutlineRename.item,
      kind: OutlineRename.kind,
      row: Id.t,
      seg,
    )
    : result(bool, string) =>
  switch (Nav.tile_ctx(~root=root_of(mod_root), row, seg)) {
  | None => Error("this row has no name to change")
  | Some((_, _, t) as at) =>
    let def = Option.value(Nav.def_at(at), ~default=[]);
    let named =
      switch (List.filter(p => !Piece.is_secondary(p), Nav.pat_of(t))) {
      | [Tile(b)] => b.id == it.binder
      | [Tile(b), Tile(colon), ..._] =>
        b.id == it.binder && Tile.label(colon) == [":"]
      | _ => false
      };
    switch (it.kind, kind) {
    | _ when !named => Error("only a plain name changes kind")
    | _ when L.blank_def(def) => Ok(false)
    | (KValue, KModule)
    | (KModule, KValue) when L.module_def(def) => Ok(true)
    | (_, KModule) => Error({js|only a {…} body can be a module|js})
    | (_, KType) => Error("a type needs a type body")
    | (KType, KValue) => Error("a value needs a value body")
    | (_, KValue) => Error({js|only a {…} module can be a value|js})
    };
  };

/* a rename typed on an item's line, checked. A kind keyword typed first
   changes its kind (its own kind's is dropped): the new kind, and whether
   its definition stays */
type renaming = {
  rn_item: OutlineRename.item,
  rn_name: string,
  rn_kind: option((OutlineRename.kind, bool)),
  rn_refs: list(Id.t),
};

/* renaming row [row] as [typed]: nothing to do, or why not */
let plan_rename =
    (
      ~mod_root: bool,
      ~info_map: Lazy.t(Language.Statics.Map.t),
      ~outline: OutlineTree.t,
      ~seg: Lazy.t(Segment.t),
      row: Id.t,
      typed: string,
    )
    : result(option(renaming), string) => {
  let (kw, rest) = OutlineEdit.split_typed(typed);
  let name = String.trim(rest);
  switch (OutlineRename.find_item(row, outline)) {
  | None => Error("this row has no name to change")
  | Some(it) =>
    let kind =
      kw == "" ? it.kind : rename_kind(fst(OutlineEdit.new_kind(kw)));
    let change =
      kind == it.kind
        ? Ok(None)
        : Result.map(
            ~f=keep => Some((kind, keep)),
            refit(~mod_root, it, kind, row, Lazy.force(seg)),
          );
    Result.bind(change, ~f=change =>
      OutlineRename.prepare(~info_map, ~kind, it, name)
      |> Result.map(~f=refs =>
           Option.map(
             rn_refs =>
               {
                 rn_item: it,
                 rn_name: name,
                 rn_kind: change,
                 rn_refs,
               },
             refs,
           )
         )
    );
  };
};

/* the program renamed as planned, at row [row]: the binder and its
   references relabeled, ids kept; with a kind change, the head re-read as
   the new kind's, its definition kept or made blank */
let apply_rename =
    (~mod_root: bool, rn: renaming, row: Id.t, seg: Segment.t)
    : result(Segment.t, K.reason) => {
  let root = root_of(mod_root);
  let token = id => K.Replace(id, Token(rn.rn_name));
  let tokens =
    List.map(
      token,
      List.filter(
        id => K.is_token(~root, id, seg),
        [rn.rn_item.binder, ...rn.rn_refs],
      ),
    );
  let rekind =
    switch (rn.rn_kind) {
    | None => []
    /* the kernel remakes a blank definition as the kind's blank, as
       [refit] expects */
    | Some((kind, _)) => [K.Replace(row, Token(kind_kw(kind)))]
    };
  run(~mod_root, seg, tokens @ rekind) |> Result.map(~f=fst);
};

type ctx = {
  mod_root: bool,
  outline: OutlineTree.t,
  info_map: Lazy.t(Language.Statics.Map.t),
  /* whether a module row is expanded: moves step over collapsed ones,
     and over modules the outline doesn't show (`let _ = {…}`) */
  is_open: Id.t => bool,
};

/* row [fid]'s parent row in [term]'s outline, and its kind */
let parent_row =
    (outline: OutlineTree.t, fid: Id.t): option((Id.t, OutlineTree.kind)) =>
  switch (Option.map(List.rev, OutlineTree.trail_of(fid, outline))) {
  | Some([_, parent, ..._]) =>
    Option.map(k => (parent, k), OutlineTree.kind_of(parent, outline))
  | _ => None
  };

/* where a new definition [place]d by row [at] goes: below it, or first
   or last inside it, a module or (last) a value */
let new_place =
    (~mod_root: bool, place: OutlineEdit.place, at: Id.t, seg: Segment.t)
    : result(K.place, K.reason) => {
  let value = L.is_value(~mod_root, at, seg);
  switch (place) {
  | Below =>
    switch (L.locate(~mod_root, at, seg)) {
    | Some((b, j, _)) => Ok(beside(~above=false, b, j, at))
    | None => Error(Missing)
    }
  | First when value => Error(NoPlace)
  | First => Ok(FirstIn(Members(at)))
  | Last => Ok(LastIn(value ? Body(at) : Members(at)))
  };
};

/* new definition [kind], its name a hole, [place]d by row [at] */
let new_def =
    (~mod_root, place, at, kind: OutlineRename.kind, seg: Segment.t): landed =>
  Result.bind(new_place(~mod_root, place, at, seg), ~f=p =>
    run(~mod_root, seg, [Insert(p, Template(blank(kind)))])
  );

/* new definition [fid] remade where row [at] [place]s it, in one step:
   [keep], moved there as it is; else a [kind]'s blank in its place */
let remake =
    (
      ~mod_root,
      ~keep: bool,
      place,
      at,
      kind: OutlineRename.kind,
      fid: Id.t,
      seg: Segment.t,
    )
    : landed =>
  Result.bind(new_place(~mod_root, place, at, seg), ~f=p =>
    run(
      ~mod_root,
      seg,
      keep
        ? [Move(fid, p)]
        : [Delete(fid), Insert(p, Template(blank(kind)))],
    )
  );

/* the new program and the row the edit lands on, or why not */
let edit =
    (ctx: ctx, e: t, seg: Segment.t)
    : result((Segment.t, option(Id.t)), string) => {
  let mod_root = ctx.mod_root;
  let why = (r: landed) => Result.map_error(r, ~f=K.describe);
  switch (e) {
  | Op((MoveUp | MoveDown) as op, fid) =>
    let owner =
      switch (parent_row(ctx.outline, fid)) {
      | Some((parent, KModule)) => Some(parent)
      | _ => None
      };
    move_why(
      ~mod_root,
      ~is_open=ctx.is_open,
      ~owner,
      ~up=op == MoveUp,
      fid,
      seg,
    )
    |> why;
  | Op(Indent, fid) =>
    indent(
      ~mod_root,
      ~shown=id => OutlineTree.node_of(id, ctx.outline) != None,
      fid,
      seg,
    )
    |> why
  | Op(Outdent, fid) =>
    (
      switch (parent_row(ctx.outline, fid)) {
      | Some((owner, KModule | KFn | KConst)) =>
        outdent(~mod_root, ~owner, fid, seg)
      | _ => Error(K.Edge)
      }
    )
    |> why
  | Op(op, fid) =>
    Result.bind(plan(~mod_root, op, fid, seg), ~f=run(~mod_root, seg))
    |> why
  | Blank(place, at, kind) =>
    new_def(~mod_root, place, at, rename_kind(kind), seg) |> why
  };
};
