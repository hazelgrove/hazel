open Language;

/* Renaming a binder in place, ids kept: the tokens naming it, its uses
   and, for a module member, its `M.x` / `M.T` labels (Info.binding_site
   resolves both). Shared by the outline's rename and the assistant's
   update_pattern. */

/* every token naming [binder] but its own */
let find_references = (~info_map: Statics.Map.t, binder: Id.t): list(Id.t) =>
  Statics.Map.ids_referencing_binding(info_map, binder)
  |> Id.Set.remove(binder)
  |> Id.Set.elements;

/* a rename is checked at every key typed: a binder's references are kept
   for the statics they were found in */
let refs_memo = Util.Slot.mk();
let references = (~info_map: Statics.Map.t, binder: Id.t): list(Id.t) =>
  Util.Slot.get(
    ~same=((m, b), (m', b')) => m === m' && b == b',
    refs_memo,
    (info_map, binder),
    () => find_references(~info_map, binder),
  );

/* the `M.x` / `M.T` labels among [refs] */
let qualified = (~info_map: Statics.Map.t, refs: list(Id.t)): list(Id.t) =>
  List.filter(
    id =>
      switch (Id.Map.find_opt(id, info_map)) {
      | Some(info) => Info.member_access(~info_map, info) != None
      | None => false
      },
    refs,
  );

/* another member of [items], the module member [binder] is in, named
   [name] (a type's when [typ]): renamed to it, [binder] and that member
   would compete for `M.name` */
let member_clash =
    (~typ: bool, items: list(Mod.t), binder: Id.t, name: string): bool =>
  switch (Info.member_binder(~typ, items, name)) {
  | Some(other) => other != binder
  | None => false
  };

let map_sharing = (f: 'a => 'a, xs: list('a)): list('a) => {
  let ys = List.map(f, xs);
  List.for_all2((===), xs, ys) ? xs : ys;
};

/* each tile in [ids] now reads [name]; untouched subtrees stay the same
   values (the parse caches compare by identity) */
let rec relabel = (ids: list(Id.t), name: string, seg: Segment.t): Segment.t =>
  map_sharing(relabel_piece(ids, name), seg)
and relabel_piece = (ids: list(Id.t), name: string, p: Piece.t): Piece.t =>
  switch (p) {
  | Tile(t) =>
    let children = map_sharing(relabel(ids, name), t.children);
    switch (t.form) {
    | Tok(_) when List.mem(t.id, ids) =>
      Piece.Tile({
        ...t,
        form: Tok(name),
        children,
      })
    | _ =>
      children === t.children
        ? p
        : Piece.Tile({
            ...t,
            children,
          })
    };
  | Projector(pr) =>
    switch (relabel(ids, name, [pr.syntax])) {
    | [syntax] when syntax !== pr.syntax =>
      Piece.Projector({
        ...pr,
        syntax,
      })
    | _ => p
    }
  | p => p
  };

/* [relabel] throughout [z], the caret kept (MapPiece descends into
   tiles, not into a projector's syntax) */
let relabel_zipper = (ids: list(Id.t), name: string, z: Zipper.t): Zipper.t =>
  ids == []
    ? z
    : ZipperBase.MapPiece.go(
        fun
        | Piece.Tile({form: Tok(_), id, _} as t) when List.mem(id, ids) => [
            Piece.Tile({
              ...t,
              form: Tok(name),
            }),
          ]
        | Projector(_) as p => [relabel_piece(ids, name, p)]
        | p => [p],
        z,
      );
