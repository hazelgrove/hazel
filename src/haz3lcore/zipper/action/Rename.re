open Language;

/* Renaming a binder in place, ids kept: the tokens naming it, its uses
   and, for a module member, its `M.x` / `M.T` labels (Info.binding_site
   resolves both). Shared by the outline's rename and the assistant's
   rename. */

/* what a new name binds: values take a lowercase identifier, modules a
   capitalized one, types either */
type name_kind =
  | Value
  | Type
  | Module;

/* why [name] can't name a [kind]: it must be one plain identifier, not a
   keyword, `$`-prefixed (a quote) or qualified (a use). Short reasons:
   the outline is narrow */
let check_name = (kind: name_kind, name: string): option(string) => {
  let spaced = String.exists(c => String.contains(" \t\n\r", c), name);
  let plain =
    name != ""
    && name.[0] != '$'
    && !String.contains(name, '.')
    && !List.mem(name, FormId.delims);
  let (lower, upper) = (Token.is_var(name), Token.is_ctr(name));
  switch (kind) {
  | _ when spaced => Some("names have no spaces")
  | _ when Token.is_keyword(name) => Some(name ++ " is a keyword")
  | _ when !plain || !lower && !upper => Some("a name is one identifier")
  | Value when !lower => Some("value names start lowercase")
  | Module when !upper => Some("module names start with a capital")
  | _ => None
  };
};

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

/* each token [name_of] names now reads that name, the walk only into the
   pieces [within]; untouched subtrees stay the same values (the parse
   caches compare by identity) */
let rec relabel_by =
        (~within, name_of: Id.t => option(string), seg: Segment.t): Segment.t =>
  map_sharing(relabel_piece_by(~within, name_of), seg)
and relabel_piece_by = (~within, name_of, p: Piece.t): Piece.t =>
  switch (p) {
  | Tile(t) when within(t.id) =>
    let children = map_sharing(relabel_by(~within, name_of), t.children);
    switch (t.form, name_of(t.id)) {
    | (Tok(_), Some(name)) =>
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
  | Projector(pr) when within(pr.id) =>
    switch (relabel_by(~within, name_of, [pr.syntax])) {
    | [syntax] when syntax !== pr.syntax =>
      Piece.Projector({
        ...pr,
        syntax,
      })
    | _ => p
    }
  | p => p
  };
