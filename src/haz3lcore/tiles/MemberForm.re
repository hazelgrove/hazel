open Util;

/* An expression-rooted program as module members (plans/world-tree.md C2),
   so a slide can be a module's body: a top-level definition `let p = d in`
   (also `type` and `module`) becomes the member `let p = d;`, a statement's
   `;` separates members, and the last expression stays last. Sequential
   members scope as the `in` chain did. Pieces keep their ids, so probes,
   pins and folds carry over. Items are the span tree's (ItemSpans), so a
   `;` or `in` inside an operand is left alone. */

/* the member form of an expression-level definition */
let member_family = (fam: Form.family): option(Form.family) =>
  switch (fam) {
  | Let => Some(ModLet)
  | TypeAlias => Some(ModType)
  | ModuleExp => Some(ModuleMod)
  | _ => None
  };

/* trailing spaces before a member's `;` (a definition's `= d in` keeps its
   space before `in`) */
let rec trim_end = (seg: Segment.t): Segment.t =>
  switch (ListUtil.split_last_opt(seg)) {
  | Some((init, Piece.Secondary({content: Whitespace(" "), _}))) =>
    trim_end(init)
  | _ => seg
  };

/* the member `;` ending the definition headed by [head] */
let member_semi = (head: Id.t): Piece.t =>
  Piece.Tile({
    id: Id.derive(~salt="c2-member-semi", head),
    form: Form.Compound(CellJoin),
    sort: Mod,
    shards: [0],
    children: [],
  });

/* the program [seg] (at an expression root) as module members, or None
   when an item has no member form: an unfinished definition, a theorem,
   a directive (`use`, a filter) */
let of_exp_root = (seg: Segment.t): option(Segment.t) => {
  let lv = ItemSpans.top(~root=Exp, seg);
  let n = Array.length(lv.pieces);
  /* the piece index of each span's head tile, and of its `;` */
  let heads = Hashtbl.create(16);
  let seps = Hashtbl.create(16);
  let ok =
    Array.for_all(
      (s: ItemSpans.span) =>
        switch (s.kind) {
        | Tail => true
        | Use => false
        | Stmt =>
          Option.iter(i => Hashtbl.replace(seps, i, ()), s.sep);
          true;
        | Def =>
          let rec find = i =>
            i >= s.hi
              ? None
              : Piece.id(lv.pieces[i]) == s.anchor ? Some(i) : find(i + 1);
          switch (Option.map(i => (i, lv.pieces[i]), find(s.lo))) {
          | Some((
              i,
              Piece.Tile({form: Compound(fam), shards: [0, 1, 2], _}),
            ))
              when member_family(fam) != None =>
            Hashtbl.replace(heads, i, ());
            true;
          | _ => false
          };
        },
      lv.spans,
    );
  ok
    ? Some(
        List.concat(
          List.init(n, i =>
            switch (lv.pieces[i]) {
            | Piece.Tile({form: Compound(fam), children: [p, d], _} as t)
                when Hashtbl.mem(heads, i) =>
              [
                Piece.Tile({
                  ...t,
                  form: Compound(Option.get(member_family(fam))),
                  sort: Mod,
                  shards: [0, 1],
                  children: [p],
                }),
                ...trim_end(d),
              ]
              @ [member_semi(t.id)]
            | Piece.Tile(t) when Hashtbl.mem(seps, i) => [
                Piece.Tile({
                  ...t,
                  sort: Mod,
                }),
              ]
            | p => [p]
            }
          ),
        ),
      )
    : None;
};
