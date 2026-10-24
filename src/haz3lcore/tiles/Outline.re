open Util;

/* A program's outline: its entries as ItemSpans' levels hold them, each
   with its kind, names and canonical path (EntryPath), down every block.
   The one tree the outline sidebar and the agent's view read
   (plans/editing-in-the-large.html). Pure: pieces only; what statics and
   the run add (errors, test results, uses) is [facts], below. */

module S = ItemSpans;
module P = EntryPath;

type entry = {
  /* the entry's handle as terms have it: ItemSpans' anchor, but for an
     expression entry (a tail, a module's statement) its root tile,
     through parentheses and projectors */
  id: Id.t,
  /* ItemSpans' join key */
  anchor: Id.t,
  kind: P.kind,
  /* a let whose definition is a function: `let f(x) = …`, `= fun x -> …` */
  fn: bool,
  /* a definition that is a module literal: its children are members */
  literal: bool,
  /* the names it binds, each with its token */
  names: list((string, Id.t)),
  hint: option(string),
  /* a test entry's test tile */
  test: option(Id.t),
  path: P.t,
  /* where it is: span [k] of [lv] */
  lv: S.level,
  k: int,
  children: list(entry),
};

type t = {
  root: Sort.t,
  seg: Segment.t,
  entries: list(entry),
};

let content = P.content;

/* an expression's root tile: through parentheses and projectors */
let rec root_of = (seg: Segment.t): option(Id.t) =>
  switch (content(seg)) {
  | [] => None
  | [Tile({children: [inner], _} as t)]
      when Tile.label(t) == ["(", ")"] && content(inner) != [] =>
    root_of(inner)
  | [Projector(pr)] => root_of([pr.syntax])
  | _ =>
    try(Some(Segment.root_id(Segment.skel(seg), seg))) {
    | _ => None
    }
  };

let head_label = (p: Piece.t): option(Label.t) =>
  switch (p) {
  | Tile(t) => Some(Tile.label(t))
  | _ => None
  };

/* a definition that is a function, through parentheses and projectors:
   `fun x -> …`, or one type abstraction over one */
let rec is_fun = (def: Segment.t): bool => {
  let fun_ = Some(["fun", "->"]);
  let tuple =
    List.exists(
      fun
      | Piece.Tile(t) => Tile.is_comma(t)
      | _ => false,
      def,
    );
  switch (content(def)) {
  | _ when tuple => false
  | [p, ..._] when head_label(p) == fun_ => true
  | [p, q, ..._] when head_label(p) == Some(["typfun", "->"]) =>
    head_label(q) == fun_
  | [Tile({children: [inner], _} as t)] when Tile.label(t) == ["(", ")"] =>
    is_fun(inner)
  | [Projector(pr)] => is_fun([pr.syntax])
  | _ => false
  };
};

/* `f(x)` or `f(x): T` */
let sugared = (pat: Segment.t): bool =>
  switch (content(pat)) {
  | [Tile(f), Tile(ap), ..._] =>
    Option.map(Token.is_var, Tile.single_token(f)) == Some(true)
    && (
      ap.form == Form.Compound(Ap)
      || List.mem(Tile.label(ap), [["(", ")"], ["()"]])
    )
  | _ => false
  };

/* a definition entry's pattern and definition */
let parts = (lv: S.level, k: int): option((Segment.t, Segment.t)) =>
  switch (S.site_of(lv, k)) {
  | Some(Kid(_, t)) =>
    switch (t.children) {
    | [pat, ..._] as kids => Some((pat, ListUtil.last(kids)))
    | [] => None
    }
  | Some(Run(h, a, b)) =>
    switch (lv.pieces[h]) {
    | Tile({children: [pat, ..._], _}) =>
      Some((pat, Array.to_list(Array.sub(lv.pieces, a, b - a))))
    | _ => None
    }
  | None => None
  };

/* span [k]'s head tile: a definition's or directive's */
let head = (lv: S.level, k: int): option(Tile.t) =>
  Option.map(snd, S.head(lv, k));

let core = S.core;

let test_tile = (lv: S.level, k: int): option(Id.t) => {
  let (c0, c1) = lv.spans[k].core;
  Option.map(
    i => Piece.id(lv.pieces[i]),
    S.only_content(lv.pieces, c0, c1),
  );
};

/* span [k]'s entry, as its level alone says */
let entry_at =
    (lv: S.level, k: int, ~path: P.t, ~children: list(entry)): entry => {
  let sp = lv.spans[k];
  let e = P.entry(lv, k);
  let parts = e.kind == Def ? parts(lv, k) : None;
  let id =
    switch (e.kind) {
    | Tail => root_of(core(lv, k))
    | Stmt
    | Test when lv.sort == Sort.Mod => root_of(core(lv, k))
    | _ => None
    };
  {
    id: Option.value(id, ~default=sp.anchor),
    anchor: sp.anchor,
    kind: e.kind,
    fn:
      switch (parts) {
      | Some((pat, def)) => sugared(pat) || is_fun(def)
      | None => false
      },
    literal: sp.body != None,
    names: e.binders,
    hint: e.hint,
    test: e.kind == Test ? test_tile(lv, k) : None,
    path,
    lv,
    k,
    children,
  };
};

/* each level's entries as last built, kept while ItemSpans keeps the
   level: an entry whose path and children are the same is the same
   object, so readers memoized on entries (the sidebar's rows) hit. Below
   an entry named by its level alone at every step ([own]), now and when
   it was built, nothing outside its pieces bears on the paths: with the
   same path it's kept whole, unvisited */
let levels: Swept.t(P.key, (S.level, bool, array(entry), list(entry))) =
  Swept.mk();

let rec level =
        (
          ~own: bool,
          top: S.level,
          rev_steps: list((S.level, int)),
          lv: S.level,
        )
        : list(entry) => {
  let key = P.key_of(lv);
  let prev =
    switch (Swept.find_opt(levels, key)) {
    | Some((lv', own', es, l)) when lv' === lv => Some((own', es, l))
    | _ => None
    };
  let es =
    Array.init(
      Array.length(lv.spans),
      k => {
        let steps = [(lv, k), ...rev_steps];
        let path = P.path_of(top, List.rev(steps));
        let own_k = own && P.unshared(lv, k);
        let children = () =>
          switch (P.sub(lv, k)) {
          | Some(below) => level(~own=own_k, top, steps, below)
          | None => []
          };
        switch (prev) {
        | Some((true, es, _)) when own_k && es[k].path == path => es[k]
        | Some((_, es, _)) =>
          let children = children();
          es[k].children === children && es[k].path == path
            ? es[k]
            : {
              ...es[k],
              path,
              children,
            };
        | None => entry_at(lv, k, ~path, ~children=children())
        };
      },
    );
  switch (prev) {
  | Some((own', es', l)) when own' == own && Array.for_all2((===), es, es') => l
  | _ =>
    let l = Array.to_list(es);
    Swept.replace(levels, key, (lv, own, es, l));
    l;
  };
};

let build = (~root: Sort.t, seg: Segment.t): t => {
  Swept.next_pass(P.cache);
  Swept.next_pass(levels);
  /* members end as the parsed term reads them, so the members after an
     unfinished one keep their rows */
  let top = S.top(~ends=Rows, ~root, seg);
  {
    root,
    seg,
    entries: level(~own=true, top, [], top),
  };
};

/* the last few programs' outlines, each while its program is the same
   (Segment.same_program: a caret move re-zips the tiles above it). A user's
   program and the one View as agent shows take turns */
let recall =
    (
      cache: ref(list((Sort.t, Segment.t, t))),
      ~root: Sort.t,
      seg: Segment.t,
      mk: unit => t,
    )
    : t =>
  switch (List.find_opt(((r, s, _)) => r == root && s === seg, cache^)) {
  | Some((_, _, o)) => o
  | None =>
    let (o, rest) =
      switch (
        List.partition(
          ((r, s, _)) => r == root && Segment.same_program(s, seg),
          cache^,
        )
      ) {
      | ([(_, _, o), ..._], rest) => (o, rest)
      | ([], rest) => (mk(), rest)
      };
    cache := [(root, seg, o), ...ListUtil.take(3, rest)];
    o;
  };

let recent: ref(list((Sort.t, Segment.t, t))) = ref([]);
let of_program = (~root: Sort.t, seg: Segment.t): t =>
  recall(recent, ~root, seg, () => build(~root, seg));

/* the outline of a program as typed: of its completion, as semantics
   reads it (an unfinished definition gets its `=`, `in` or `;` and holes,
   keeping its ids), so its rows stand while it's being typed */
let sources: ref(list((Sort.t, Segment.t, t))) = ref([]);
let of_source = (~root: Sort.t, source: Segment.t): t =>
  recall(sources, ~root, source, () =>
    build(~root, fst(MakeTerm.semantic_segment(~root, source)))
  );

/* every entry, each before its children */
let rec flatten = (es: list(entry)): list(entry) =>
  List.concat_map(e => [e, ...flatten(e.children)], es);

let all = (o: t): list(entry) => flatten(o.entries);

/* the entry with handle or anchor [id], and those above it, outermost
   first */
let find = (o: t, id: Id.t): option((list(entry), entry)) => {
  let rec go = (above, es: list(entry)) =>
    List.find_map(
      e =>
        e.id == id || e.anchor == id
          ? Some((List.rev(above), e)) : go([e, ...above], e.children),
      es,
    );
  go([], o.entries);
};

/* a definition body's entry */
let in_block = (e: entry): bool =>
  switch (e.lv.owner) {
  | Block(_) => true
  | Top
  | Members(_)
  | Chain(_) => false
  };

let path_string = (e: entry): string => P.to_string(e.path);

/* the entry [s] names */
let resolve = (o: t, s: string): result(entry, string) =>
  switch (P.resolve_string(~root=o.root, o.seg, s)) {
  | Ok(f) =>
    switch (find(o, f.id)) {
    | Some((_, e)) => Ok(e)
    | None => Error("No entry at \"" ++ s ++ "\"")
    }
  | Error(e) => Error(P.message(e))
  };

/* ---- facts: what statics and the run say about entries ---- */

open Language;

/* the entries' handles holding each error, each error at its deepest
   entry: Info's item ancestry where it has it, else the entry whose core
   holds its tile */
let errors_by_entry =
    (o: t, ~info_map: Statics.Map.t, error_ids: list(Id.t))
    : Id.Map.t(list(Id.t)) => {
  let handles =
    List.fold_left(
      (m, e) => Id.Map.add(e.id, e.id, Id.Map.add(e.anchor, e.id, m)),
      Id.Map.empty,
      all(o),
    );
  /* the deepest entry whose core holds a piece, by walking down */
  let rec holding = (es: list(entry), id: Id.t, best) =>
    switch (
      List.find_opt(e => List.mem(id, Segment.ids(core(e.lv, e.k))), es)
    ) {
    | Some(e) => holding(e.children, id, Some(e.id))
    | None => best
    };
  List.fold_left(
    (acc, err) => {
      let own =
        switch (Id.Map.find_opt(err, info_map)) {
        | Some(info) => [err, ...Info.within_item(~info_map, info)]
        | None => [err]
        };
      let at =
        switch (List.find_map(id => Id.Map.find_opt(id, handles), own)) {
        | Some(_) as h => h
        | None => holding(o.entries, err, None)
        };
      switch (at) {
      | Some(h) =>
        Id.Map.update(
          h,
          l => Some([err, ...Option.value(l, ~default=[])]),
          acc,
        )
      | None => acc
      };
    },
    Id.Map.empty,
    error_ids,
  )
  |> Id.Map.map(List.rev);
};

/* how many references each binder has in the program: its variables and
   `M.x` labels, in one pass. Statics' own references (a module's
   exports, function sugar) have no token, so they don't count */
let uses_memo: Slot.t((Statics.Map.t, Segment.t), Id.Map.t(int)) =
  Slot.mk();
let uses = (o: t, ~info_map: Statics.Map.t): Id.Map.t(int) =>
  Slot.get(
    ~same=((m, s), (m', s')) => m === m' && s === s',
    uses_memo,
    (info_map, o.seg),
    () => {
      let tokens =
        List.fold_left(
          (acc, id) => Id.Map.add(id, (), acc),
          Id.Map.empty,
          Segment.ids(o.seg),
        );
      Id.Map.fold(
        (id, info, acc) =>
          switch (info) {
          | Info.InfoExp(_) when Id.Map.mem(id, tokens) =>
            switch (Info.binding_site(~info_map, info)) {
            | Some(site) when site != id =>
              let named =
                Option.bind(
                  Id.Map.find_opt(site, info_map),
                  Info.binder_name,
                );
              switch (Info.label_name(info)) {
              | Some(l) when Some(l) != named => acc
              | _ =>
                Id.Map.update(
                  site,
                  n => Some(Option.value(n, ~default=0) + 1),
                  acc,
                )
              };
            | _ => acc
            }
          | _ => acc
          },
        info_map,
        Id.Map.empty,
      );
    },
  );
