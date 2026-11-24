/* The client's half of the eval request in pieces (EvalPieces): a
   program's pieces cut from its DefStatics items, top-level items and
   modules' members alike, each encoded once and kept while its item
   stands; a module DefStatics kept whole keeps its members' pieces
   without a walk. A module's value is its members' bindings, each in its
   member's piece, in a node of its own. A program DefStatics didn't
   analyze, or whose pieces don't account for every entry exactly once,
   goes as one piece. A world's run is cut (WorldTree.run_cut): what
   doesn't run has no piece, and a module run empty has one of its own.

   Which piece has an entry follows how DefStatics merges maps: an
   item's root entries (its chain's overlay) shadow the item's own map
   and everything inside it, so they ship in the chain's roots piece and
   nowhere below */
open Util;
open Haz3lcore;
open Language;
open Poly;
module P = EvalPieces;

/* what a split item holds around its nested value: a let's pattern and
   body, or a member's binding, as elaborated */
type shell =
  | LetShell(Pat.t, Exp.t)
  | MemberShell(Mod.t);

let shell_equal = (a: shell, b: shell): bool =>
  switch (a, b) {
  | (LetShell(p, b), LetShell(p', b')) =>
    phys_equal(p, p') && phys_equal(b, b')
  /* a member's binding is rebuilt around its value: its parts kept */
  | (MemberShell(m), MemberShell(m')) =>
    phys_equal(m, m')
    || phys_equal(m.annotation, m'.annotation)
    && (
      switch (m.term, m'.term) {
      | (ModLet(p, _), ModLet(p', _)) => phys_equal(p, p')
      | (ModExp(_), ModExp(_)) => true
      | _ => false
      }
    )
  | _ => false
  };

/* what a piece was cut from, to tell when it still holds. [skip]: the
   keys it leaves to pieces above. a module member's piece also holds its
   binding */
type source =
  | Plain(Statics.Map.t, Exp.t, list(Id.t), option(Mod.t)) /* map, elab, skip, binding */
  /* the item's annotation and shell, its own map, skip, its entries as
     projected, and its binding */
  | Shell(
      IdTagged.IdTag.t,
      shell,
      Statics.Map.t,
      list(Id.t),
      list(P.entry),
      option(Mod.t),
    )
  /* skip; per item, its root infos and their entries as projected */
  | Roots(list(Id.t), list((Id.t, list((Id.t, Info.t)), list(P.entry))))
  /* a module value's annotation, and a Mod root's own entries */
  | Node(IdTagged.IdTag.t, list(P.entry))
  | Emptied(Exp.t, option(Mod.t)) /* the item's elab, and its binding */
  | Targets(Sample.targets)
  | Whole(Exp.t, Statics.Map.t, Sample.targets);

/* [keys]: the statics map's keys the piece accounts for */
type cached = {
  source,
  bytes: string,
  hash: P.hash,
  keys: int,
};

/* a chain's pieces as last cut, and its split items' members' */
type chain_cache = {
  pieces: Stdlib.Hashtbl.t(P.key, cached),
  members: Stdlib.Hashtbl.t(Id.t, members),
}
/* a split item's members, kept while DefStatics keeps its nest */
and members = {
  nest: DefStatics.nest,
  shadowed: list(Id.t),
  cut: option(WorldTree.cut),
  cache: chain_cache,
  chain: P.chain,
  pieces: list((P.hash, string)),
  keys: int,
};

let empty_cache = () => {
  pieces: Stdlib.Hashtbl.create(16),
  members: Stdlib.Hashtbl.create(4),
};

/* a program's pieces, while the probes' witnessing holds */
type doc = {
  witnessing: EvalInfo.witnessing,
  mutable top: chain_cache,
  singles: Stdlib.Hashtbl.t(P.key, cached) /* its targets, or it whole */
};

/* by the program's term id, most recent first, as DefStatics' slots */
let docs: ref(list((Id.t, doc))) = ref([]);
let docs_cap = 8;

let doc_for = (id: Id.t, witnessing: EvalInfo.witnessing): doc => {
  let doc =
    switch (List.Assoc.find(docs^, id, ~equal=Poly.equal)) {
    | Some(d) when d.witnessing == witnessing => d
    | _ => {
        witnessing,
        top: empty_cache(),
        singles: Stdlib.Hashtbl.create(2),
      }
    };
  docs :=
    [
      (id, doc),
      ...ListUtil.take(
           docs_cap - 1,
           List.Assoc.remove(docs^, id, ~equal=Poly.equal),
         ),
    ];
  doc;
};

/* for tests: the keys the last program encoded, and why it went whole */
let encoded: ref(list(P.key)) = ref([]);
let went_whole: ref(option(string)) = ref(None);

exception Unsplittable(string);

/* a chain's cut: what it holds so far */
type acc = {
  witnessing: EvalInfo.witnessing,
  mutable chunks: list(list((P.hash, string))),
  mutable keys: int,
};

let use = (acc: acc, key: P.key, c: cached): P.slot => {
  acc.chunks = [[(c.hash, c.bytes)], ...acc.chunks];
  acc.keys = acc.keys + c.keys;
  P.{
    key,
    hash: c.hash,
  };
};

let encode = (source, content: P.content, keys, key: P.key): cached => {
  let bytes = P.encode(content);
  encoded := [key, ...encoded^];
  {
    source,
    bytes,
    hash: P.hash_of(bytes),
    keys,
  };
};

/* [key]'s piece from [old]: the one cut from the same sources, or a
   fresh cut; kept in [into] */
let piece =
    (
      acc: acc,
      ~old: Stdlib.Hashtbl.t(P.key, cached),
      ~into: Stdlib.Hashtbl.t(P.key, cached),
      key: P.key,
      ~holds: source => bool,
      ~make,
    ) => {
  let c =
    switch (Stdlib.Hashtbl.find_opt(old, key)) {
    | Some(c) when holds(c.source) => c
    | _ =>
      let (source, content, keys) = make();
      encode(source, content, keys, key);
    };
  Stdlib.Hashtbl.replace(into, key, c);
  use(acc, key, c);
};

let ids_equal = List.equal(Id.equal);

/* [map]'s keys but those at [skip] */
let keys_of = (map: Statics.Map.t, ~skip: list(Id.t)): int =>
  Id.Map.cardinal(map)
  - List.length(
      List.filter(
        ~f=id => Id.Map.mem(id, map),
        List.dedup_and_sort(~compare, skip),
      ),
    );

/* an item's terms, which its entries' elab_terms may be */
type terms = {
  graft: Exp.t,
  own: Exp.t,
  nested: option(Exp.t),
};

let elab_of = (t: terms, e: Exp.t): P.elab =>
  if (phys_equal(e, t.graft)) {
    Graft;
  } else if (phys_equal(e, t.own)) {
    Own;
  } else {
    switch (t.nested) {
    | Some(v) when phys_equal(e, v) => Nested
    | _ => Inline(e)
    };
  };

/* the entries of [infos] but those at [skip] */
let project =
    (acc: acc, t: terms, ~skip: list(Id.t), infos: list((Id.t, Info.t)))
    : list(P.entry) =>
  List.filter_map(
    ~f=
      ((id, info)) =>
        List.exists(~f=Id.equal(id), skip)
          ? None
          : EvalInfo.entry_of_info(acc.witnessing, info)
            |> Option.map(~f=(e: EvalInfo.entry) =>
                 P.{
                   id,
                   elab: elab_of(t, e.elab_term),
                   co_ctx: e.co_ctx,
                   probe_targets: e.probe_targets,
                 }
               ),
    infos,
  );

let entries_equal = (a: list(P.entry), b: list(P.entry)): bool =>
  List.equal(
    (x: P.entry, y: P.entry) =>
      Id.equal(x.id, y.id)
      && (
        switch (x.elab, y.elab) {
        | (Inline(e1), Inline(e2)) => phys_equal(e1, e2)
        | (Graft, Graft)
        | (Own, Own)
        | (Nested, Nested)
        | (Value, Value) => true
        | _ => false
        }
      )
      && compare(x.co_ctx, y.co_ctx) == 0
      && EvalInfo.equal_probe_targets(x.probe_targets, y.probe_targets),
    a,
    b,
  );

/* a module member's binding as its module's value holds it, from its
   elaboration, its module of it. a type member has none: the value drops
   it */
let bind = (src: Mod.t, elab: Exp.t): option(Mod.t) =>
  switch (src.term, elab.term) {
  | (ModType(_), Module([])) => None
  | (_, Module([binding])) => Some(binding)
  | _ => raise(Unsplittable("a member's elaboration is not its module"))
  };

/* bindings alike. a binding's definition is its hollow's, which a kept
   piece shares, so the comparison stops there */
let same_binding =
  Option.equal((a: Mod.t, b: Mod.t) =>
    phys_equal(a, b) || compare(a, b) == 0
  );

/* what a split item's members are: a let-chain member's links, whose
   chain is its value, or a module literal's members, whose bindings are
   (DefStatics.module_elab) */
type nested =
  | Links
  | Members(list(Mod.t));

/* a definition's nested value is its members' chain finished, when
   DefStatics built it so: a module literal's value at its let's
   definition, or a let-chain member's chain as its expression */
let split_of = (it: DefStatics.item) =>
  switch (it.d_nest, it.d_members) {
  | (Some(n), [_, ..._]) =>
    switch (DefStatics.top_sum(n.n_sums), it.d_elab.term) {
    | (Some({s_elab: Some(_), _}), Let(p, v, body)) =>
      switch (DefStatics.module_literal_members(~ctx=it.d_ctx_in, it.d_node)) {
      | Some((_, def, members, _)) =>
        switch (Statics.Map.lookup_exp(Exp.rep_id(def), n.n_wrap)) {
        | Some(info) when phys_equal(info.elab_term, v) =>
          Some((Members(members), n, v, LetShell(p, body)))
        | _ => None
        }
      | None => None
      }
    | (Some({s_elab: Some(top), _}), Module([{term: ModExp(v), _} as b]))
        when
          phys_equal(v, top)
          && Option.is_some(
               Option.bind(
                 DefStatics.member_of(it.d_node),
                 ~f=DefStatics.member_chain,
               ),
             ) =>
      Some((Links, n, v, MemberShell(b)))
    | (Some({s_elab: Some(_), _}), Module([{term: ModLet(_, v), _} as b])) =>
      switch (Option.map(~f=Mod.def, DefStatics.member_of(it.d_node))) {
      | Some(Some(Exp({term: Module(members), _} as def))) =>
        switch (Statics.Map.lookup_exp(Exp.rep_id(def), n.n_wrap)) {
        | Some(info) when phys_equal(info.elab_term, v) =>
          Some((Members(members), n, v, MemberShell(b)))
        | _ => None
        }
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };

/* the shell's stand-in for its nested value (constant: an unchanged
   shell encodes the same) */
let placeholder: Exp.t = Exp.temp(EmptyHole);

/* a chain's root entries: re-projected where DefStatics re-summed an
   item, and kept where they came out the same */
let roots_piece =
    (
      acc: acc,
      ~old: Stdlib.Hashtbl.t(P.key, cached),
      ~into: Stdlib.Hashtbl.t(P.key, cached),
      key: P.key,
      ~skip: list(Id.t),
      items: list((DefStatics.item, DefStatics.sum, terms)),
    ) => {
  let before =
    switch (Stdlib.Hashtbl.find_opt(old, key)) {
    | Some({source: Roots(skip', rows), _} as c)
        when ids_equal(skip', skip) =>
      Some((c, rows))
    | _ => None
    };
  /* by position, then by id where items moved */
  let by_id =
    lazy({
      let t = Stdlib.Hashtbl.create(16);
      Option.iter(
        ~f=
          ((_, rows)) =>
            List.iter(
              ~f=
                ((id, overlay, entries)) =>
                  Stdlib.Hashtbl.replace(t, id, (overlay, entries)),
              rows,
            ),
        before,
      );
      t;
    });
  let same = ref(true);
  let rec rows = (items, olds) =>
    switch (items) {
    | [] => []
    | [(it: DefStatics.item, s: DefStatics.sum, t), ...items] =>
      let (prior, olds) =
        switch (olds) {
        | [(id, overlay, entries), ...olds] when Id.equal(id, it.d_id) => (
            Some((overlay, entries)),
            olds,
          )
        | [_, ...olds] =>
          same := false;
          (Stdlib.Hashtbl.find_opt(Lazy.force(by_id), it.d_id), olds);
        | [] =>
          same := false;
          (Stdlib.Hashtbl.find_opt(Lazy.force(by_id), it.d_id), []);
        };
      let entries =
        switch (prior) {
        | Some((overlay, entries)) when phys_equal(overlay, s.s_overlay) => entries
        | Some((_, entries)) =>
          let fresh = project(acc, t, ~skip, s.s_overlay);
          entries_equal(fresh, entries)
            ? entries
            : {
              same := false;
              fresh;
            };
        | None =>
          same := false;
          project(acc, t, ~skip, s.s_overlay);
        };
      [(it.d_id, s.s_overlay, entries), ...rows(items, olds)];
    };
  let rows =
    rows(
      items,
      switch (before) {
      | Some((_, rows)) => rows
      | None => []
      },
    );
  let c =
    switch (before) {
    | Some((c, olds)) when same^ && List.length(olds) == List.length(rows) => {
        /* the same piece; the new infos make the next check cheap */
        ...c,
        source: Roots(skip, rows),
      }
    | _ =>
      encode(
        Roots(skip, rows),
        Roots(List.map(~f=((id, _, entries)) => (id, entries), rows)),
        List.fold(rows, ~init=0, ~f=(n, (_, overlay, _)) =>
          n
          + List.length(
              List.filter(
                ~f=((id, _)) => !List.exists(~f=Id.equal(id), skip),
                overlay,
              ),
            )
        ),
        key,
      )
    };
  Stdlib.Hashtbl.replace(into, key, c);
  use(acc, key, c);
};

/* a split item's piece: its shell, a hole for the nested value (in its
   binding too, if a member). the module's own map is rebuilt whenever a
   member changes, so its entries are compared as projected, as root
   entries are */
let shell_piece =
    (
      acc: acc,
      ~old: Stdlib.Hashtbl.t(P.key, cached),
      ~into: Stdlib.Hashtbl.t(P.key, cached),
      key: P.key,
      it: DefStatics.item,
      t: terms,
      n: DefStatics.nest,
      ~shell: shell,
      ~skip: list(Id.t),
      ~src: option(Mod.t),
    ) => {
  let hollow: Exp.t = {
    ...it.d_elab,
    term:
      switch (shell) {
      | LetShell(p, body) => Let(p, placeholder, body)
      | MemberShell(b) => Module([P.plug_binding(b, placeholder)])
      },
  };
  let binding = lazy(Option.bind(src, ~f=src => bind(src, hollow)));
  let source = entries =>
    Shell(
      it.d_elab.annotation,
      shell,
      n.n_wrap,
      skip,
      entries,
      Lazy.force(binding),
    );
  let fresh = lazy(project(acc, t, ~skip, Id.Map.bindings(n.n_wrap)));
  let c =
    switch (Stdlib.Hashtbl.find_opt(old, key)) {
    | Some({source: Shell(a, shell', w, skip', entries, was), _} as c)
        when
          phys_equal(a, it.d_elab.annotation)
          && shell_equal(shell', shell)
          && ids_equal(skip', skip)
          && (
            phys_equal(w, n.n_wrap)
            || entries_equal(Lazy.force(fresh), entries)
          )
          && same_binding(was, Lazy.force(binding)) => {
        ...c,
        source: source(entries),
        /* the map's other infos may come and go */
        keys: keys_of(n.n_wrap, ~skip),
      }
    | _ =>
      let entries = Lazy.force(fresh);
      encode(
        source(entries),
        Item({
          hollow,
          hole: it.d_hole,
          entries,
          binding: Lazy.force(binding),
        }),
        keys_of(n.n_wrap, ~skip),
        key,
      );
    };
  Stdlib.Hashtbl.replace(into, key, c);
  use(acc, key, c);
};

/* a module value's node: its annotation, kept while it reads the same,
   and a Mod root's own entries ([infos]), which read [value] */
let node_piece =
    (
      acc: acc,
      ~old: Stdlib.Hashtbl.t(P.key, cached),
      ~into: Stdlib.Hashtbl.t(P.key, cached),
      ~infos: list((Id.t, Info.t))=[],
      ~value: option(Exp.t)=?,
      key: P.key,
      annotation: IdTagged.IdTag.t,
    ) => {
  let entries =
    List.filter_map(
      ~f=
        ((id, info)) =>
          EvalInfo.entry_of_info(acc.witnessing, info)
          |> Option.map(~f=(e: EvalInfo.entry) =>
               P.{
                 id,
                 elab:
                   switch (value) {
                   | Some(v) when phys_equal(e.elab_term, v) => Value
                   | _ => Inline(e.elab_term)
                   },
                 co_ctx: e.co_ctx,
                 probe_targets: e.probe_targets,
               }
             ),
      infos,
    );
  piece(
    acc,
    ~old,
    ~into,
    key,
    ~holds=
      fun
      | Node(a, es) =>
        (phys_equal(a, annotation) || IdTagged.IdTag.equal(a, annotation))
        && entries_equal(es, entries)
      | _ => false,
    ~make=() =>
    (
      Node(annotation, entries),
      Node(annotation, entries): P.content,
      List.length(infos),
    )
  );
};

/* a module run empty's piece: its let with an empty module for its value
   (WorldTree.apply), its binding likewise */
let empty_piece =
    (
      acc: acc,
      ~old: Stdlib.Hashtbl.t(P.key, cached),
      ~into: Stdlib.Hashtbl.t(P.key, cached),
      it: DefStatics.item,
      ~src: option(Mod.t),
    ) => {
  /* the value run empty: a let's definition, or a member's binding (its
     module of it) */
  let hollow: Exp.t = {
    let empty = (def: Exp.t): Exp.t => {
      ...def,
      term: Module([]),
    };
    switch (it.d_elab.term) {
    | Let(_, def, _)
    | Module([{term: ModLet(_, def), _}]) => P.plug(it.d_elab, empty(def))
    | _ => raise(Unsplittable("a module run empty is not a let"))
    };
  };
  let binding = lazy(Option.bind(src, ~f=src => bind(src, hollow)));
  piece(
    acc,
    ~old,
    ~into,
    Empty(it.d_id),
    ~holds=
      fun
      | Emptied(e, was) =>
        phys_equal(e, it.d_elab) && same_binding(was, Lazy.force(binding))
      | _ => false,
    ~make=() =>
    (
      Emptied(it.d_elab, Lazy.force(binding)),
      Item({
        hollow,
        hole: it.d_hole,
        entries: [],
        binding: Lazy.force(binding),
      }),
      0,
    )
  );
};

/* the keys an item that doesn't run accounts for: those its pieces would
   hold, its map's and its root entries', less those above */
let dropped_keys =
    (it: DefStatics.item, s: DefStatics.sum, ~shadowed: list(Id.t)): int => {
  let has = id =>
    Id.Map.mem(id, it.d_map)
    || List.Assoc.mem(s.s_overlay, id, ~equal=Poly.equal);
  Id.Map.cardinal(it.d_map)
  + List.length(
      List.filter(~f=((id, _)) => !Id.Map.mem(id, it.d_map), s.s_overlay),
    )
  - List.length(
      List.filter(~f=has, List.dedup_and_sort(~compare, shadowed)),
    );
};

/* [items]' pieces from [old]'s, and the chain's new cache. [shadowed]:
   the root keys of the items holding the chain, whose entries sit in
   pieces above. [srcs]: a module's members, its items. [cut]: which
   members run (the tail always does) */
let rec chain =
        (
          acc: acc,
          ~old: chain_cache,
          ~roots: P.key,
          ~shadowed: list(Id.t),
          ~srcs: list(Mod.t),
          ~cut: option(WorldTree.cut),
          items: list(DefStatics.item),
          sums: list(DefStatics.sum),
        )
        : (P.chain, chain_cache) => {
  if (List.length(items) != List.length(sums)) {
    raise(Unsplittable("items and sums differ"));
  };
  if (Option.is_some(cut) && List.is_empty(srcs)) {
    raise(Unsplittable("a cut of no module's members"));
  };
  let srcs =
    switch (srcs) {
    | [] => List.map(~f=_ => None, items)
    | _ when List.length(srcs) == List.length(items) =>
      List.map(~f=Option.some, srcs)
    | _ => raise(Unsplittable("members and items differ"))
    };
  /* the worker finds a chain's items by id */
  let ids = Stdlib.Hashtbl.create(List.length(items));
  List.iter(
    ~f=
      (it: DefStatics.item) =>
        if (Stdlib.Hashtbl.mem(ids, it.d_id)) {
          raise(Unsplittable("an item id twice in a chain"));
        } else {
          Stdlib.Hashtbl.replace(ids, it.d_id, ());
        },
    items,
  );
  let cache = {
    pieces: Stdlib.Hashtbl.create(List.length(items) + 1),
    members: Stdlib.Hashtbl.create(4),
  };
  let part = (src: option(Mod.t)): option(WorldTree.part) =>
    switch (cut, src) {
    | (Some(c), Some(m)) => Id.Map.find_opt(Mod.rep_id(m), c)
    | _ => Some(Whole)
    };
  let cut_whole = (it: DefStatics.item, s: DefStatics.sum, src, ~inner_cut) => {
    let graft =
      switch (s.s_elab) {
      | Some(g) => g
      | None => raise(Unsplittable("an elaboration gap"))
      };
    let skip = List.map(~f=fst, s.s_overlay) @ shadowed;
    let key: P.key = Item(it.d_id);
    switch (split_of(it), inner_cut) {
    | (Some((Links, _, _, _)), Some(_))
    | (None, Some(_)) =>
      raise(Unsplittable("a cut into a module kept whole"))
    | (Some((nested, n, v, shell)), _) =>
      let m =
        switch (Stdlib.Hashtbl.find_opt(old.members, it.d_id)) {
        | Some(m)
            when
              phys_equal(m.nest, n)
              && ids_equal(m.shadowed, skip)
              && Option.equal(WorldTree.equal_cut, m.cut, inner_cut) => m
        | prior =>
          let inner = {
            witnessing: acc.witnessing,
            chunks: [],
            keys: 0,
          };
          let (members, cache) =
            chain(
              inner,
              ~old=
                switch (prior) {
                | Some(m) => m.cache
                | None => empty_cache()
                },
              ~roots=Roots(it.d_id),
              ~shadowed=skip,
              ~srcs=
                switch (nested) {
                | Links => []
                | Members(members) => members
                },
              ~cut=inner_cut,
              it.d_members,
              n.n_sums,
            );
          {
            nest: n,
            shadowed: skip,
            cut: inner_cut,
            cache,
            chain: members,
            pieces: List.concat(inner.chunks),
            keys: inner.keys,
          };
        };
      Stdlib.Hashtbl.replace(cache.members, it.d_id, m);
      acc.chunks = [m.pieces, ...acc.chunks];
      acc.keys = acc.keys + m.keys;
      let t = {
        graft,
        own: it.d_elab,
        nested: Some(v),
      };
      let slot =
        shell_piece(
          acc,
          ~old=old.pieces,
          ~into=cache.pieces,
          key,
          it,
          t,
          n,
          ~shell,
          ~skip,
          ~src,
        );
      let finish =
        switch (nested) {
        | Links => P.Chain
        | Members(_) =>
          P.ModuleValue(
            node_piece(
              acc,
              ~old=old.pieces,
              ~into=cache.pieces,
              Node(Exp.rep_id(v)): P.key,
              v.annotation,
            ),
          )
        };
      (
        P.{
          slot,
          members: Some((finish, m.chain)),
        },
        Some((it, s, t)),
      );
    | (None, None) =>
      let t = {
        graft,
        own: it.d_elab,
        nested: None,
      };
      let binding = lazy(Option.bind(src, ~f=src => bind(src, it.d_elab)));
      let slot =
        piece(
          acc,
          ~old=old.pieces,
          ~into=cache.pieces,
          key,
          ~holds=(
            fun
            | Plain(m, e, skip', was) =>
              phys_equal(m, it.d_map)
              && phys_equal(e, it.d_elab)
              && ids_equal(skip', skip)
              && same_binding(was, Lazy.force(binding))
            | _ => false
          ),
          ~make=() =>
          (
            Plain(it.d_map, it.d_elab, skip, Lazy.force(binding)),
            Item({
              hollow: it.d_elab,
              hole: it.d_hole,
              entries: project(acc, t, ~skip, Id.Map.bindings(it.d_map)),
              binding: Lazy.force(binding),
            }),
            keys_of(it.d_map, ~skip),
          )
        );
      (
        P.{
          slot,
          members: None,
        },
        Some((it, s, t)),
      );
    };
  };
  /* an item that doesn't run has no piece; one run empty, a piece of its
     own and no root entries */
  let cut_item = ((it: DefStatics.item, s: DefStatics.sum), src) =>
    switch (part(src)) {
    | None =>
      acc.keys = acc.keys + dropped_keys(it, s, ~shadowed);
      None;
    | Some(Empty) =>
      acc.keys = acc.keys + dropped_keys(it, s, ~shadowed);
      let slot =
        empty_piece(acc, ~old=old.pieces, ~into=cache.pieces, it, ~src);
      Some((
        P.{
          slot,
          members: None,
        },
        None,
      ));
    | Some(Whole) => Some(cut_whole(it, s, src, ~inner_cut=None))
    | Some(Inside(c)) => Some(cut_whole(it, s, src, ~inner_cut=Some(c)))
    };
  let cuts =
    List.filter_map(
      ~f=Fun.id,
      List.map2_exn(~f=cut_item, List.zip_exn(items, sums), srcs),
    );
  let roots =
    roots_piece(
      acc,
      ~old=old.pieces,
      ~into=cache.pieces,
      roots,
      ~skip=shadowed,
      List.filter_map(~f=snd, cuts),
    );
  (
    {
      items: List.map(~f=fst, cuts),
      roots,
    },
    cache,
  );
};

/* a piece of its own: a program's targets, or the program whole */
let single = (acc: acc, doc: doc, key: P.key, ~holds, ~make) =>
  piece(acc, ~old=doc.singles, ~into=doc.singles, key, ~holds, ~make);

let targets_piece = (acc: acc, doc: doc, id: Id.t, targets: Sample.targets) =>
  single(
    acc,
    doc,
    Targets(id),
    ~holds=
      fun
      | Targets(t) =>
        phys_equal(t, targets)
        || Id.Map.equal(Sample.equal_capture_spec, t, targets)
      | _ => false,
    ~make=() =>
    (Targets(targets), Targets(targets), 0)
  );

let whole_piece =
    (acc: acc, doc: doc, ~targets: Sample.targets, statics: CachedStatics.t) =>
  single(
    acc,
    doc,
    Whole(Exp.rep_id(statics.term)),
    ~holds=
      fun
      | Whole(e, m, t) =>
        phys_equal(e, statics.elaborated)
        && phys_equal(m, statics.info_map)
        && phys_equal(t, targets)
      | _ => false,
    ~make=() =>
    (
      Whole(statics.elaborated, statics.info_map, targets),
      Whole(
        statics.elaborated,
        EvalInfo.of_info_map(
          ~probe_all=acc.witnessing.probe_all,
          ~external_ids=acc.witnessing.external_ids,
          ~targets,
          statics.info_map,
        ),
      ),
      Id.Map.cardinal(statics.info_map),
    )
  );

/* [statics]' program in pieces, when its DefStatics analysis made it
   ([run]: statics cut from that analysis' as the run says) */
let split =
    (
      acc: acc,
      doc: doc,
      ~targets: Sample.targets,
      ~run: option(WorldTree.run),
      statics: CachedStatics.t,
    )
    : P.layout => {
  let ds =
    switch (DefStatics.last(statics.term)) {
    | Some(ds)
        when Option.is_some(run) || phys_equal(ds.merged, statics.info_map) => ds
    | _ => raise(Unsplittable("no analysis of these statics"))
    };
  /* the program is the top chain, finished as DefStatics.whole_elab */
  let (whole, cut) =
    switch (run) {
    | Some({whole, cut}) => (whole, Some(cut))
    | None => (statics.elaborated, None)
    };
  switch (DefStatics.whole_elab(ds)) {
  | Some(e) when phys_equal(e, whole) => ()
  | Some(_) => raise(Unsplittable("an elaboration not DefStatics'"))
  | None => raise(Unsplittable("an elaboration gap"))
  };
  let id = Exp.rep_id(ds.term);
  let root = DefStatics.strip(ds.term);
  let (chain, cache) =
    chain(
      acc,
      ~old=doc.top,
      ~roots=TopRoots(id),
      ~shadowed=[],
      ~srcs=
        switch (root.term) {
        | Module(members) => members
        | _ => []
        },
      ~cut,
      ds.items,
      ds.sums,
    );
  let finish =
    switch (root.term) {
    | Module(_) =>
      P.ModuleValue(
        node_piece(
          acc,
          ~old=doc.singles,
          ~into=doc.singles,
          /* the root's own info, which no item holds: the run's (a cut
             world's root runs as cut, WorldTree.run_statics) */
          ~infos=
            List.filter_map(
              ~f=
                id =>
                  Option.map(
                    ~f=i => (id, i),
                    Id.Map.find_opt(id, statics.info_map),
                  ),
              IdTagged.ids(root),
            ),
          ~value=statics.elaborated,
          Node(Exp.rep_id(root)): P.key,
          statics.elaborated.annotation,
        ),
      )
    | _ => P.Chain
    };
  let targets = targets_piece(acc, doc, id, targets);
  /* every key in exactly one piece (or what doesn't run), or the worker's
     map would differ */
  if (acc.keys != Id.Map.cardinal(ds.merged)) {
    raise(Unsplittable("pieces hold other keys than the statics"));
  };
  doc.top = cache;
  Pieces({
    chain,
    finish,
    targets,
  });
};

/* the program [statics] elaborated, as pieces, its eval info projected
   for [targets] as EvalInfo.of_info_map does. [run]: a world's, [statics]
   cut as it says */
let program =
    (
      ~probe_all: bool,
      ~external_ids: list(Id.t)=[],
      ~targets: Sample.targets,
      ~run: option(WorldTree.run)=?,
      statics: CachedStatics.t,
    )
    : P.program => {
  let witnessing = EvalInfo.witnessing(~probe_all, ~external_ids, ~targets);
  let doc = doc_for(Exp.rep_id(statics.term), witnessing);
  let acc = {
    witnessing,
    chunks: [],
    keys: 0,
  };
  encoded := [];
  let layout =
    switch (split(acc, doc, ~targets, ~run, statics)) {
    | layout =>
      went_whole := None;
      layout;
    | exception (Unsplittable(why)) =>
      went_whole := Some(why);
      acc.chunks = [];
      P.Whole(whole_piece(acc, doc, ~targets, statics));
    };
  {
    layout,
    pieces: List.concat(acc.chunks),
  };
};
