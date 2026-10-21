/* An eval request in pieces, one per definition (a DefStatics item: a
   top-level item or a module's member), so the main thread encodes and
   posts only what changed and the worker composes the program the way
   DefStatics did: each item's elaboration grafted into the body hole of
   the one above, and a module's value its members' bindings, in order,
   in its node. The client cuts the pieces (EvalSplit); this half, which
   the worker links, is language-only */
open Util;
open Language;

/* a piece's identity across requests */
[@deriving (show({with_path: false}), sexp, yojson)]
type key =
  | Item(Id.t) /* a definition, by its rep id */
  | Roots(Id.t) /* the root entries of the members of the item with this id */
  | TopRoots(Id.t) /* a program's top-level root entries, by its term's id */
  | Targets(Id.t) /* a program's probe targets, by its term's id */
  | Node(Id.t) /* a module value's node, by the module's id */
  | Whole(Id.t); /* a program in one piece, by its term's id */

/* a piece's identity: its content's Merkle hash (Util.ValueHash: two
   32-bit hashes and its size), so that it is the same piece posted plain
   or shared (Util.ShareCodec), or, for a content too deep to hash, its
   bytes' hash (under two seeds, and their length) */
[@deriving (show({with_path: false}), sexp, yojson)]
type hash = (int, int, int);

let hash_of = (bytes: string): hash => (
  Hashtbl.seeded_hash(17, bytes),
  Hashtbl.seeded_hash(31, bytes),
  String.length(bytes),
);

/* an eval-info entry's elab_term: its own, or one of its item's terms
   that the worker rebuilds when it composes the program */
[@deriving (show({with_path: false}), sexp, yojson)]
type elab =
  | Inline(Exp.t)
  | Graft /* the item's elaboration, the items below grafted in */
  | Own /* the item's elaboration, its nested value in place */
  | Nested; /* a split item's nested value */

/* (^ticket(513)) one entry per term, under each of its ids: statics files
   a term's info under every id the term has (a tuple's commas, ...), and
   one entry per id repeated its elab_term, so a change inside a big term
   made every one of them new */
[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  ids: list(Id.t),
  elab,
  co_ctx: CoCtx.t,
  probe_targets: EvalInfo.probe_targets,
};

/* (^ticket(513)) entries in chunks, each ended by an entry whose id
   hashes to 0 mod 32, so the chunks follow from the entries alone: a
   changed or new entry changes its chunk, and the chunks around it are
   the same values, which a piece posted shared sends as references. as
   one list (sorted by id, so in no order an edit follows) a change made
   every cell before it new */
[@deriving (show({with_path: false}), sexp, yojson)]
type entries = list(list(entry));

let chunk = (es: list(entry)): entries => {
  let (chunks, last) =
    List.fold_left(
      ((chunks, cur), e: entry) => {
        let cur = [e, ...cur];
        (
          switch (e.ids) {
          | [id, ..._] => Hashtbl.hash(id) land 31 == 0
          | [] => false
          }
        )
          ? ([List.rev(cur), ...chunks], []) : (chunks, cur);
      },
      ([], []),
      es,
    );
  List.rev(
    switch (last) {
    | [] => chunks
    | _ => [List.rev(last), ...chunks]
    },
  );
};

/* a definition: its elaboration, a hole for its body (and, when its
   members are pieces of their own, for its nested value), and the eval
   info it owns. a module's member also has its binding as the module's
   value holds it (ModuleHelpers.refold_item), the hollow's definition in
   it; a type member, which the value drops, has none */
[@deriving (show({with_path: false}), sexp, yojson)]
type item = {
  hollow: Exp.t,
  hole: option(Id.t), /* None: a chain's tail */
  entries,
  binding: option(Mod.t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type content =
  | Item(item)
  /* a chain's root entries, per item: each one's uses are most of the
     next one's, which only a piece of their own keeps shared */
  | Roots(list((Id.t, entries)))
  | Targets(Sample.targets)
  /* a module value's node: the module's annotation (its ids) */
  | Node(IdTagged.IdTag.t)
  | Whole(Exp.t, EvalInfo.t);

let encode = (c: content): string => Marshal.to_string(c, []);
let decode = (bytes: string): content => Marshal.from_string(bytes, 0);

let hash_of_content = (c: content): option(hash) => ValueHash.hash(c);

/* (^ticket(513)) a piece as the client holds it: its content, encoded only
   when posted, or, when the content has no hash, its bytes. a changed
   definition is a new piece, posted whole (in Kids' Choice a livelit
   release posted ~490 KB): its content can go shared, the values the
   worker already has as references */
[@deriving (show({with_path: false}), sexp, yojson)]
type piece =
  | Content(content)
  | Bytes(string);

let piece_of = (c: content): (hash, piece) =>
  switch (hash_of_content(c)) {
  | Some(h) => (h, Content(c))
  | None =>
    let bytes = encode(c);
    (hash_of(bytes), Bytes(bytes));
  };

/* the posted piece [hash] names, decoded, [resolve] swapping in the
   values its references name (it raises on one it can't): None when it
   is not that piece */
let take =
    (~resolve: content => content, hash: hash, bytes: string)
    : option(content) =>
  switch (resolve(decode(bytes))) {
  | c when hash_of_content(c) == Some(hash) || hash_of(bytes) == hash =>
    Some(c)
  | _ => None
  | exception _ => None
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type slot = {
  key,
  hash,
};

/* how a split item's nested value comes of its members' chain */
[@deriving (show({with_path: false}), sexp, yojson)]
type finish =
  | Chain /* the chain itself (a let-chain member) */
  /* a module's value: its members' bindings in the node this piece
     holds (DefStatics.module_elab) */
  | ModuleValue(slot);

[@deriving (show({with_path: false}), sexp, yojson)]
type node = {
  slot,
  members: option((finish, chain)),
}
and chain = {
  items: list(node),
  roots: slot,
};

/* every piece of a program, in order */
[@deriving (show({with_path: false}), sexp, yojson)]
type layout =
  | Pieces({
      chain,
      finish, /* the top chain to the program */
      targets: slot,
    })
  | Whole(slot);

/* a program as the client holds it: its layout and its pieces */
[@deriving (show({with_path: false}), sexp, yojson)]
type program = {
  layout,
  pieces: list((hash, piece)),
};

let slots = (layout: layout): list(slot) => {
  let finish = (f: finish, acc) =>
    switch (f) {
    | Chain => acc
    | ModuleValue(s) => [s, ...acc]
    };
  let rec chain = (c: chain, acc) =>
    List.fold_left(
      (acc, n: node) => {
        let acc = [n.slot, ...acc];
        switch (n.members) {
        | Some((f, c)) => chain(c, finish(f, acc))
        | None => acc
        };
      },
      [c.roots, ...acc],
      c.items,
    );
  switch (layout) {
  | Pieces({chain: c, finish: f, targets}) =>
    chain(c, finish(f, [targets]))
  | Whole(s) => [s]
  };
};

/* the worker's half: a layout's program from the pieces it holds */

exception Malformed(string);

/* a split item's elaboration: its nested value at its let's definition */
let plug = (hollow: Exp.t, nested: Exp.t): Exp.t =>
  switch (hollow.term) {
  | Let(p, _, body) => {
      ...hollow,
      term: Let(p, nested, body),
    }
  | _ => raise(Malformed("a split item's elaboration is not a let"))
  };

/* a split member's binding: its nested value as the definition */
let plug_binding = (binding: Mod.t, nested: Exp.t): Mod.t => {
  ...binding,
  term:
    switch (binding.term) {
    | ModLet(p, _) => ModLet(p, nested)
    | ModExp(_) => ModExp(nested)
    | ModVal(x, _) => ModVal(x, nested)
    | _ => raise(Malformed("a split member's binding binds nothing"))
    },
};

/* an item's terms, which its entries refer to */
type built = {
  graft: Exp.t,
  own: Exp.t,
  nested: option(Exp.t),
};

let resolve = (b: built, e: entry): EvalInfo.entry => {
  elab_term:
    switch (e.elab) {
    | Inline(x) => x
    | Graft => b.graft
    | Own => b.own
    | Nested =>
      switch (b.nested) {
      | Some(v) => v
      | None => raise(Malformed("a nested value on an unsplit item"))
      }
    },
  co_ctx: e.co_ctx,
  probe_targets: e.probe_targets,
};

let add_entries = (b: built, entries: entries, map) =>
  List.fold_left(
    List.fold_left((map, e: entry) => {
      let r = resolve(b, e);
      List.fold_left((map, id) => Id.Map.add(id, r, map), map, e.ids);
    }),
    map,
    entries,
  );

/* [layout]'s program, its pieces from [piece]; Error: the keys of
   pieces it lacks */
let assemble =
    (~piece: hash => option(content), layout: layout)
    : result((Exp.t, EvalInfo.t), list(key)) => {
  let missing =
    List.filter_map(
      (s: slot) => Option.is_some(piece(s.hash)) ? None : Some(s.key),
      slots(layout),
    );
  let get = (s: slot): content =>
    switch (piece(s.hash)) {
    | Some(c) => c
    | None => raise(Malformed("a piece went missing"))
    };
  let finish_with = (f: finish, top: Exp.t, bindings: list(Mod.t)): Exp.t =>
    switch (f) {
    | Chain => top
    | ModuleValue(s) =>
      switch (get(s)) {
      | Node(annotation) =>
        let value: Exp.t = {
          annotation,
          term: Module(bindings),
        };
        value;
      | _ => raise(Malformed("a module's node piece holds no node"))
      }
    };
  /* bottom up, as DefStatics sums a chain: the top item's graft, and the
     items' bindings in order */
  let rec compose = (c: chain, map) => {
    let (top, bindings, built, map) =
      List.fold_left(
        ((below, bindings, built, map), n: node) => {
          let it =
            switch (get(n.slot)) {
            | Item(it) => it
            | _ => raise(Malformed("an item's piece holds no item"))
            };
          let (own, nested, binding, map) =
            switch (n.members) {
            | None => (it.hollow, None, it.binding, map)
            | Some((f, members)) =>
              let (top, inner, map) = compose(members, map);
              let v = finish_with(f, top, inner);
              (
                plug(it.hollow, v),
                Some(v),
                Option.map(b => plug_binding(b, v), it.binding),
                map,
              );
            };
          let graft =
            switch (it.hole, below) {
            | (Some(h), Some(b)) =>
              switch (Exp.graft_spine(h, b, own)) {
              | Some(g) => g
              | None => raise(Malformed("a body hole off the spine"))
              }
            | (None, _) => own
            | (Some(_), None) =>
              raise(Malformed("a hole with nothing below"))
            };
          let b = {
            graft,
            own,
            nested,
          };
          let id =
            switch (n.slot.key) {
            | Item(id) => id
            | _ => raise(Malformed("an item's key is no item's"))
            };
          (
            Some(graft),
            switch (binding) {
            | Some(binding) => [binding, ...bindings]
            | None => bindings
            },
            Id.Map.add(id, b, built),
            add_entries(b, it.entries, map),
          );
        },
        (None, [], Id.Map.empty, map),
        List.rev(c.items),
      );
    let map =
      switch (get(c.roots)) {
      | Roots(per_item) =>
        List.fold_left(
          (map, (id, entries)) =>
            switch (Id.Map.find_opt(id, built)) {
            | Some(b) => add_entries(b, entries, map)
            | None => raise(Malformed("root entries of no item"))
            },
          map,
          per_item,
        )
      | _ => raise(Malformed("a chain's roots piece holds no roots"))
      };
    switch (top) {
    | Some(top) => (top, bindings, map)
    | None => raise(Malformed("an empty chain"))
    };
  };
  switch (missing, layout) {
  | ([_, ..._], _) => Error(missing)
  | ([], Whole(s)) =>
    switch (get(s)) {
    | Whole(expr, info) => Ok((expr, info))
    | _ => raise(Malformed("a whole program's piece holds no program"))
    }
  | ([], Pieces({chain, finish, targets})) =>
    let (top, bindings, statics) = compose(chain, Id.Map.empty);
    let targets =
      switch (get(targets)) {
      | Targets(t) => t
      | _ => raise(Malformed("a targets piece holds no targets"))
      };
    Ok((
      finish_with(finish, top, bindings),
      {
        statics,
        targets,
      },
    ));
  };
};
