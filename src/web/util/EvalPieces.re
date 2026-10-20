/* An eval request in pieces, one per definition (a DefStatics item: a
   top-level item or a module's member), so the main thread encodes and
   posts only what changed and the worker composes the program the way
   DefStatics did: each item's elaboration grafted into the body hole of
   the one above, a module's members finished into its value. The
   client cuts the pieces (EvalSplit); this half, which the worker links,
   is language-only */
open Util;
open Language;

/* a piece's identity across requests */
[@deriving (show({with_path: false}), sexp, yojson)]
type key =
  | Item(Id.t) /* a definition, by its rep id */
  | Roots(Id.t) /* the root entries of the members of the item with this id */
  | TopRoots(Id.t) /* a program's top-level root entries, by its term's id */
  | Targets(Id.t) /* a program's probe targets, by its term's id */
  | Whole(Id.t); /* a program in one piece, by its term's id */

/* a piece's content hash: its bytes under two seeds, and their length */
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

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  id: Id.t,
  elab,
  co_ctx: CoCtx.t,
  probe_targets: EvalInfo.probe_targets,
};

/* a definition: its elaboration, a hole for its body (and, when its
   members are pieces of their own, for its nested value), and the eval
   info it owns */
[@deriving (show({with_path: false}), sexp, yojson)]
type item = {
  hollow: Exp.t,
  hole: option(Id.t), /* None: a chain's tail */
  entries: list(entry),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type content =
  | Item(item)
  /* a chain's root entries, per item: each one's uses are most of the
     next one's, which only a piece of their own keeps shared */
  | Roots(list((Id.t, list(entry))))
  | Targets(Sample.targets)
  | Whole(Exp.t, EvalInfo.t);

let encode = (c: content): string => Marshal.to_string(c, []);
let decode = (bytes: string): content => Marshal.from_string(bytes, 0);

/* how a split item's nested value comes of its members' chain */
[@deriving (show({with_path: false}), sexp, yojson)]
type finish =
  | Chain /* the chain itself (a let-chain member) */
  | ModuleValue(Id.t); /* a module's value (ModuleHelpers.module_elab) */

[@deriving (show({with_path: false}), sexp, yojson)]
type slot = {
  key,
  hash,
};

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

/* a program as the client holds it: its layout and its pieces' bytes */
[@deriving (show({with_path: false}), sexp, yojson)]
type program = {
  layout,
  pieces: list((hash, string)),
};

let slots = (layout: layout): list(slot) => {
  let rec chain = (c: chain, acc) =>
    List.fold_left(
      (acc, n: node) => {
        let acc = [n.slot, ...acc];
        switch (n.members) {
        | Some((_, c)) => chain(c, acc)
        | None => acc
        };
      },
      [c.roots, ...acc],
      c.items,
    );
  switch (layout) {
  | Pieces({chain: c, targets, _}) => chain(c, [targets])
  | Whole(s) => [s]
  };
};

/* the worker's half: a layout's program from the pieces it holds */

exception Malformed(string);

let finish_with = (f: finish, e: Exp.t): Exp.t =>
  switch (f) {
  | Chain => e
  /* never posted here: a module's value refolds from its items */
  | ModuleValue(_) => raise(Malformed("a split module value"))
  };

/* a split item's elaboration: its nested value at its let's definition */
let plug = (hollow: Exp.t, nested: Exp.t): Exp.t =>
  switch (hollow.term) {
  | Let(p, _, body) => {
      ...hollow,
      term: Let(p, nested, body),
    }
  | _ => raise(Malformed("a split item's elaboration is not a let"))
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

let add_entries = (b: built, entries: list(entry), map) =>
  List.fold_left(
    (map, e: entry) => Id.Map.add(e.id, resolve(b, e), map),
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
  /* bottom up, as DefStatics sums a chain: the top item's graft */
  let rec compose = (c: chain, map) => {
    let (top, built, map) =
      List.fold_left(
        ((below, built, map), n: node) => {
          let it =
            switch (get(n.slot)) {
            | Item(it) => it
            | _ => raise(Malformed("an item's piece holds no item"))
            };
          let (own, nested, map) =
            switch (n.members) {
            | None => (it.hollow, None, map)
            | Some((f, members)) =>
              let (top, map) = compose(members, map);
              let v = finish_with(f, top);
              (plug(it.hollow, v), Some(v), map);
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
            Id.Map.add(id, b, built),
            add_entries(b, it.entries, map),
          );
        },
        (None, Id.Map.empty, map),
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
    | Some(top) => (top, map)
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
    let (top, statics) = compose(chain, Id.Map.empty);
    let targets =
      switch (get(targets)) {
      | Targets(t) => t
      | _ => raise(Malformed("a targets piece holds no targets"))
      };
    Ok((
      finish_with(finish, top),
      {
        statics,
        targets,
      },
    ));
  };
};
