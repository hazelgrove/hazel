open Util;

/* The world tree (plans/world-tree.md, step 2): one program holding the
   standard library's items, then a module per folder of slides, each
   slide a module sealed with `{}`:

     <library items>;
     module Documentation = {
       module BasicReference : {} = { <its members> };
       module B2T2 = {
         module DotProduct : {} = ⋱
       }
     };
     module Scratch = { module Scratchpad1 : {} = { … } }

   A slide not loaded yet has an empty body (whoever assembles the world
   keeps which are loaded): sealed, it exports nothing whatever it holds,
   so checking and running the others never needs it. Every module's ids come from its path, so a slide is
   found again after edits and reloads. */

/* a slide's or folder's place: its folders, then its own name */
type path = list(string);

let key = (path: path): string => String.concat(" / ", path);

type node =
  | Slide(path, option(Segment.t))
  | Folder(path, list(node));

/* [slides] (paths, with their members when loaded) as folders and slides,
   in the order each folder first appears */
let rec nest =
        (~above: path=[], slides: list((path, option(Segment.t))))
        : list(node) =>
  switch (slides) {
  | [] => []
  | [([leaf], members), ...rest] => [
      Slide(above @ [leaf], members),
      ...nest(~above, rest),
    ]
  | [([folder, ..._], _), ..._] =>
    let (inside, outside) =
      List.partition(
        ((p, _)) =>
          switch (p) {
          | [f, _, ..._] => f == folder
          | _ => false
          },
        slides,
      );
    [
      Folder(
        above @ [folder],
        nest(
          ~above=above @ [folder],
          List.map(((p, m)) => (List.tl(p), m), inside),
        ),
      ),
      ...nest(~above, outside),
    ];
  | [([], _), ...rest] => nest(~above, rest)
  };

/* module names unique among siblings: a later clash takes a number */
let names = (leaves: list(string)): list(string) => {
  let seen = Hashtbl.create(8);
  List.map(
    leaf => {
      let base = World.module_name(leaf);
      let rec free = n => {
        let name = n == 0 ? base : base ++ string_of_int(n + 1);
        Hashtbl.mem(seen, name) ? free(n + 1) : name;
      };
      let name = free(0);
      Hashtbl.replace(seen, name, ());
      name;
    },
    leaves,
  );
};

let parse_seeded = (~seed: string, text: string): option(Segment.t) =>
  /* the text parser: FastParse misreads `{}` here */
  Id.with_seed(seed, () => Parser.to_segment(text, ~root=Mod));

/* `module Name : {} = {⏎}` for the slide at [path] */
let slide_wrapper = (~name: string, path: path): option(Segment.t) =>
  parse_seeded(
    ~seed="world-slide:" ++ key(path),
    "module " ++ name ++ " : {} = {\n}",
  );

/* `module Name = {⏎}` for the folder at [path] */
let folder_wrapper = (~name: string, path: path): option(Segment.t) =>
  parse_seeded(
    ~seed="world-folder:" ++ key(path),
    "module " ++ name ++ " = {\n}",
  );

/* a slide's and a folder's module tile, by path alone: what finds it
   after edits, renames of other slides and reloads */
let slide_id = (path: path): Id.t => Id.mk_str("world-slide:" ++ key(path));
let folder_id = (path: path): Id.t =>
  Id.mk_str("world-folder:" ++ key(path));

/* [wrapper] with its module tile's id [id] */
let with_head = (id: Id.t, wrapper: Segment.t): Segment.t => {
  let seen = ref(false);
  List.map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when ! seen^ =>
        seen := true;
        Piece.Tile({
          ...t,
          id,
        });
      | p => p
      },
    wrapper,
  );
};

let is_brace = World.is_brace;

/* the body of a slide not loaded: empty, marked so */
let unloaded = (brace: Id.t): Segment.t => [
  Piece.Grout({
    id: Id.derive(~salt="world-unloaded", brace),
    shape: Convex,
  }),
  World.newline(brace, "world-open"),
];

/* [wrapper] with [body] as its brace's: members between a linebreak
   each side, or the placeholder */
let with_body = (wrapper: Segment.t, body: option(Segment.t)): Segment.t =>
  List.map(
    (p: Piece.t) =>
      switch (p, body) {
      | (Tile(t), Some(members)) when is_brace(t) =>
        Piece.Tile({
          ...t,
          children: [World.body(t.id, members)],
        })
      | (Tile(t), None) when is_brace(t) =>
        Piece.Tile({
          ...t,
          children: [unloaded(t.id)],
        })
      | (p, _) => p
      },
    wrapper,
  );

/* members separated by `;`, a linebreak after each */
let separated = (~salt: string, items: list((Id.t, Segment.t))): Segment.t =>
  List.concat(
    List.mapi(
      (i, (id, item)) =>
        i == List.length(items) - 1
          ? item
          : item
            @ [
              Piece.Tile({
                id: Id.derive(~salt=salt ++ "-semi", id),
                form: Form.Compound(CellJoin),
                sort: Mod,
                shards: [0],
                children: [],
              }),
              World.newline(id, salt ++ "-gap"),
            ],
      items,
    ),
  );

let rec module_of = (~name: string, node: node): option((Id.t, Segment.t)) =>
  switch (node) {
  | Slide(path, members) =>
    let id = slide_id(path);
    Option.map(
      w => (id, with_body(with_head(id, w), members)),
      slide_wrapper(~name, path),
    );
  | Folder(path, kids) =>
    let id = folder_id(path);
    Option.bind(folder_wrapper(~name, path), w =>
      Option.map(
        body => (id, with_body(with_head(id, w), Some(body))),
        level(kids),
      )
    );
  }
/* a level's modules, named uniquely, separated */
and level = (nodes: list(node)): option(Segment.t) => {
  let leaf =
    fun
    | Slide(path, _)
    | Folder(path, _) => List.nth(path, List.length(path) - 1);
  let items =
    List.map2(
      (name, node) => module_of(~name, node),
      names(List.map(leaf, nodes)),
      nodes,
    );
  List.for_all(Option.is_some, items)
    ? Some(separated(~salt="world-level", List.map(Option.get, items)))
    : None;
};

/* what the pieces between the library and the tree derive from */
let root_anchor = Id.mk_str("world-root");

/* the document: [library]'s items, then [slides] in their folders */
let assemble =
    (~library: Segment.t, slides: list((path, option(Segment.t))))
    : option(Segment.t) => {
  let anchor = root_anchor;
  Option.map(
    body => World.prefix(~anchor, library) @ body,
    level(nest(slides)),
  );
};

/* the pieces from the tile with id [id] to the end of its level */
let rec from_tile = (~id: Id.t, seg: Segment.t): option(Segment.t) =>
  switch (seg) {
  | [] => None
  | [Tile(t), ..._] when t.id == id => Some(seg)
  | [Tile(t), ...rest] =>
    switch (List.find_map(from_tile(~id), t.children)) {
    | Some(found) => Some(found)
    | None => from_tile(~id, rest)
    }
  | [_, ...rest] => from_tile(~id, rest)
  };

type body =
  | Loaded(Segment.t)
  | Unloaded
  | Gone;

/* the slide [slide]'s members, if loaded */
let body = (~slide: Id.t, seg: Segment.t): body =>
  switch (from_tile(~id=slide, seg)) {
  | None => Gone
  | Some([_, ...after]) =>
    /* its definition: the next brace or placeholder at its level */
    let rec go = (ps: Segment.t) =>
      switch (ps) {
      | [Tile(t), ..._] when is_brace(t) =>
        switch (t.children) {
        | [[Grout(g), ..._]]
            when g.id == Id.derive(~salt="world-unloaded", t.id) =>
          Unloaded
        | [kid] => Loaded(World.members_of_body(t.id, kid))
        | _ => Gone
        }
      | [Secondary(_), ...rest] => go(rest)
      | _ => Gone
      };
    go(after);
  | Some([]) => Gone
  };

/* [seg] with the slide [slide]'s body [members] */
let load = (~slide: Id.t, members: Segment.t, seg: Segment.t): Segment.t => {
  /* the pieces after the slide's tile, its body filled */
  let rec fill = (ps: Segment.t): Segment.t =>
    switch (ps) {
    | [Tile(brace), ...rest] when is_brace(brace) => [
        Piece.Tile({
          ...brace,
          children: [World.body(brace.id, members)],
        }),
        ...rest,
      ]
    | [Secondary(_) as p, ...rest] => [p, ...fill(rest)]
    | ps => ps
    };
  let rec level = (ps: Segment.t): (Segment.t, bool) =>
    switch (ps) {
    | [] => ([], false)
    | [Tile(t) as p, ...rest] when t.id == slide => (
        [p, ...fill(rest)],
        true,
      )
    | [Tile(t) as p, ...rest] =>
      let found = ref(false);
      let kids =
        List.map(
          kid =>
            if (found^) {
              kid;
            } else {
              let (kid, hit) = level(kid);
              found := hit;
              kid;
            },
          t.children,
        );
      if (found^) {
        (
          [
            Piece.Tile({
              ...t,
              children: kids,
            }),
            ...rest,
          ],
          true,
        );
      } else {
        let (rest, hit) = level(rest);
        ([p, ...rest], hit);
      };
    | [p, ...rest] =>
      let (rest, hit) = level(rest);
      ([p, ...rest], hit);
    };
  fst(level(seg));
};

/* a world's elaboration cut to the slide at the end of [trail] (module
   ids from the top level down): each module on the way holds only the
   next, so only the slide runs, the library's definitions coming from
   the bake (Builtins.env_for) */
let rec run_elab = (~trail: list(Id.t), elab: Language.Exp.t): Language.Exp.t =>
  switch (trail, elab.term) {
  | ([m, ...below], Module(items)) =>
    switch (
      List.find_opt(
        (item: Language.Mod.t) => Language.Mod.rep_id(item) == m,
        items,
      )
    ) {
    | Some(item) =>
      let item =
        switch (below, item.term) {
        | ([], _) => item
        | (_, ModLet(p, def)) => {
            ...item,
            term: ModLet(p, run_elab(~trail=below, def)),
          }
        | _ => item
        };
      {
        ...elab,
        term: Module([item]),
      };
    | None => elab
    }
  | _ => elab
  };

/* [seg] split at its first top-level tile among [heads]: the library's
   items (without what assembling added after them, derived from
   [anchor]) and the rest */
let split_library =
    (~anchor: Id.t, ~heads: list(Id.t), seg: Segment.t)
    : option((Segment.t, Segment.t)) => {
  let ours =
    List.map(
      salt => Id.derive(~salt, anchor),
      ["world-library-end", "world-gap-1", "world-gap-2"],
    );
  let rec go = (before, rest: Segment.t) =>
    switch (rest) {
    | [Piece.Tile(t), ..._] when List.mem(t.id, heads) =>
      Some((
        List.filter(
          (p: Piece.t) => !List.mem(Piece.id(p), ours),
          List.rev(before),
        ),
        rest,
      ))
    | [p, ...rest] => go([p, ...before], rest)
    | [] => None
    };
  go([], seg);
};

/* [seg] with [library] in place of its library's items */
let with_library =
    (~anchor: Id.t, ~heads: list(Id.t), ~library: Segment.t, seg: Segment.t)
    : option(Segment.t) =>
  Option.map(
    ((_, rest)) => World.prefix(~anchor, library) @ rest,
    split_library(~anchor, ~heads, seg),
  );
