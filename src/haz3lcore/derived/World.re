/* The world tree's document (plans/world-tree.md): the standard library's
   items, then a slide as a module sealed with the empty signature,

     <library items>;
     module Scratchpad1 : {} = {
       <the slide's members>
     }

   assembled from what is stored apart (the library once, each slide as
   its members) and split back to store. The slide's pieces keep their
   ids; the wrapper's come from the slide's name, so a split finds it. */

/* the identifier a slide's display name gives its module */
let module_name = (name: string): string => {
  let b = Stdlib.Buffer.create(String.length(name));
  String.iter(
    c =>
      switch (c) {
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_' => Stdlib.Buffer.add_char(b, c)
      | _ => ()
      },
    name,
  );
  switch (Stdlib.Buffer.contents(b)) {
  | "" => "Slide"
  | s =>
    switch (s.[0]) {
    | 'a' .. 'z' => String.capitalize_ascii(s)
    | 'A' .. 'Z' => s
    | _ => "Slide" ++ s
    }
  };
};

/* `module X : {} = {⏎}` for [name], its ids seeded by the name */
let wrapper = (name: string): option(Segment.t) => {
  let text = "module " ++ module_name(name) ++ " : {} = {\n}";
  /* the text parser: FastParse misreads `{}` here */
  Id.with_seed("world-slide:" ++ name, () =>
    Parser.to_segment(text, ~root=Mod)
  );
};

/* the wrapper's module tile: the slide's row in the outline */
let slide_id = (name: string): option(Id.t) =>
  Option.bind(wrapper(name), seg =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) => Some(t.id)
        | _ => None
        },
      seg,
    )
  );

let is_brace = (t: Tile.t) => Tile.has_label(t, ["{", "}"]);

/* [seg]'s brace tile's child replaced */
let with_body = (seg: Segment.t, body: Segment.t): Segment.t =>
  List.map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when is_brace(t) =>
        Piece.Tile({
          ...t,
          children: [body],
        })
      | p => p
      },
    seg,
  );

let newline = (id: Id.t, salt: string): Piece.t =>
  Secondary(Secondary.mk_newline(Id.derive(~salt, id)));

/* the `;` ending the library's last member, when it has none */
let library_end = (anchor: Id.t): Piece.t =>
  Piece.Tile({
    id: Id.derive(~salt="world-library-end", anchor),
    form: Form.Compound(CellJoin),
    sort: Mod,
    shards: [0],
    children: [],
  });

let last_content = (seg: Segment.t): option(Piece.t) =>
  List.find_opt(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(_) => false
      | _ => true
      },
    List.rev(seg),
  );

/* [library]'s items, ended and spaced for the slide's module after them */
let prefix = (~anchor: Id.t, library: Segment.t): Segment.t => {
  let sep =
    switch (last_content(library)) {
    | Some(p) when Piece.is_semi(p) => []
    | Some(_) => [library_end(anchor)]
    | None => []
    };
  library
  @ sep
  @ [newline(anchor, "world-gap-1"), newline(anchor, "world-gap-2")];
};

/* the document: [library]'s items, then [name]'s members as its sealed
   module */
let assemble =
    (~library: Segment.t, ~name: string, members: Segment.t)
    : option(Segment.t) =>
  Option.bind(wrapper(name), w =>
    Option.map(
      anchor =>
        prefix(~anchor, library)
        @ with_body(
            w,
            [newline(anchor, "world-open")]
            @ members
            @ [newline(anchor, "world-close")],
          ),
      slide_id(name),
    )
  );

type parts = {
  library: Segment.t,
  members: Segment.t,
};

/* [seg] at the slide's module (its tile's id [slide]): the pieces before
   it, and it with the pieces after */
let at_slide =
    (~slide: Id.t, seg: Segment.t): option((Segment.t, Segment.t)) => {
  let rec go = (before: Segment.t, rest: Segment.t) =>
    switch (rest) {
    | [Piece.Tile(t), ..._] when t.id == slide =>
      Some((List.rev(before), rest))
    | [p, ...rest] => go([p, ...before], rest)
    | [] => None
    };
  go([], seg);
};

let body_of = (from_slide: Segment.t): option(Segment.t) =>
  List.find_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) when is_brace(t) =>
        switch (t.children) {
        | [body] => Some(body)
        | _ => None
        }
      | _ => None
      },
    from_slide,
  );

/* [seg] split back into the library's items and the slide's members, if
   the slide's module (its tile's id [slide]) is still there */
let split = (~slide: Id.t, seg: Segment.t): option(parts) => {
  let derived = salt => Id.derive(~salt, slide);
  let ours = [
    derived("world-library-end"),
    derived("world-gap-1"),
    derived("world-gap-2"),
  ];
  let mine = [derived("world-open"), derived("world-close")];
  Option.bind(at_slide(~slide, seg), ((before, from_slide)) =>
    Option.map(
      body =>
        {
          library:
            List.filter(
              (p: Piece.t) => !List.mem(Piece.id(p), ours),
              before,
            ),
          members:
            List.filter(
              (p: Piece.t) => !List.mem(Piece.id(p), mine),
              body,
            ),
        },
      body_of(from_slide),
    )
  );
};

/* [seg] with [library] in place of its library's items: the slide's
   module as it is */
let with_library =
    (~slide: Id.t, ~library: Segment.t, seg: Segment.t): option(Segment.t) =>
  Option.map(
    ((_, from_slide)) => prefix(~anchor=slide, library) @ from_slide,
    at_slide(~slide, seg),
  );

/* a world's elaboration (its module's items) cut to the slide's own
   item: what runs, the library's definitions coming from the bake
   (Builtins.env_for) */
let run_elab = (~slide: Id.t, elab: Language.Exp.t): Language.Exp.t =>
  switch (elab.term) {
  | Module(items) =>
    switch (
      List.find_opt(
        (item: Language.Mod.t) => Language.Mod.rep_id(item) == slide,
        items,
      )
    ) {
    | Some(item) => {
        ...elab,
        term: Module([item]),
      }
    | None => elab
    }
  | _ => elab
  };

/* a stored slide's members: as stored at a module root, or converted from
   an expression root (MemberForm); None when it has no member form */
let members_of = (~root: Sort.t, seg: Segment.t): option(Segment.t) =>
  switch (root) {
  | Mod => Some(seg)
  | _ => MemberForm.of_exp_root(seg)
  };
