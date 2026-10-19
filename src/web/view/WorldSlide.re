open Haz3lcore;
open Util;

/* A slide in the library's world (plans/world-tree.md, step 1): its
   program is the standard library's items, then the slide as a module
   sealed with `{}` (World). It is stored as its members alone; the
   library is LibraryDoc's, and runs from its bake. */

/* the library a world assembles with: the code that runs, ids and all */
let library = (): Segment.t => LibraryDoc.segment(LibraryDoc.source^);

/* off with `noworld` in the page's query, to compare a build with and
   without worlds (benchmarks, while step 1 settles) */
let enabled: Lazy.t(bool) =
  lazy(
    try(
      !
        Js_of_ocaml.Js.to_bool(
          Js_of_ocaml.Js.Unsafe.global##.location##.search##includes(
            Js_of_ocaml.Js.string("noworld"),
          ),
        )
    ) {
    | _ => true
    }
  );

/* [seg], the slide [name] as stored at [root], in the world: the
   document and the slide's module id; None when no library is shipped or
   the slide has no member form (it stays as it is) */
let enter =
    (~name: string, ~root: Sort.t, seg: Segment.t)
    : option((Segment.t, Id.t)) =>
  LibraryDoc.shipped^ == "" || !Lazy.force(enabled)
    ? None
    : Option.bind(World.members_of(~root, seg), members =>
        Option.bind(World.slide_id(name), slide =>
          Option.map(
            doc => (doc, slide),
            World.assemble(~library=library(), ~name, members),
          )
        )
      );

/* a world's statics see the library as its items, not as the bake's
   context: a name deleted from it is unbound */
let ctx = (): Language.Ctx.t =>
  Language.Builtins.ctx_base(Some(Language.Operators.default_mode));

/* the same pieces: zipping rebuilds the tiles around the caret as they
   were, so only those are compared, down to where they share */
let rec same_seg = (a: Segment.t, b: Segment.t): bool =>
  a === b
  || List.length(a) == List.length(b)
  && List.for_all2(same_piece, a, b)
and same_piece = (a: Piece.t, b: Piece.t): bool =>
  a === b
  || (
    switch (a, b) {
    | (Tile(t), Tile(u)) =>
      t.id == u.id
      && t.form == u.form
      && t.sort == u.sort
      && t.shards == u.shards
      && List.length(t.children) == List.length(u.children)
      && List.for_all2(same_seg, t.children, u.children)
    | (Tile(_), _)
    | (_, Tile(_)) => false
    | _ => a == b
    }
  );

/* the library's items, if [doc] isn't the one that runs */
let library_edit = (~slide: Id.t, doc: Segment.t): option(Segment.t) =>
  switch (World.split(~slide, doc)) {
  | Some({library: lib, _}) =>
    let running = library();
    same_seg(lib, running) ? None : Some(lib);
  | None => None
  };

/* [doc] with the library that runs, if it holds another */
let with_running_library = (~slide: Id.t, doc: Segment.t): option(Segment.t) =>
  switch (library_edit(~slide, doc)) {
  | Some(_) => World.with_library(~slide, ~library=library(), doc)
  | None => None
  };

/* what a world's run reads: the slide's item alone (the library's
   definitions come from the bake), and the analysis of the expressions
   in it, so a request carries the slide's size, not the library's */
let run_memo: Slot.t((Id.t, CachedStatics.t), CachedStatics.t) = Slot.mk();
let run_statics = (~slide: Id.t, s: CachedStatics.t): CachedStatics.t =>
  Slot.get(
    ~same=((a, s), (b, s')) => a == b && s === s',
    run_memo,
    (slide, s),
    () => {
      let elaborated = World.run_elab(~slide, s.elaborated);
      let ids = ref([]);
      let _: Language.Exp.t =
        Language.Exp.map_term(
          ~f_exp=
            (cont, e) => {
              ids := Language.IdTagged.ids(e) @ ids^;
              cont(e);
            },
          elaborated,
        );
      let info_map =
        List.fold_left(
          (m, id) =>
            switch (Id.Map.find_opt(id, s.info_map)) {
            | Some(info) => Id.Map.add(id, info, m)
            | None => m
            },
          Id.Map.empty,
          ids^,
        );
      {
        ...s,
        elaborated,
        info_map,
      };
    },
  );

/* what a slide is on its own (export, a share link): a world's members,
   at a module root */
let own_editor = (code: ScratchModel.Scratchpad.code): CellEditor.Model.t =>
  switch (
    Option.bind(code.world, slide =>
      World.split(~slide, Program.document(code.program))
    )
  ) {
  | Some({members, _}) =>
    CellEditor.Model.mk(Editor.Model.mk(Zipper.unzip(members), ~root=Mod))
  | None => Program.whole(code.program)
  };
