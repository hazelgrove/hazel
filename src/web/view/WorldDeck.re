open Haz3lcore;
open Util;

/* The world tree's deck (plans/world-tree.md, step 2): every
   Documentation and Scratch slide in one program (WorldTree), the
   library's items first. Its slides are named by path ("Documentation /
   B2T2 / Dot Product"), so the top bar's crumbs are the tree's folders.
   One program is live, in the current slide; switching moves it to the
   slide switched to, loading that slide's members on first visit. Each
   slide is stored as its members, under the world's key, read from where
   Documentation or Scratch kept it until then. */

module Persist = ScratchPersist;
module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;

let prefix = ScratchModel.prefix(World);

/* between a slide path's parts, as SlidePath reads them */
let sep = " / ";

let path = (name: string): WorldTree.path =>
  SlidePath.segments(SlidePath.of_string(name));

let slide_id = (name: string): Id.t => WorldTree.slide_id(path(name));

/* where a slide was kept before the tree: its deck's prefix and name */
let legacy = (name: string): option((string, string)) =>
  switch (path(name)) {
  | ["Documentation", ...rest] when rest != [] =>
    Some(("doc", String.concat(sep, rest)))
  | ["Scratch", ...rest] when rest != [] =>
    Some(("scratch", String.concat(sep, rest)))
  | _ => None
  };

/* a Documentation slide's name there: what its prose is keyed by */
let doc_name = (name: string): string =>
  switch (legacy(name)) {
  | Some(("doc", n)) => n
  | _ => name
  };

/* the slides: Documentation's code slides, then Scratch's */
let names = (): list(string) => {
  let deck = (p, defaults) =>
    switch (Persist.load_meta(p)) {
    | Some(m) => m.names
    | None => defaults
    };
  let drv = Init.documentation_drv_slide_names();
  let docs =
    deck("doc", List.map(fst, snd(Lazy.force(Init.startup).documentation)))
    |> List.filter(n => !List.mem(n, drv));
  let scratch =
    deck("scratch", List.map(fst, snd(Lazy.force(Init.startup).scratch)));
  List.map(n => "Documentation" ++ sep ++ n, docs)
  @ List.map(n => "Scratch" ++ sep ++ n, scratch);
};

/* the slide [name]'s members as stored: the world's copy, else where its
   deck kept it, else its shipped text; a program with no member form is
   one expression member */
let members = (name: string): (Segment.t, Refractors.RefractorList.t) => {
  let shipped = (p, n) => {
    let persisted =
      p == "doc"
        ? Init.default_documentation_slide_name(n)
        : Init.empty_cell_editor_persistent(~root=Exp);
    let root = persisted.editor.root;
    let z = PersistentZipper.unpersist(persisted.editor.zipper, ~root);
    (Zipper.unselect_and_zip(~erase_buffer=true, z), root, []);
  };
  let (seg, root, probes) =
    switch (Persist.stored(prefix, name)) {
    | Some(s) => s
    | None =>
      switch (legacy(name)) {
      | Some((p, n)) =>
        switch (Persist.stored(p, n)) {
        | Some(s) => s
        | None => shipped(p, n)
        }
      | None => ([], Sort.Mod, [])
      }
    };
  (World.members_of(~root, seg) |> Option.value(~default=seg), probes);
};

/* the slides whose members are in the program this session: what saves
   write (a slide not loaded holds an empty stand-in) */
let loaded: Hashtbl.t(string, unit) = Hashtbl.create(16);

let zipper = (~probes, doc: Segment.t): Zipper.t =>
  Zipper.unzip(~direction=Left, doc)
  |> Zipper.remold_regrout(Right, ~root=Mod)
  |> ZipperBase.update_refractors(_, r =>
       Refractors.{
         ...r,
         manuals: probes,
       }
     );

let editor = (~settings, ~probes, doc: Segment.t): CellEditor.Model.t =>
  CellEditor.Model.unpersist_with(
    ~settings,
    ~t_cache=WorldSlide.parse_cache,
    ~m_cache=WorldSlide.layout_cache,
    ~zipper=zipper(~probes, doc),
    Init.empty_cell_editor_persistent(~root=Mod),
  );

/* the current slide's saved view, waiting for its outline (RestorePins
   puts it in the world's paths, under the slide's module) */
let stage_view = (name: string): unit => {
  Persist.stage_side(prefix, name);
  Hashtbl.replace(
    Persist.pending_pins,
    Persist.content_key(prefix, name),
    Persist.read_side(prefix, name),
  );
  Hashtbl.remove(Persist.world_roots, Persist.content_key(prefix, name));
};

/* the deck, its program with [current]'s members loaded */
let load = (~settings): Model.t => {
  Hashtbl.reset(loaded);
  let names = names();
  let current =
    switch (Persist.load_meta(prefix)) {
    | Some({current, names: saved}) =>
      switch (List.nth_opt(saved, current)) {
      | Some(n) =>
        ListUtil.findi_opt(m => m == n, names)
        |> Option.map(fst)
        |> Option.value(~default=0)
      | None => 0
      }
    | None => 0
    };
  let current_name = List.nth(names, current);
  let (mine, probes) = members(current_name);
  Hashtbl.replace(loaded, current_name, ());
  let doc =
    WorldTree.assemble(
      ~library=WorldSlide.library(),
      List.map(
        n => (path(n), n == current_name ? Some(mine) : None),
        names,
      ),
    )
    |> Option.get;
  stage_view(current_name);
  {
    current,
    scratchpads:
      List.mapi(
        (i, n) =>
          i == current
            ? Scratchpad.mk_code(
                ~name=n,
                ~editor=editor(~settings, ~probes, doc),
                ~world=slide_id(n),
                (),
              )
            : Scratchpad.dormant_code(n),
        names,
      ),
  };
};

/* switched to the current slide: the live program moves there, with the
   slide's members loaded if they aren't yet */
let hydrate_current = (~settings, model: Model.t): Model.t => {
  let target = List.nth(model.scratchpads, model.current);
  let live =
    ListUtil.findi_opt(
      (sp: Scratchpad.t) =>
        switch (sp.kind) {
        | Code(_) => !sp.dormant
        | Drv(_) => false
        },
      model.scratchpads,
    );
  switch (live) {
  | _ when !target.dormant => model
  | Some((k, {name: old, kind: Code(code), _})) =>
    let slide = slide_id(target.name);
    let doc = Program.document(code.program);
    let probes = Program.probes(code.program);
    let (doc, probes) =
      switch (WorldTree.body(~slide, doc)) {
      | Unloaded =>
        let (mine, own) = members(target.name);
        (WorldTree.load(~slide, mine, doc), probes @ own);
      | Loaded(_)
      | Gone => (doc, probes)
      };
    Hashtbl.replace(loaded, target.name, ());
    stage_view(target.name);
    {
      ...model,
      scratchpads:
        List.mapi(
          (i, sp: Scratchpad.t) =>
            if (i == k) {
              Scratchpad.dormant_code(old);
            } else if (i == model.current) {
              Scratchpad.mk_code(
                ~name=target.name,
                ~editor=editor(~settings, ~probes, doc),
                ~world=slide,
                (),
              );
            } else {
              sp;
            },
          model.scratchpads,
        ),
    };
  | _ => load(~settings)
  };
};

/* per loaded slide, the members last written */
let last_saved: Hashtbl.t(string, Segment.t) = Hashtbl.create(16);

/* the loaded slides other than the current one, where changed (an edit
   at the program level), as members at a module root; the current one,
   its view and the deck's place go through Persist.save_current */
let save = (model: Model.t): unit =>
  HazelDB.kv_batch(() => {
    Persist.save_current'(prefix, model);
    switch (List.nth(model.scratchpads, model.current)) {
    | {kind: Code({program, _}), dormant: false, name: current} =>
      let doc = lazy(Program.document(program));
      let probes = lazy(Program.probes(program));
      Hashtbl.iter(
        (name, ()) =>
          if (name != current) {
            switch (WorldTree.body(~slide=slide_id(name), Lazy.force(doc))) {
            | Loaded(mine) =>
              let same =
                switch (Hashtbl.find_opt(last_saved, name)) {
                | Some(prev) =>
                  List.length(prev) == List.length(mine)
                  && List.for_all2((===), prev, mine)
                | None => false
                };
              if (!same) {
                Hashtbl.replace(last_saved, name, mine);
                let ids = Segment.ids(mine);
                let z =
                  Zipper.unzip(mine)
                  |> ZipperBase.update_refractors(_, r =>
                       Refractors.{
                         ...r,
                         manuals:
                           List.filter(
                             ((id, _)) => List.mem(id, ids),
                             Lazy.force(probes),
                           ),
                       }
                     );
                Persist.save_items(prefix, name, z);
                Persist.save_slide_kind(
                  prefix,
                  name,
                  CodePersist({
                    editor:
                      Some({
                        editor:
                          Editor.Model.mk_persistent(
                            PersistentZipper.of_text(
                              PersistentZipper.to_string(z) ++ "\n",
                            ),
                            ~root=Mod,
                          ),
                        result:
                          EvalResult.Model.init |> EvalResult.Model.persist,
                      }),
                    agent: Agent.Persistent.persist(Agent.Utils.init()),
                  }),
                );
              };
            | Unloaded
            | Gone => ()
            };
          },
        loaded,
      );
    | _ => ()
    };
  });

/* the slide names Documentation's or Scratch's deck keeps */
let deck_names = (p: string): list(string) =>
  switch (Persist.load_meta(p)) {
  | Some(m) => m.names
  | None =>
    List.map(
      fst,
      snd(
        p == "doc"
          ? Lazy.force(Init.startup).documentation
          : Lazy.force(Init.startup).scratch,
      ),
    )
  };

let set_deck_names = (p: string, names: list(string)): unit => {
  let current =
    switch (Persist.load_meta(p)) {
    | Some(m) => min(m.current, max(0, List.length(names) - 1))
    | None => 0
    };
  Persist.save_meta(
    p,
    {
      current,
      names,
    },
  );
};

/* [doc] for the deck's slides as they are now: each body as [doc] has it
   (loaded or not), [fresh] ones as given, the library's items as [doc]
   has them */
let reassemble =
    (~fresh: list((string, Segment.t)), doc: Segment.t): option(Segment.t) =>
  Option.bind(
    WorldTree.split_library(
      ~anchor=WorldTree.root_anchor,
      ~heads=WorldSlide.tree_heads,
      doc,
    ),
    ((library, _)) =>
    WorldTree.assemble(
      ~library,
      List.map(
        n =>
          (
            path(n),
            switch (List.assoc_opt(n, fresh)) {
            | Some(m) => Some(m)
            | None =>
              switch (WorldTree.body(~slide=slide_id(n), doc)) {
              | Loaded(m) => Some(m)
              | Unloaded
              | Gone => None
              }
            },
          ),
        names(),
      ),
    )
  );

/* [model] after the deck's slides changed: the live program re-assembled
   for them, on [current] */
let rebuild =
    (
      ~settings,
      ~moved: list((string, Segment.t))=[],
      ~current: string,
      model: Model.t,
    )
    : Model.t =>
  switch (
    List.find_map(
      (sp: Scratchpad.t) =>
        switch (sp.kind) {
        | Code(code) when !sp.dormant => Some(code)
        | _ => None
        },
      model.scratchpads,
    )
  ) {
  | None => load(~settings)
  | Some(code) =>
    let doc = Program.document(code.program);
    let fresh =
      moved
      @ (
        switch (WorldTree.body(~slide=slide_id(current), doc)) {
        | _ when List.mem_assoc(current, moved) => []
        | Loaded(_) => []
        | Unloaded
        | Gone => [(current, fst(members(current)))]
        }
      );
    switch (reassemble(~fresh, doc)) {
    | None => model
    | Some(doc) =>
      Hashtbl.replace(loaded, current, ());
      stage_view(current);
      let names = names();
      {
        current:
          ListUtil.findi_opt(n => n == current, names)
          |> Option.map(fst)
          |> Option.value(~default=0),
        scratchpads:
          List.map(
            n =>
              n == current
                ? Scratchpad.mk_code(
                    ~name=n,
                    ~editor=
                      editor(
                        ~settings,
                        ~probes=Program.probes(code.program),
                        doc,
                      ),
                    ~world=slide_id(n),
                    (),
                  )
                : Scratchpad.dormant_code(n),
            names,
          ),
      };
    };
  };

/* a new Scratch slide, empty, the current one */
let add_scratch = (~settings, model: Model.t): Model.t => {
  let scratch = deck_names("scratch");
  let used =
    List.filter_map(
      n =>
        switch (String.split_on_char(' ', n)) {
        | ["Scratchpad", k] => int_of_string_opt(k)
        | _ => None
        },
      scratch,
    );
  let rec free = k => List.mem(k, used) ? free(k + 1) : k;
  let name = "Scratchpad " ++ string_of_int(free(1));
  set_deck_names("scratch", scratch @ [name]);
  rebuild(~settings, ~current="Scratch" ++ sep ++ name, model);
};

/* the current slide gone (its keys too), the one before it current */
let delete_current = (~settings, model: Model.t): Model.t => {
  let name = List.nth(model.scratchpads, model.current).name;
  switch (legacy(name)) {
  | Some((p, n)) =>
    set_deck_names(p, List.filter(m => m != n, deck_names(p)));
    Persist.forget_slide(prefix, name);
    Hashtbl.remove(loaded, name);
    switch (names()) {
    | [] => model
    | names =>
      let current =
        List.nth(
          names,
          max(0, min(model.current - 1, List.length(names) - 1)),
        );
      rebuild(~settings, ~current, model);
    };
  | None => model
  };
};

/* the current slide under [name] (a path in the same deck): its body as
   the program has it, its keys and its place in its deck's list moved */
let rename_current = (~settings, name: string, model: Model.t): Model.t => {
  let old = List.nth(model.scratchpads, model.current).name;
  let body =
    List.find_map(
      (sp: Scratchpad.t) =>
        switch (sp.kind) {
        | Code(code) when !sp.dormant =>
          switch (
            WorldTree.body(
              ~slide=slide_id(old),
              Program.document(code.program),
            )
          ) {
          | Loaded(m) => Some(m)
          | Unloaded
          | Gone => None
          }
        | _ => None
        },
      model.scratchpads,
    );
  switch (legacy(old), legacy(name), body) {
  | (Some((p, n)), Some((p', n')), Some(body))
      when p == p' && name != old && !List.mem(name, names()) =>
    set_deck_names(p, List.map(m => m == n ? n' : m, deck_names(p)));
    Persist.rename_slide(prefix, old, name);
    Persist.rename_slide(p, n, n');
    Hashtbl.remove(loaded, old);
    Hashtbl.remove(Persist.world_roots, Persist.content_key(prefix, old));
    rebuild(~settings, ~moved=[(name, body)], ~current=name, model);
  | _ => model
  };
};
