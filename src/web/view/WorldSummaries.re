open Haz3lcore;

/* what the World's unloaded slides are (WorldSummary), computed one a
   tick while the editor is idle (plans/world-tree.md, summaries). Held for
   the session, against the library they were analyzed with */

let table: Hashtbl.t(string, (string, WorldSummary.t)) = Hashtbl.create(64);

/* bumped as summaries arrive: views keyed on it redraw */
let version = ref(0);

/* the summaries still to make, as last seen: the tick asks for a step
   only while there are some, or once the library changes */
let wanted = ref(true);

let library = () =>
  Digest.string(
    LibraryDoc.source^ != "" ? LibraryDoc.source^ : LibraryDoc.shipped^,
  );

let last_library = ref("");

let due = (): bool => {
  let lib = library();
  if (lib != last_library^) {
    last_library := lib;
    wanted := true;
  };
  wanted^;
};

/* no input for a second and no analysis waiting */
let idle = (): bool =>
  StaticsTiming.Browser.now()
  -. StaticsTiming.Browser.last_input() > 1000.
  && !CodeWithStatics.StaticsDebounce.pending();

/* [name]'s summary, made against the library as it is */
let find = (name: string): option(WorldSummary.t) =>
  switch (Hashtbl.find_opt(table, name)) {
  | Some((lib, s)) when lib == library() => Some(s)
  | _ => None
  };

/* the perf corpora (expression programs): no summary */
let skipped = (name: string): bool =>
  switch (WorldDeck.legacy(name)) {
  | Some(("doc", inner)) =>
    List.exists(
      ((n, root, _)) => n == inner && root == Sort.Exp,
      Init.documentation_slides,
    )
  | _ => false
  };

/* the slides [doc] holds unloaded that have no summary yet */
let missing = (doc: Segment.t): list(string) =>
  List.filter(
    name =>
      !skipped(name)
      && find(name) == None
      && WorldTree.body(~slide=WorldDeck.slide_id(name), doc) == Unloaded,
    WorldDeck.names(),
  );

/* one summary made, if [doc] lacks any; whether one was */
let step = (~settings: Language.CoreSettings.t, doc: Segment.t): bool =>
  switch (missing(doc)) {
  | [] =>
    wanted := false;
    false;
  | [name, ...rest] =>
    let summary =
      WorldSummary.of_members(
        ~settings,
        ~ctx_in=
          Language.Builtins.ctx_init(Some(Language.Operators.default_mode)),
        fst(WorldDeck.members(name)),
      );
    Hashtbl.replace(table, name, (library(), summary));
    incr(version);
    wanted := rest != [];
    true;
  };

/* the World's slides summarized with errors, as outline rows: each slide
   and the folders above it */
let error_rows = (): list(Id.t) =>
  List.concat_map(
    name =>
      switch (find(name)) {
      | Some(s) when s.errors > 0 =>
        let path = WorldDeck.path(name);
        List.init(List.length(path) - 1, i =>
          WorldTree.folder_id(Util.ListUtil.take(i + 1, path))
        )
        @ [WorldTree.slide_id(path)];
      | _ => []
      },
    WorldDeck.names(),
  )
  |> Util.ListUtil.dedup;
