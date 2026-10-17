open Haz3lcore;

/* The standard library as a document the app shows at the program level
   (plans/stdlib.md §6). Editable: an edit applies when the slide is shown
   again (re-baked, installed here and in the worker), and is kept in the
   browser until reset. */

/* the shipped source, as loaded at boot (StdlibServed) */
let shipped: ref(string) = ref("");

/* where an edited library's source is kept (HazelDB) */
let key = "stdlib:text";

/* the source as last applied */
let source: ref(string) = ref("");

let ship = (text: string): unit => {
  shipped := text;
  source := text;
};

/* bumped by each install after boot: a slide shown again re-checks and
   re-runs once per change (Workspace.calculate) */
let generation: ref(int) = ref(0);

/* the last source parsed, and its segment */
let parsed: ref(option((string, Segment.t))) = ref(None);

let segment = (text: string): Segment.t =>
  switch (parsed^) {
  | Some((t, seg)) when t == text => seg
  | _ =>
    let seg =
      switch (FastParse.of_text(~root=Mod, text)) {
      | Some(seg) => seg
      /* an edit the fast path refuses: the editor's own parser */
      | None => Parser.to_segment(text, ~root=Mod) |> Option.get
      };
    parsed := Some((text, seg));
    seg;
  };

/* an editor over the applied source, caret at the start */
let editor = (): CellEditor.Model.t =>
  ScratchFocus.cell_of_seg(~root=Mod, segment(source^));

let text_of = (ed: CellEditor.Model.t): string =>
  Printer.of_segment(~holes="?", ed.editor.editor.syntax.segment);

let install = (baked: Language.Library.baked): unit => {
  Language.Library.install_baked(baked);
  WorkerClient.install_library(Marshal.to_string(baked, []));
};

/* static errors in the applied source: it applies anyway, as any program
   with errors runs */
let errors: ref(int) = ref(0);

/* [text] baked and installed, here and in the worker, and kept (or
   forgotten, if it is the shipped source): whether it changed anything, or
   why it can't be baked at all (its shape, not its errors) */
let apply = (text: string): result(bool, string) =>
  if (text == source^) {
    Ok(false);
  } else {
    switch (LibraryBake.of_text(~strict=false, text)) {
    | Error(msg) => Error(msg)
    | Ok((baked, n, _)) =>
      install(baked);
      errors := n;
      source := text;
      text == shipped^ ? HazelDB.kv_remove(key) : HazelDB.kv_save(key, text);
      incr(generation);
      Ok(true);
    };
  };

let reset = (): unit => ignore(apply(shipped^));

let edited = (): bool => source^ != shipped^;

/* at boot: an edit kept from an earlier session, applied over the shipped
   library */
let restore = (): unit =>
  switch (HazelDB.kv_get(key)) {
  | Some(text) when text != shipped^ =>
    switch (apply(text)) {
    /* before any slide is checked: nothing to re-check */
    | Ok(_) => generation := 0
    | Error(msg) => print_endline("stdlib: the kept edit fails: " ++ msg)
    }
  | _ => ()
  };

/* statics only, against the builtins alone (the library never sees
   itself); never a worker request: the slide's evaluation keeps the
   worker */
let calculate =
    (~settings: Language.CoreSettings.t, ~is_edited, ed: CellEditor.Model.t)
    : CellEditor.Model.t =>
  CellEditor.Update.calculate(
    ~settings={
      ...settings,
      dynamics: false,
    },
    /* never analyzed yet: as a slide's load is */
    ~is_edited=is_edited || Id.Map.is_empty(ed.editor.statics.info_map),
    /* per item: a monolithic pass over the library overflows the stack */
    ~compositional=true,
    ~ctx=Language.Builtins.ctx_base(Some(Language.Operators.default_mode)),
    ~queue_worker=None,
    ~stitch=x => x,
    ed,
  );
