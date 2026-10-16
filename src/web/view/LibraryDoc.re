open Haz3lcore;

/* The standard library as a document the app shows at the program level
   (plans/stdlib.md §6): its baked source, parsed when first shown. Read
   only for now. */
let segment: Lazy.t(Segment.t) =
  lazy(
    switch (FastParse.of_text(~root=Mod, StdlibSource.text)) {
    | Some(seg) => seg
    | None => failwith("stdlib: the library's source is off the fast parser")
    }
  );

/* an editor over it, caret at the start */
let editor = (): CellEditor.Model.t =>
  ScratchFocus.cell_of_seg(~root=Mod, Lazy.force(segment));

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
