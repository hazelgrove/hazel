open Haz3lcore;

/* The standard library as a document the app shows at the program level
   (plans/stdlib.md §6, §9b). Shown, it is the code that runs (installed
   from the editor's own parse); edited, it applies as it changes
   (re-baked, installed here and in the worker) and is kept in the browser
   until reset. */

/* the shipped source, as loaded at boot (StdlibServed) */
let shipped: ref(string) = ref("");

/* where an edited library's source is kept (HazelDB) */
let key = "stdlib:text";

/* the source as last applied */
let source: ref(string) = ref("");

/* the code that runs is the seeded parse of this text (LibraryBake.parse:
   the shipped blob, or an applied text), so an editor fresh from that parse
   shows it, ids and all */
let running_parse: ref(option(string)) = ref(None);

let ship = (text: string): unit => {
  shipped := text;
  source := text;
  running_parse := Some(text);
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
      switch (LibraryBake.parse(text)) {
      | Ok({segment, _}) => segment
      /* an edit the fast path refuses: the editor's own parser */
      | Error(_) => Parser.to_segment(text, ~root=Mod) |> Option.get
      };
    parsed := Some((text, seg));
    seg;
  };

/* the library's editor, kept across zooming out and back (its probes with
   it), one for every slide as the library is */
let kept: ref(option(CellEditor.Model.t)) = ref(None);

/* the segment installed with its own ids, by an apply (None: only the
   running parse, if any) */
let installed: ref(option(Segment.t)) = ref(None);

/* the library's editor changed since the code that runs was installed */
let dirty: ref(bool) = ref(false);

/* the kept editor, or one over the applied source, caret at the start:
   fresh from the running parse, it is the code that runs */
let editor = (): CellEditor.Model.t =>
  switch (kept^) {
  | Some(ed) => ed
  | None =>
    let ed = ScratchFocus.cell_of_seg(~root=Mod, segment(source^));
    if (running_parse^ == Some(source^)) {
      installed := Some(ed.editor.editor.syntax.segment);
    };
    ed;
  };

let text_of = (ed: CellEditor.Model.t): string =>
  Printer.of_segment(~holes="?", ed.editor.editor.syntax.segment);

let install = (baked: Language.Library.baked): unit => {
  Language.Library.install_baked(baked);
  WorkerClient.install_library(Marshal.to_string(baked, []));
};

/* static errors in the applied source: it applies anyway, as any program
   with errors runs */
let errors: ref(int) = ref(0);

/* the last bake's analysis, so the next is incremental */
let last_statics: ref(option(DefStatics.t)) = ref(None);

/* [seg], whose text is [text], baked with its own ids and installed here
   and in the worker: the code that runs is then the code an editor of
   [seg] shows. Kept (or forgotten, if it is the shipped source); slides
   run again. Whether it changed anything, or why it can't be baked at all
   (its shape, not its errors) */
let apply_segment_now = (text: string, seg: Segment.t): result(bool, string) =>
  switch (installed^) {
  | Some(i) when i === seg => Ok(false)
  | _ =>
    switch (MakeTerm.Incr.term_of_mod(seg).term) {
    | Module(items) =>
      switch (
        LibraryBake.of_items(
          ~strict=false,
          ~prev=?last_statics^,
          ~hash=Digest.to_hex(Digest.string(text)),
          items,
        )
      ) {
      | Error(msg) => Error(msg)
      | Ok({baked, errors: n, statics}) =>
        install(baked);
        errors := n;
        last_statics := Some(statics);
        installed := Some(seg);
        dirty := false;
        running_parse :=
          (
            switch (parsed^) {
            | Some((t, s)) when t == text && s === seg => Some(text)
            | _ => None
            }
          );
        /* the editor made next shows this parse, and these ids */
        parsed := Some((text, seg));
        if (text != source^) {
          source := text;
          text == shipped^
            ? HazelDB.kv_remove(key) : HazelDB.kv_save(key, text);
        };
        incr(generation);
        Ok(true);
      }
    | _ => Error("the library did not parse as module items")
    }
  };

/* timed (a performance measure, for the benchmarks) */
let apply_segment = (text: string, seg: Segment.t): result(bool, string) => {
  let perf = Js_of_ocaml.Js.Unsafe.global##.performance;
  perf##mark(Js_of_ocaml.Js.string("stdlib:apply:start")) |> ignore;
  let r = apply_segment_now(text, seg);
  perf##measure(
    Js_of_ocaml.Js.string("stdlib:apply"),
    Js_of_ocaml.Js.string("stdlib:apply:start"),
  )
  |> ignore;
  r;
};

let apply = (text: string): result(bool, string) =>
  apply_segment(text, segment(text));

/* an edit applies once typing pauses: a bake per keystroke is too slow
   to type through */
let apply_after_ms = 300.0;
let apply_timer: ref(option(Js_of_ocaml.Dom_html.timeout_id)) = ref(None);

let cancel_apply = () => {
  Option.iter(
    id => Js_of_ocaml.Dom_html.window##clearTimeout(id),
    apply_timer^,
  );
  apply_timer := None;
};

/* [fire] (which applies) once no edit has come for apply_after_ms */
let apply_when_idle = (~fire: unit => unit): unit => {
  dirty := true;
  cancel_apply();
  apply_timer :=
    Some(
      Js_of_ocaml.Dom_html.window##setTimeout(
        Js_of_ocaml.Js.wrap_callback(() => {
          apply_timer := None;
          fire();
        }),
        apply_after_ms,
      ),
    );
};

/* the library as [ed] holds it, if edited since the last install */
let apply_editor = (ed: CellEditor.Model.t): result(bool, string) =>
  ! dirty^ && installed^ != None
    ? Ok(false)
    : apply_segment(text_of(ed), ed.editor.editor.syntax.segment);

let reset = (): unit => {
  kept := None;
  ignore(apply(shipped^));
};

/* the library's probes, as targets for a slide's run: once the code that
   runs is the editor's (installed from its parse), so its ids are the
   editor's. Never automatic probes: only those placed in it */
let targets_memo = Util.Slot.mk();
let targets = (~settings: Language.CoreSettings.t): Language.Sample.targets =>
  switch (kept^, installed^) {
  | (Some(ed), Some(_)) =>
    let z = ed.editor.editor.state.zipper;
    let statics = ed.editor.statics;
    Util.Slot.get(
      ~same=
        ((z: Zipper.t, s, p), (z': Zipper.t, s', p')) =>
          z.refractors === z'.refractors && s === s' && p == p',
      targets_memo,
      (z, statics, settings.probe_all),
      () =>
        Haz3lcore.CachedStatics.with_targets(
          ~settings={
            ...settings,
            probe_all: false,
          },
          z,
          statics,
        ).
          targets,
    );
  | _ => Language.Sample.no_targets
  };

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
    (
      ~settings: Language.CoreSettings.t,
      ~is_edited,
      ~statics_mode,
      /* the slide's samples: its run is the library's (§9b) */
      ~dynamics: Language.Dynamics.Map.t,
      ed: CellEditor.Model.t,
    )
    : CellEditor.Model.t =>
  CellEditor.Update.calculate(
    ~extra_dynamics=dynamics,
    ~settings={
      ...settings,
      dynamics: false,
    },
    /* never analyzed yet: as a slide's load is */
    ~is_edited=is_edited || Id.Map.is_empty(ed.editor.statics.info_map),
    /* typing defers statics, as in a slide */
    ~statics_mode,
    /* per item: a monolithic pass over the library overflows the stack */
    ~compositional=true,
    ~ctx=Language.Builtins.ctx_base(Some(Language.Operators.default_mode)),
    ~queue_worker=None,
    ~stitch=x => x,
    ed,
  );
