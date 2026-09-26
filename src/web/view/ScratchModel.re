open Haz3lcore;
open Util;

/* The scratch/documentation mode's data model: slides (Scratchpad)
   and the per-slide model incl. the definition-focus stack. */

module Scratchpad = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type code = {
    editor: CellEditor.Model.t,
    agent: Agent.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind =
    | Code(code)
    | Drv(DerivationExerciseMode.Model.t);

  /* Lazy hydration: boot builds a full editor (parse + statics cache +
     agent state) for the CURRENT slide only; every other slide is a
     blank placeholder with [dormant] set, swapped for the real slide on
     first switch (Persist.hydrate_current). save_current refuses to
     write a dormant placeholder over the stored slide. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    name: string,
    kind,
    dormant: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type code_persistent = {
    editor: option(CellEditor.Model.persistent),
    agent: Agent.Persistent.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind_persistent =
    | CodePersist(code_persistent)
    | DrvPersist(DerivationExerciseMode.Model.persistent);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    name: string,
    kind: kind_persistent,
  };

  let persist = (s: t): persistent => {
    switch (s.kind) {
    | Code({editor, agent}) =>
      let current_zipper = editor.editor.editor.state.zipper;
      let current_segment = Zipper.zip(current_zipper);
      let original = Init.find_documentation_slide(s.name);
      /* Originals are text-backed (committed .hz) and mint fresh ids on
         every parse, so id-sensitive segment equality can never match;
         compare by the text projection instead — FastParse loads the
         text verbatim, so an unedited slide prints byte-identically
         modulo the stored final newline (the writer's artifact, which
         the print never carries). */
      let unchanged =
        switch (original) {
        | None => false
        | Some(pce) =>
          MarkerParse.seg_to_text(
            ~refractors=current_zipper.refractors.manuals,
            current_segment,
          )
          == Util.StringUtil.strip_final_newline(
               pce.editor.zipper.backup_text,
             )
        };
      let editor_persist =
        if (unchanged) {
          None;
        } else {
          Some(CellEditor.Model.persist(editor));
        };
      {
        name: s.name,
        kind:
          CodePersist({
            editor: editor_persist,
            agent: Agent.Persistent.persist(agent),
          }),
      };
    | Drv(m) => {
        name: s.name,
        kind:
          DrvPersist(
            DerivationExerciseMode.Model.persist(m, ~instructor_mode=false),
          ),
      }
    };
  };

  let mk_code = (~name, ~editor, ()): t => {
    name,
    kind:
      Code({
        editor,
        agent: Agent.Utils.init(),
      }),
    dormant: false,
  };

  let blank_code = (name: string): t =>
    mk_code(
      ~name,
      ~editor=CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp)),
      (),
    );

  let dormant_code = (name: string): t => {
    ...blank_code(name),
    dormant: true,
  };

  let blank_drv = (~settings, name: string): t => {
    name,
    kind:
      Drv(
        DerivationExerciseMode.Model.of_spec(
          ~settings,
          ~instructor_mode=false,
          DerivationExercise.blank_spec(~title=name, ~module_name=name),
        ),
      ),
    dormant: false,
  };
};

module Model = {
  /* Definition-focus mode, STACKED (modular-editors phases 2-3):
     focusing definitions opens a STACK of (header, body) cell pairs
     rendered INSTEAD of the master cell — the master itself stays in
     its scratchpad slot untouched (statics warm, zipper immutable
     while the stack is open). Closing splices every entry's header
     into its pattern slot and body into its definition slot.
     Transient — never persisted; persistence splices live
     (Persist.persist_spliced, a text-backed snapshot). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type stack_entry = {
    e_id: Haz3lcore.Id.t, /* the item tile's id in the master */
    /* header: pattern+signature, PAT- (or TPAT-)rooted */
    e_header: CellEditor.Model.t,
    /* module items: binder is an MPat — wrapped pat statics would
       misread the capitalized name as a constructor, so their headers
       stay statics-off */
    e_mod: bool,
    /* headerless items (top-level statements / the trailing
       expression): the static symbol shown instead of a header cell */
    e_sym: option(string),
    /* a RUN cell: one editor spanning a contiguous run of test
       statements, anchored at the first test's item id */
    e_run: bool,
    /* run cells: the item ids the run covers (first = e_id) */
    e_members: list(Haz3lcore.Id.t),
    /* body: the definition RHS, EXP- (or TYP-)rooted */
    e_body: CellEditor.Model.t,
    e_ctx: Language.Ctx.t /* frozen outer ctx at the definition */
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus_t = {
    f_entries: list(stack_entry),
    /* the master's zipped segment, cached when the stack opens (and
       updated when an entry closes): persistence splices every
       autosave tick — don't re-zip each second */
    f_master_seg: Haz3lcore.Segment.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list(Scratchpad.t),
    focus: option(focus_t),
  };

  let rec header_name = (e: stack_entry): option(string) =>
    switch (e.e_sym) {
    | Some(sym) => Some(sym)
    | None => header_name_of_cell(e)
    }
  and header_name_of_cell = (e: stack_entry): option(string) => {
    let txt =
      Haz3lcore.MarkerParse.to_text(e.e_header.editor.editor.state.zipper);
    let name =
      switch (String.index_opt(txt, ':')) {
      | Some(i) => String.sub(txt, 0, i)
      | None => txt
      };
    let name = String.trim(name);
    name == "" ? None : Some(name);
  };

  /* (id, live name) for every stack entry — outline labels track
     header renames before any splice-back. Headerless entries (tests,
     statements, ⇒) report None: their outline labels are the
     outline's own (a pinned test was showing ';' instead of its
     number). */
  let focused_names = (model: t): list((Haz3lcore.Id.t, option(string))) =>
    switch (model.focus) {
    | None => []
    | Some(f) =>
      List.concat_map(
        (e: stack_entry) =>
          e.e_run
            ? List.map(id => (id, None), e.e_members)
            : [(e.e_id, e.e_sym == None ? header_name(e) : None)],
        f.f_entries,
      )
    };

  /* The monolithic export/import format (per-slide keys are the live
     storage; see Persist below). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list(Scratchpad.persistent));

  let scratchpad_names = (model: t): list(string) =>
    List.map((s: Scratchpad.t) => s.name, model.scratchpads);
};
