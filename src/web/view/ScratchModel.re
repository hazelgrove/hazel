open Haz3lcore;
open Util;

module Scratchpad = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type code = {
    program: Program.t,
    /* what the slide shows of it: pins, zoom, parked */
    view: SlideView.t,
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
    | Code({program, agent, _}) =>
      let editor = Program.whole(program);
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
        program: Whole(editor),
        view: SlideView.init,
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
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list(Scratchpad.t),
  };

  let current_program = (model: t): option(Program.t) =>
    switch (List.nth_opt(model.scratchpads, model.current)) {
    | Some({kind: Code({program, _}), _}) => Some(program)
    | _ => None
    };

  let current_view = (model: t): option(SlideView.t) =>
    switch (List.nth_opt(model.scratchpads, model.current)) {
    | Some({kind: Code({view, _}), _}) => Some(view)
    | _ => None
    };

  /* (id, live name) for every open cell of the current slide: outline
     labels track header renames before any splice-back */
  let focused_names = (model: t): list((Haz3lcore.Id.t, option(string))) =>
    switch (current_program(model)) {
    | Some(p) => Program.focused_names(p)
    | None => []
    };

  /* The monolithic export/import format (ScratchPersist's per-slide keys
     are the live storage). */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list(Scratchpad.persistent));

  /* [m]'s slides with the views [from] has for the same slides: undo
     restores programs, not what they show */
  let with_views_of = (~from: t, m: t): t => {
    let view_of = name =>
      List.find_map(
        (sp: Scratchpad.t) =>
          switch (sp.kind) {
          | Code({view, _}) when sp.name == name => Some(view)
          | _ => None
          },
        from.scratchpads,
      );
    {
      ...m,
      scratchpads:
        List.map(
          (sp: Scratchpad.t) =>
            switch (sp.kind, view_of(sp.name)) {
            | (Code(code), Some(view)) => {
                ...sp,
                kind:
                  Code({
                    ...code,
                    view,
                  }),
              }
            | _ => sp
            },
          m.scratchpads,
        ),
    };
  };

  let scratchpad_names = (model: t): list(string) =>
    List.map((s: Scratchpad.t) => s.name, model.scratchpads);
};
