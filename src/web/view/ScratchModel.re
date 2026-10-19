open Haz3lcore;
open Util;

/* what a mode's saved slides are keyed under */
let prefix = (~is_documentation) => is_documentation ? "doc" : "scratch";

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

  let mk_code = (~name, ~editor, ~agent=Agent.Utils.init(), ()): t => {
    name,
    kind:
      Code({
        program: Whole(editor),
        view: SlideView.init,
        agent,
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

  let current_code = (model: t): option(Scratchpad.code) =>
    switch (List.nth_opt(model.scratchpads, model.current)) {
    | Some({kind: Code(code), _}) => Some(code)
    | _ => None
    };

  let current_program = (model: t): option(Program.t) =>
    Option.map((c: Scratchpad.code) => c.program, current_code(model));

  let current_view = (model: t): option(SlideView.t) =>
    Option.map((c: Scratchpad.code) => c.view, current_code(model));

  let map_current = (f: Scratchpad.t => Scratchpad.t, model: t): t => {
    ...model,
    scratchpads: ListUtil.map_nth(model.current, f, model.scratchpads),
  };

  let with_current_kind = (kind: Scratchpad.kind, model: t): t =>
    map_current(
      sp =>
        {
          ...sp,
          kind,
        },
      model,
    );

  /* every Code slide's code through [f], given the slide's name */
  let map_code =
      (f: (string, Scratchpad.code) => Scratchpad.code, model: t): t => {
    ...model,
    scratchpads:
      List.map(
        (sp: Scratchpad.t) =>
          switch (sp.kind) {
          | Code(code) =>
            let code' = f(sp.name, code);
            code' === code
              ? sp
              : {
                ...sp,
                kind: Code(code'),
              };
          | Drv(_) => sp
          },
        model.scratchpads,
      ),
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
    map_code(
      (name, code) =>
        switch (view_of(name)) {
        | Some(view) => {
            ...code,
            view,
          }
        | None => code
        },
      m,
    );
  };

  /* [m]'s programs with the incremental caches of [from]'s programs for
     the same slides (Program.with_caches_of) */
  let with_caches_of = (~from: t, m: t): t => {
    let program_of = name =>
      List.find_map(
        (sp: Scratchpad.t) =>
          switch (sp.kind) {
          | Code({program, _}) when sp.name == name => Some(program)
          | _ => None
          },
        from.scratchpads,
      );
    map_code(
      (name, code) =>
        switch (program_of(name)) {
        | Some(live) => {
            ...code,
            program: Program.with_caches_of(~from=live, code.program),
          }
        | None => code
        },
      m,
    );
  };

  let scratchpad_names = (model: t): list(string) =>
    List.map((s: Scratchpad.t) => s.name, model.scratchpads);
};
