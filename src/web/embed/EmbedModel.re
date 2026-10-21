/* Embed mode's state (EmbedMain runs it in a frame): one read-only cell
   of Hazel's tiles holding the program a page sent, its statics on so the
   tiles carry Hazel's own error marks, and the status the frame reports
   back. Free of the DOM and of Page, so a page that mounts Hazel's editor
   itself can use the same model and update. */
open Util;
open Haz3lcore;

/* The program a page sends: one text, or the parts of a program of named
   modules, as Hazel.analyzeProgram takes them (ModuleProgram) */
module Program = {
  type parts = {
    preamble: string,
    modules: list((string, string)),
    tail: string,
  };

  type t =
    | Text(string)
    | Parts(parts);

  /* The program as the one text the cell shows */
  let text =
    fun
    | Text(text) => text
    | Parts({preamble, modules, tail}) =>
      ModuleProgram.text_of_parts(~preamble, ~modules, ~tail);

  /* The modules whose holes the status counts: a program's parts name
     them; in a text, they are the modules it defines */
  let names =
    fun
    | Text(_) => None
    | Parts({modules, _}) => Some(List.map(fst, modules));
};

/* The program's errors and holes, each with its module (None for the
   preamble, the tail and the joins between them), as analyzeProgram gives
   them, but on the cell's own ids */
module Status = {
  type t = {
    errors: list(ModuleProgram.error),
    holes: list((string, int)),
    program_holes: int,
  };

  let of_statics =
      (~names: option(list(string))=?, statics: CachedStatics.t): t => {
    let p = ModuleProgram.of_term(~names?, statics.term);
    let (holes, program_holes) = ModuleProgram.holes(p);
    {
      errors: ModuleProgram.errors(p, statics.info_map),
      holes,
      program_holes,
    };
  };
};

/* Statics on, and nothing that runs the program: no dynamics, no probes */
let settings: Settings.t = {
  ...Settings.Model.init,
  core: {
    ...Settings.Model.init.core,
    dynamics: false,
    probe_all: false,
    flip_animations: false,
  },
  autoprobe_mode: Haz3lcore.AutoProbe.Off,
  line_numbers: false,
};

module Model = {
  type t = {
    /* None until a page sends one */
    program: option(Program.t),
    cell: CodeWithStatics.Model.t,
    status: option(Status.t),
    font_metrics: FontMetrics.t,
  };

  let init: t = {
    program: None,
    cell:
      CodeWithStatics.Model.mk(
        Editor.Model.mk(Zipper.init(), ~root=Sort.Exp),
      ),
    status: None,
    font_metrics: FontMetrics.init,
  };

  let globals = (model: t): Globals.t =>
    Globals.Model.init(~settings, ~font_metrics=model.font_metrics, ());
};

module Update = {
  type t =
    | Set(Program.t)
    | SetFontMetrics(FontMetrics.t);

  /* A cell for the text, by the path a saved editor loads by, and its
     statics. Leading spaces are dropped from each line, as for a slide's
     source: Hazel lays out indentation itself, and draws written spaces
     on top of it. */
  let cell_of_text = (text: string): CodeWithStatics.Model.t => {
    let zipper =
      switch (
        PersistentZipper.parse_text(
          ~source="embed",
          ~root=Sort.Exp,
          StringUtil.trim_leading(text),
        )
      ) {
      | Some(z) => z
      | None => Zipper.init()
      };
    Editor.Model.mk(zipper, ~root=Sort.Exp)
    |> CodeWithStatics.Model.mk
    |> CodeWithStatics.Update.calculate(
         ~settings=settings.core,
         ~is_edited=true,
         ~stitch=Fun.id,
         ~dynamics=Language.Dynamics.Map.empty,
         ~is_dynamic_term=false,
       );
  };

  let update = (action: t, model: Model.t): Model.t =>
    switch (action) {
    | Set(program) when model.program == Some(program) => model
    | Set(program) =>
      let cell = cell_of_text(Program.text(program));
      {
        ...model,
        program: Some(program),
        cell,
        status:
          Some(
            Status.of_statics(~names=?Program.names(program), cell.statics),
          ),
      };
    | SetFontMetrics(font_metrics) => {
        ...model,
        font_metrics,
      }
    };
};

/* What the frame tells its page, and when: size and status only when
   they change, since a display comes every animation frame */
module Outbox = {
  type message =
    | Ready(string)
    | Size(int)
    | Status(Status.t)
    | Crash(string);

  type sent = {
    size: option(int),
    status: option(Status.t),
  };

  let nothing_sent = {
    size: None,
    status: None,
  };

  /* After a display: the messages for what changed, and what has then
     been sent. No status before a program. */
  let after_display =
      (~height: int, model: Model.t, sent: sent): (list(message), sent) => {
    let size = sent.size == Some(height) ? [] : [Size(height)];
    let status =
      switch (model.status) {
      | Some(status) when sent.status != Some(status) => [Status(status)]
      | _ => []
      };
    (
      size @ status,
      {
        size: Some(height),
        status: Option.is_some(model.status) ? model.status : sent.status,
      },
    );
  };
};

module View = {
  open Virtual_dom.Vdom;

  /* The cell: the program's tiles, read-only, with its error marks */
  let view = (model: Model.t): Node.t =>
    Node.div(
      ~attrs=[Attr.class_("cell")],
      [
        Node.div(
          ~attrs=[Attr.classes(["cell-item", "code-editor", "read-only"])],
          [
            CodeWithStatics.View.view(
              ~globals=Model.globals(model),
              model.cell,
            ),
          ],
        ),
      ],
    );
};
