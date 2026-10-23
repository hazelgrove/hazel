/* Embed mode's state (EmbedMain runs it in a frame): one read-only cell
   of Hazel's tiles holding the program a page sent, its statics on so the
   tiles carry Hazel's own error marks, and the status the frame reports
   back. The cell is selectable (CodeSelectable): a click places a caret, a
   drag or a shifted key selects, and the arrows move, so Hazel's tile
   shapes show, but nothing in it can be edited. Free of the DOM and of
   Page, so a page that mounts Hazel's editor itself can use the same model
   and update. */
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

/* The program's errors, holes and incomplete tiles, each with its module
   (None for the preamble, the tail and the joins between them), as
   analyzeProgram gives them, but on the cell's own ids */
module Status = {
  type t = {
    errors: list(ModuleProgram.error),
    holes: list((string, int)),
    program_holes: int,
    incomplete: list((string, int)),
    program_incomplete: int,
  };

  let of_statics =
      (~names: option(list(string))=?, statics: CachedStatics.t): t => {
    let p = ModuleProgram.of_term(~names?, statics.term);
    let (holes, program_holes) = ModuleProgram.holes(p);
    let (incomplete, program_incomplete) = ModuleProgram.incomplete(p);
    {
      errors: ModuleProgram.errors(p, statics.info_map),
      holes,
      program_holes,
      incomplete,
      program_incomplete,
    };
  };
};

/* Statics on, and nothing that runs the program: no dynamics, no probes.
   No completion arrows either: they offer edits, and the cell takes none. */
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
  quiver: false,
};

/* A module's body as last read: its place and name, its text, and what
   it was read as */
type body = {
  place: int,
  name: string,
  text: string,
  read: option(Zipper.t),
};

module Model = {
  type t = {
    /* None until a page sends one */
    program: option(Program.t),
    cell: CodeSelectable.Model.t,
    /* whether the cell has been clicked since its program was set: until
       then it shows no caret or selection */
    active: bool,
    /* (ticket 578) whether the page asked for a cell it can edit; then
       every action reaches the cell, and each edit's text goes back */
    editable: bool,
    /* the program's text after the last edit, and how many edits there
       have been, so the page is told each one once */
    edited: option(string),
    edits: int,
    status: option(Status.t),
    font_metrics: FontMetrics.t,
    /* the bodies of the last program of parts, so a set re-reads only
       the bodies whose text changed */
    bodies: list(body),
  };

  let init: t = {
    program: None,
    cell:
      CodeWithStatics.Model.mk(
        Editor.Model.mk(Zipper.init(), ~root=Sort.Exp),
      ),
    active: false,
    editable: false,
    edited: None,
    edits: 0,
    status: None,
    font_metrics: FontMetrics.init,
    bodies: [],
  };

  let globals = (model: t): Globals.t =>
    Globals.Model.init(~settings, ~font_metrics=model.font_metrics, ());
};

module Update = {
  type t =
    | Set(Program.t)
    | SetFontMetrics(FontMetrics.t)
    /* a click in the cell */
    | Activate
    /* a move, a selection or a copy in the cell */
    | Cell(CodeSelectable.Update.t)
    /* (ticket 578) the page asks for a cell it can edit, or not */
    | SetEditable(bool)
    /* an edit in a cell the page can edit, or any other action there */
    | Edit(CodeEditable.Update.t);

  /* A cell holding the zipper, and its statics */
  let cell_of_zipper = (zipper: Zipper.t): CodeWithStatics.Model.t =>
    Editor.Model.mk(zipper, ~root=Sort.Exp)
    |> CodeWithStatics.Model.mk
    |> CodeWithStatics.Update.calculate(
         ~settings=settings.core,
         ~is_edited=true,
         ~stitch=Fun.id,
         ~dynamics=Language.Dynamics.Map.empty,
         ~is_dynamic_term=false,
       );

  /* Leading spaces are dropped from each line, as for a slide's source:
     Hazel lays out indentation itself, and draws written spaces on top
     of it. */
  let zipper_of_text = (text: string): Zipper.t =>
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

  /* The program as one zipper, and the bodies read for it. A text is
     read whole, by the path a saved editor loads by. A program's parts
     are laid out as one text lays them out (Program.text), but each body
     is read on its own, inside its own braces (ModuleProgram.
     zipper_of_parts), and a body whose place, name and text are those of
     a body last read is not read again. */
  let read =
      (~bodies: list(body), program: Program.t): (Zipper.t, list(body)) =>
    switch (program) {
    | Text(text) => (zipper_of_text(text), [])
    | Parts({preamble, modules, tail}) =>
      let bodies_read = ref([]);
      let read_body = (place, name, text) => {
        let body =
          switch (
            List.find_opt(
              (b: body) =>
                b.place == place && b.name == name && b.text == text,
              bodies,
            )
          ) {
          | Some(b) => b
          | None => {
              place,
              name,
              text,
              read:
                PersistentZipper.parse_text(
                  ~source="embed module " ++ name,
                  ~root=Sort.Mod,
                  text,
                ),
            }
          };
        bodies_read := [body, ...bodies_read^];
        body.read;
      };
      switch (
        ModuleProgram.zipper_of_parts(
          ~read_body,
          ~preamble=StringUtil.trim_leading(preamble),
          ~modules=
            List.map(
              ((name, text)) => (name, StringUtil.trim_leading(text)),
              modules,
            ),
          ~tail=StringUtil.trim_leading(tail),
          (),
        )
      ) {
      | Some(z) => (z, List.rev(bodies_read^))
      | None => (zipper_of_text(Program.text(program)), [])
      };
    };

  let update = (action: t, model: Model.t): Model.t =>
    switch (action) {
    | Set(program) when model.program == Some(program) => model
    | Set(program) =>
      let (zipper, bodies) = read(~bodies=model.bodies, program);
      let cell = cell_of_zipper(zipper);
      {
        ...model,
        program: Some(program),
        cell,
        active: false,
        status:
          Some(
            Status.of_statics(~names=?Program.names(program), cell.statics),
          ),
        bodies,
      };
    | SetFontMetrics(font_metrics) => {
        ...model,
        font_metrics,
      }
    | Activate => {
        ...model,
        active: true,
      }
    /* A move that cannot be made (the caret at the program's end, say)
       leaves the cell as it was, as in the editor */
    | Cell(action) =>
      switch (CodeSelectable.Update.update(~settings, action, model.cell)) {
      | updated => {
          ...model,
          cell: updated.model,
        }
      | exception (Action.Failure.Exception(_)) => model
      }
    | SetEditable(editable) => {
        ...model,
        editable,
      }
    | Edit(_) when !model.editable => model
    /* An edit: the cell's statics again, the status from them, and the
       program's text, written as Hazel lays it out (a hole as ?), for the
       page. An action that cannot be done leaves the cell as it was. */
    | Edit(action) =>
      switch (CodeEditable.Update.update(~settings, action, model.cell)) {
      | updated =>
        let cell =
          updated.recalculate
            ? CodeWithStatics.Update.calculate(
                ~settings=settings.core,
                ~is_edited=true,
                ~stitch=Fun.id,
                ~dynamics=Language.Dynamics.Map.empty,
                ~is_dynamic_term=false,
                updated.model,
              )
            : updated.model;
        let names = Option.bind(model.program, Program.names);
        let text =
          Printer.of_zipper(
            ~holes="?",
            ~indent=" ",
            cell.editor.state.zipper,
          );
        let changed = model.edited != Some(text) && updated.is_edit;
        {
          ...model,
          cell,
          active: true,
          status: Some(Status.of_statics(~names?, cell.statics)),
          edited: changed ? Some(text) : model.edited,
          edits: changed ? model.edits + 1 : model.edits,
        };
      | exception (Action.Failure.Exception(_)) => model
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
    /* (ticket 578) the program's text after an edit, and its count */
    | Edited(string, int)
    | Crash(string);

  type sent = {
    size: option(int),
    status: option(Status.t),
    edits: int,
  };

  let nothing_sent = {
    size: None,
    status: None,
    edits: 0,
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
    let edited =
      switch (model.edited) {
      | Some(text) when model.edits > sent.edits => [
          Edited(text, model.edits),
        ]
      | _ => []
      };
    (
      size @ status @ edited,
      {
        size: Some(height),
        status: Option.is_some(model.status) ? model.status : sent.status,
        edits: max(sent.edits, model.edits),
      },
    );
  };
};

module View = {
  open Virtual_dom.Vdom;

  /* The cell: the program's tiles with its error marks, selectable but
     not editable. Its actions go to inject; edits never reach it, since
     CodeSelectable drops them. */
  let view = (~inject: Update.t => Effect.t(unit), model: Model.t): Node.t =>
    Node.div(
      ~attrs=[Attr.class_("cell")],
      [
        model.editable
          /* (ticket 578) a cell the page can edit: every action reaches it */
          ? CodeEditable.View.view(
              ~globals=Model.globals(model),
              ~signal=
                fun
                | MakeActive => inject(Activate),
              ~edit_mode=
                EditMode.Editable({
                  inject: action => inject(Edit(action)),
                  escape: _ => Effect.Ignore,
                  take_focus: _ => Effect.Ignore,
                  focus: model.active ? Some() : None,
                }),
              ~dynamics=Language.Dynamics.Map.empty,
              model.cell,
            )
          : CodeSelectable.View.view(
              ~globals=Model.globals(model),
              ~signal=
                fun
                | MakeActive => inject(Activate),
              ~edit_mode=
                EditMode.Editable({
                  inject: action => inject(Cell(action)),
                  escape: _ => Effect.Ignore,
                  take_focus: _ => Effect.Ignore,
                  focus: model.active ? Some() : None,
                }),
              ~dynamics=Language.Dynamics.Map.empty,
              model.cell,
            ),
      ],
    );
};
