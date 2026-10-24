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
    /* (ticket 578) the one module the cell shows, when the page asked for
       one: its body alone, read in the context Hazel has at that body in
       the whole program (the preamble's types, the modules before it) */
    focus: option(string),
    /* (ticket 578) Hazel's line numbers beside the cell, when the page
       asks (Matthew: "line numbers vanished") */
    line_numbers: bool,
    /* (^ticket(587)) reveals done, for the page to be told where the caret
       went after each (Outbox: the frame is as tall as its code, so the
       page scrolls); and a goto definition this cell could not answer:
       the name under the caret, and how many, for the page to find in
       another cell (^ticket(586)) */
    reveals: int,
    goto: option((string, int)),
    /* the context the focused body is read in */
    ctx: option(Language.Ctx.t),
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
    focus: None,
    ctx: None,
    line_numbers: false,
    reveals: 0,
    goto: None,
    status: None,
    font_metrics: FontMetrics.init,
    bodies: [],
  };

  let globals = (model: t): Globals.t =>
    Globals.Model.init(
      ~settings={
        ...settings,
        line_numbers: model.line_numbers,
      },
      ~font_metrics=model.font_metrics,
      (),
    );
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
    /* the page asks for one module's body alone, or the whole program */
    | SetFocus(option(string))
    /* the page asks for Hazel's line numbers, or not */
    | SetLineNumbers(bool)
    /* the page asks for a definition in this cell: its name */
    | Reveal(string)
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

  /* Leading spaces are dropped from each line, as for a slide's source,
     and the indentation written again as Hazel's Format(Indent) writes it
     (Matthew 2026-10-23: "indentation on RHS (hazel tiles) is missing
     too?": indentation is written spaces now, not laid out) */
  let zipper_of_text = (text: string): Zipper.t =>
    Perform.reindent_zipper @@
    (
      switch (
        PersistentZipper.parse_text(
          ~source="embed",
          ~root=Sort.Exp,
          StringUtil.trim_leading(text),
        )
      ) {
      | Some(z) => z
      | None => Zipper.init()
      }
    );

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

  /* The context Hazel has at a module's body in the whole program (full):
     the body read for the program has the ids it has there. None without
     a body read (nothing written, or a part that did not parse). */
  /* The context at a module's definition in the whole program: its
     `module Name = def in` found along the program's chain of
     definitions (as ModuleProgram.of_term walks it), in the term the whole
     program's statics were made from, so its id is surely in their map.
     (The body read on its own does not carry the ids the program's term
     gives it: looked up by those, the context was not found, and the cell
     read every type and constructor of the preamble as unbound.) */
  let module_ctx =
      (~full: CodeWithStatics.Model.t, name: string): option(Language.Ctx.t) => {
    let rec find = (e: Language.Exp.t) =>
      switch (e.term) {
      | Let(_, _, rest)
      | TyAlias(_, _, rest)
      | Theorem(_, _, rest)
      | Use(_, rest)
      | Filter(_, rest)
      | Seq(_, rest) => find(rest)
      | ModuleExp(mp, def, rest) =>
        ModuleProgram.name_of_module_pat(mp) == Some(name)
          ? Some(def) : find(rest)
      | _ => None
      };
    switch (find(full.statics.term)) {
    | Some(def) =>
      Language.Statics.Map.ctx_of(
        Language.Exp.rep_id(def),
        full.statics.info_map,
      )
    | None => None
    };
  };

  let focus_body =
      (~bodies: list(body), ~full: CodeWithStatics.Model.t, name: string)
      : (Zipper.t, string, option(Language.Ctx.t)) =>
    switch (List.find_opt((b: body) => b.name == name, bodies)) {
    | Some({read: Some(z), text, _}) => (z, text, module_ctx(~full, name))
    | _ => (Zipper.init(), "", module_ctx(~full, name))
    };

  /* A cell rooted at Mod holding a module's body, its statics in ctx */
  let mod_cell =
      (~ctx: option(Language.Ctx.t), editor: Editor.t)
      : CodeWithStatics.Model.t =>
    CodeWithStatics.Model.mk(editor)
    |> CodeWithStatics.Update.calculate(
         ~settings=settings.core,
         ~is_edited=true,
         ~ctx?,
         ~stitch=Fun.id,
         ~dynamics=Language.Dynamics.Map.empty,
         ~is_dynamic_term=false,
       );

  /* A body's text as the page keeps it: trimmed, without a last ";" */
  let body_key = (text: string): string => {
    let t = String.trim(text);
    let n = String.length(t);
    n > 0 && t.[n - 1] == ';' ? String.trim(String.sub(t, 0, n - 1)) : t;
  };

  /* A program set: the whole of it read, for the status (and, without a
     focus, for the cell). With a focus, the cell is that module's body;
     when its text is the one the cell already has (the page sending back
     what an edit here gave it, after its own compile), the cell keeps its
     editor, caret and all, and only its statics are made again, in the
     body's context now. */
  let set = (~fresh: bool, program: Program.t, model: Model.t): Model.t => {
    let (zipper, bodies) = read(~bodies=model.bodies, program);
    /* (its indentation written, as for a text: the whole-program cell) */
    let full = cell_of_zipper(Perform.reindent_zipper(zipper));
    let status =
      Some(Status.of_statics(~names=?Program.names(program), full.statics));
    switch (model.focus) {
    | Some(name) =>
      let (body, text, ctx) = focus_body(~bodies, ~full, name);
      let same =
        !fresh && Option.map(body_key, model.edited) == Some(body_key(text));
      let cell =
        same
          ? mod_cell(~ctx, model.cell.editor)
          : mod_cell(
              ~ctx,
              Editor.Model.mk(Perform.reindent_zipper(body), ~root=Sort.Mod),
            );
      {
        ...model,
        program: Some(program),
        cell,
        active: same ? model.active : false,
        edited: same ? model.edited : None,
        ctx,
        status,
        bodies,
      };
    | None => {
        ...model,
        program: Some(program),
        cell: full,
        active: false,
        ctx: None,
        status,
        bodies,
      }
    };
  };

  /* The tile defining a name in a cell: the name's own tile, at the top
     of the pattern of a let or a type (its first child), found anywhere in
     the cell's tiles, first first */
  let rec defining = (name: string, seg: Segment.t): option(Id.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) =>
          let own =
            switch (Tile.label(t), t.children) {
            | (["let", ..._] | ["type", ..._], [pat, ..._]) =>
              List.find_map(
                (q: Piece.t) =>
                  switch (q) {
                  | Tile(u) when Tile.label(u) == [name] => Some(u.id)
                  | _ => None
                  },
                pat,
              )
            | _ => None
            };
          switch (own) {
          | Some(id) => Some(id)
          | None => List.find_map(defining(name), t.children)
          };
        | _ => None
        },
      seg,
    );

  /* The tokens of the tile the caret is on, joined (a name, or a dotted
     path such as Common.flip2), for a goto definition this cell cannot
     answer */
  let indicated_name = (z: Zipper.t): option(string) =>
    switch (Indicated.index(z)) {
    | None => None
    | Some(id) =>
      let rec find = (seg: Segment.t): option(string) =>
        List.find_map(
          (p: Piece.t) =>
            switch (p) {
            | Tile(t) when t.id == id =>
              Some(String.concat("", Tile.label(t)))
            | Tile(t) => List.find_map(find, t.children)
            | _ => None
            },
          seg,
        );
      find(Zipper.unselect_and_zip(z));
    };

  let update = (action: t, model: Model.t): Model.t =>
    switch (action) {
    | Set(program) when model.program == Some(program) => model
    | Set(program) => set(~fresh=false, program, model)
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
    | SetLineNumbers(line_numbers) => {
        ...model,
        line_numbers,
      }
    /* The caret on the name's defining tile, the tile selected; nothing if
       the name is not defined in this cell */
    | Reveal(name) =>
      switch (
        defining(
          name,
          Zipper.unselect_and_zip(model.cell.editor.state.zipper),
        )
      ) {
      | None => model
      | Some(id) =>
        let step = (m: CodeWithStatics.Model.t, a: Action.t) =>
          switch (CodeEditable.Update.update(~settings, Perform(a), m)) {
          | updated => updated.model
          | exception (Action.Failure.Exception(_)) => m
          };
        let cell =
          step(
            step(model.cell, Move(Goal(TileId(id)))),
            Select(Term(Current)),
          );
        {
          ...model,
          cell,
          active: true,
          reveals: model.reveals + 1,
        };
      }
    | SetFocus(focus) when focus == model.focus => model
    | SetFocus(focus) =>
      let model = {
        ...model,
        focus,
        edited: None,
      };
      switch (model.program) {
      | Some(program) => set(~fresh=true, program, model)
      | None => model
      };
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
                ~ctx=?model.ctx,
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
          /* (a focused body's status comes with the next program the page
             sends, read whole) */
          status:
            model.focus == None
              ? Some(Status.of_statics(~names?, cell.statics)) : model.status,
          edited: changed ? Some(text) : model.edited,
          edits: changed ? model.edits + 1 : model.edits,
          /* (Matthew 2026-10-23: "goto Thing didn't work here") a goto
             answered in this cell moves the caret where the frame cannot
             scroll to it, the page sizing the frame to its code: the caret's
             place told, as after a reveal, for the page to scroll there */
          reveals:
            switch (action) {
            | Perform(Move(Goal(BindingSiteOfIndicatedVar))) =>
              model.reveals + 1
            | _ => model.reveals
            },
        };
      /* A goto definition with its binding site outside this cell (a
         type of the program's start, a member of another module): the
         name under the caret, for the page */
      | exception (Action.Failure.Exception(_)) =>
        switch (action) {
        | Perform(Move(Goal(BindingSiteOfIndicatedVar))) =>
          switch (indicated_name(model.cell.editor.state.zipper)) {
          | Some(name) => {
              ...model,
              goto:
                Some((
                  name,
                  switch (model.goto) {
                  | Some((_, n)) => n + 1
                  | None => 1
                  },
                )),
            }
          | None => model
          }
        | _ => model
        }
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
    /* (^ticket(587)) where the caret went after a reveal: its top, in the
       frame's pixels */
    | Caret(int)
    /* (^ticket(586)) a goto definition for another cell: the name, its count */
    | Goto(string, int)
    | Crash(string);

  type sent = {
    size: option(int),
    status: option(Status.t),
    edits: int,
    reveals: int,
    gotos: int,
  };

  let nothing_sent = {
    size: None,
    status: None,
    edits: 0,
    reveals: 0,
    gotos: 0,
  };

  /* After a display: the messages for what changed, and what has then
     been sent. No status before a program. */
  let after_display =
      (~caret: option(int)=?, ~height: int, model: Model.t, sent: sent)
      : (list(message), sent) => {
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
    let caret =
      switch (caret) {
      | Some(top) when model.reveals > sent.reveals => [Caret(top)]
      | _ => []
      };
    let goto =
      switch (model.goto) {
      | Some((name, n)) when n > sent.gotos => [Goto(name, n)]
      | _ => []
      };
    (
      size @ status @ edited @ caret @ goto,
      {
        size: Some(height),
        status: Option.is_some(model.status) ? model.status : sent.status,
        edits: max(sent.edits, model.edits),
        reveals: max(sent.reveals, model.reveals),
        gotos:
          switch (model.goto) {
          | Some((_, n)) => max(sent.gotos, n)
          | None => sent.gotos
          },
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
              /* (Matthew 2026-10-23: "no line numbers, still") the view draws
                 Hazel's numbers only when asked here as well as in settings */
              ~lines=true,
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
              /* (Matthew 2026-10-23: "no line numbers, still") the view draws
                 Hazel's numbers only when asked here as well as in settings */
              ~lines=true,
              model.cell,
            ),
      ],
    );
};
