open Util;
open Haz3lcore;
open Poly;

/* This file follows conventions in [docs/ui-architecture.md] */

/* Where a goal editor's highlights and lines go. They are read off the
 * editor's layout when the editor or what it marks changes, not on every
 * render. */
module Deco = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type marks = {
    /* The offered steps and reflexivities, which a click takes. */
    next_steps: list(Id.t),
    refls: list(Id.t),
    /* The part the step below acts on. */
    acts_on: option(Id.t),
    /* The selected part, which the write box writes a replacement for. */
    selected: option(Id.t),
    /* The part a step the user is choosing would act on. */
    previewing: option(Id.t),
  };

  let no_marks = {
    next_steps: [],
    refls: [],
    acts_on: None,
    selected: None,
    previewing: None,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    marks,
    /* The layout read. */
    [@opaque]
    syntax: CachedSyntax.t,
    /* In the order of their marks, so that a click on one takes it by
     * index. None for a mark the editor doesn't draw. */
    next_steps: list(option(Arms.geometry)),
    refls: list(option(Arms.geometry)),
    /* A line under each row the part covers. */
    acts_on: list(Measured.measurement),
    selected: list(Measured.measurement),
    previewing: list(Measured.measurement),
  };

  let mk = (~syntax: CachedSyntax.t, marks: marks): t => {
    let measured = CachedSyntax.measured(syntax);
    let arm = id =>
      Option.bind(TermData.root_tile(id, syntax.term_data), ~f=tile =>
        Arms.geometry(
          ~term_data=syntax.term_data,
          ~terms=syntax.terms,
          ~measured,
          tile,
        )
      );
    let line = id =>
      switch (
        Option.bind(id, ~f=id =>
          TermData.extreme_measures(id, syntax.term_data, measured)
        )
      ) {
      | None => []
      | Some((origin, last)) =>
        ListUtil.range(~lo=origin.row, last.row + 1)
        |> List.map(~f=row => {
             let shape = Measured.Rows.find(row, measured.rows);
             Measured.{
               origin: {
                 row,
                 col: row == origin.row ? origin.col : shape.content_start,
               },
               last: {
                 row,
                 col: row == last.row ? last.col : shape.content_end,
               },
             };
           })
      };
    {
      marks,
      syntax,
      next_steps: List.map(~f=arm, marks.next_steps),
      refls: List.map(~f=arm, marks.refls),
      acts_on: line(marks.acts_on),
      selected: line(marks.selected),
      previewing: line(marks.previewing),
    };
  };

  /* `deco` itself while its editor's syntax and its marks are the same,
   * so that its identity says whether it changed. */
  let update = (~syntax: CachedSyntax.t, marks: marks, deco: t): t =>
    phys_equal(syntax, deco.syntax) && marks == deco.marks
      ? deco : mk(~syntax, marks);
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    editor: CodeSelectable.Model.t,
    deco: Deco.t,
    /* Whether the offered steps and reflexivities are highlighted. */
    hints: bool,
    /* Whether the line goes under the selected part instead of where the
     * step acts. */
    replacing: bool,
    previewing: bool,
  };
};

module Update = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Update.t;
};

module Selection = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CodeSelectable.Selection.t;

  let get_cursor_info = CodeSelectable.Selection.get_cursor_info;
};

module View = {
  type event =
    | MakeActive
    | TakeStep(int)
    | Refl(int);

  /* Highlights on the offered steps and reflexivities, where a click
   * takes one, and lines under parts, which take no click: they mark
   * where a step acts, which isn't a step to take. */
  let deco =
      (
        ~font_metrics: FontMetrics.t,
        signal: event => Ui_effect.t(unit),
        model: Model.t,
      ) => {
    let deco = model.deco;
    let rows = CachedSyntax.measured(deco.syntax).rows;
    let highlights = (~cls, arms, take: int => event) =>
      arms
      |> List.mapi(~f=(i, arm) =>
           Option.map(
             ~f=
               arm =>
                 WebUtil.div_c(
                   cls,
                   Arms.of_geometry(
                     ~attr=[
                       WebUtil.Attr.on_mousedown(_ => signal(take(i))),
                     ],
                     ~font_metrics,
                     ~rows,
                     arm,
                   ),
                 ),
             arm,
           )
         )
      |> List.filter_map(~f=Fn.id);
    let lines = (~cls, lines) =>
      List.map(
        ~f=
          (line: Measured.measurement) =>
            Virtual_dom.Vdom.Node.div(
              ~attrs=[
                WebUtil.Attr.classes([cls]),
                DecUtil.abs_style(~font_metrics, line),
              ],
              [],
            ),
        lines,
      );
    (
      model.hints
        ? highlights(~cls="step-next", deco.next_steps, i => TakeStep(i))
          @ highlights(~cls="step-refl", deco.refls, i => Refl(i))
        : []
    )
    @ lines(
        ~cls="step-acts-on",
        model.replacing ? deco.selected : deco.acts_on,
      )
    @ (model.previewing ? lines(~cls="step-previewing", deco.previewing) : []);
  };

  /* Steppers don't support probe dynamics - expressions shown are
     intermediate evaluation steps, not the main program being probed. */
  let view =
      (
        ~globals: Globals.t,
        ~inject,
        ~signal: event => 'a,
        ~overlays=[],
        ~keys: Util.Key.t => option(Ui_effect.t(unit))=_ => None,
        ~selected,
        ~_dynamics: Language.Dynamics.Map.t=Language.Dynamics.Map.empty,
        model: Model.t,
      ) => {
    CodeSelectable.View.view(
      ~expand_selection=true,
      ~keys,
      ~dynamics=Language.Dynamics.Map.empty,
      ~signal=
        fun
        | MakeActive => signal(MakeActive),
      ~edit_mode=
        EditMode.Editable({
          inject,
          escape: _ => Ui_effect.Ignore,
          take_focus: _ => Ui_effect.Ignore,
          focus: selected ? Some() : None,
        }),
      ~globals,
      ~overlays=
        overlays @ deco(~font_metrics=globals.font_metrics, signal, model),
      model.editor,
    );
  };
};
