open Util;
open Haz3lcore;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    editor: CodeSelectable.Model.t,
    /* Where the offered steps' and reflexivities' underlines go. */
    next_steps: list(Id.t),
    refls: list(Id.t),
    /* The part the step below acts on. */
    acts_on: option(Id.t),
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

  /* Underlines on the offered steps and reflexivities. A click on one
   * selects it, and a click on the selected one takes it. */
  let deco =
      (
        ~syntax: CachedSyntax.t,
        ~font_metrics: FontMetrics.t,
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected_id: option(Id.t),
        signal: event => Ui_effect.t(unit),
        model: Model.t,
      ) => {
    let underlines = (~cls, ids, take: int => event) =>
      ids
      |> List.mapi((i, id) => (i, id))
      |> List.filter_map(((i, id)) => {
           open OptUtil.Syntax;
           let* tile = TermData.root_tile(id, syntax.term_data);
           let+ range =
             TermData.extreme_measures(
               id,
               syntax.term_data,
               CachedSyntax.measured(syntax),
             );
           let click =
             Some(id) == selected_id
               ? signal(take(i)) : inject(Select(PointToPoint(range)));
           WebUtil.div_c(
             cls,
             Arms.term(
               ~attr=[WebUtil.Attr.on_mousedown(_ => click)],
               ~font_metrics,
               ~syntax,
               tile,
             ),
           );
         });
    /* A plain line under each row the term covers, with no click: it
     * marks where the step below acts, which isn't a step to take. */
    let acts_on =
      switch (
        Option.bind(model.acts_on, id =>
          TermData.extreme_measures(
            id,
            syntax.term_data,
            CachedSyntax.measured(syntax),
          )
        )
      ) {
      | None => []
      | Some((origin, last)) =>
        let rows = CachedSyntax.measured(syntax).rows;
        ListUtil.range(~lo=origin.row, last.row + 1)
        |> List.map(row => {
             let shape = Measured.Rows.find(row, rows);
             let col_from =
               row == origin.row ? origin.col : shape.content_start;
             let col_to = row == last.row ? last.col : shape.content_end;
             Virtual_dom.Vdom.Node.div(
               ~attrs=[
                 WebUtil.Attr.classes(["step-acts-on"]),
                 DecUtil.abs_style(
                   ~font_metrics,
                   {
                     origin: {
                       row,
                       col: col_from,
                     },
                     last: {
                       row,
                       col: col_to,
                     },
                   },
                 ),
               ],
               [],
             );
           });
      };
    underlines(~cls="step-next", model.next_steps, i => TakeStep(i))
    @ underlines(~cls="step-refl", model.refls, i => Refl(i))
    @ acts_on;
  };

  /* Steppers don't support probe dynamics - expressions shown are
     intermediate evaluation steps, not the main program being probed. */
  let view =
      (
        ~globals: Globals.t,
        ~inject,
        ~signal: event => 'a,
        ~overlays=[],
        ~selected,
        ~selected_id,
        ~_dynamics: Language.Dynamics.Map.t=Language.Dynamics.Map.empty,
        model: Model.t,
      ) => {
    CodeSelectable.View.view(
      ~expand_selection=true,
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
        overlays
        @ deco(
            ~syntax=model.editor.editor.syntax,
            ~font_metrics=globals.font_metrics,
            ~inject,
            ~selected_id,
            signal,
            model,
          ),
      model.editor,
    );
  };
};
