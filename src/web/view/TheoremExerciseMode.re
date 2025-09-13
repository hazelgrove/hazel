open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;

/* The exercises mode interface for a theorem exercise. Composed of multiple editors and results. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type cells = {
    // prelude: CellEditor.Model.t,
    // lemmas: CellEditor.Model.t,
    theorem: CellEditor.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    title: string,
    prompt: string,
    cells,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    // lemmas: CellEditor.Model.persistent,
    theorem: EvalResult.Model.persistent,
  };

  let persist = (model: t): persistent => {
    // lemmas: model.cells.lemmas |> CellEditor.Model.persist,
    theorem: model.cells.theorem.result |> EvalResult.Model.persist,
  };

  let unpersist =
      (~settings as _, spec: TheoremExerciseSpec.t, persistent: persistent): t => {
    {
      id: spec.id,
      title: spec.title,
      prompt: spec.prompt,
      cells: {
        // prelude: CellEditor.Model.mk(Editor.Model.mk(spec.prelude)),
        // lemmas: persistent.lemmas |> CellEditor.Model.unpersist(~settings),
        theorem: {
          editor: CellEditor.Model.mk(Editor.Model.mk(spec.theorem)).editor,
          result: persistent.theorem |> EvalResult.Model.unpersist,
        },
      },
    };
  };

  let of_spec = (spec: TheoremExerciseSpec.t): t => {
    {
      id: spec.id,
      title: spec.title,
      prompt: spec.prompt,
      cells: {
        // prelude: CellEditor.Model.mk(Editor.Model.mk(spec.prelude)),
        // lemmas: CellEditor.Model.mk(Editor.Model.mk(spec.lemmas)),
        theorem: CellEditor.Model.mk(Editor.Model.mk(spec.theorem)),
      },
    };
  };

  let spec_of_t = (model: t): TheoremExerciseSpec.t => {
    {
      id: model.id,
      title: model.title,
      prompt: model.prompt,
      // prelude: model.cells.prelude.editor.state.zipper, --- IGNORE ---
      // lemmas: model.cells.lemmas.editor.state.zipper, --- IGNORE ---
      theorem: model.cells.theorem.editor.editor.state.zipper,
    };
  };

  let export_module = (model: t): string => {
    let prefix = "let exercise : TheoremExerciseSpec.t =
 \n";
    let spec = spec_of_t(model);
    prefix ++ TheoremExerciseSpec.show(spec) ++ "\n";
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | UpdateTitle(string)
    | UpdatePrompt(string)
    // | Prelude(CellEditor.Update.t)
    // | Lemmas(CellEditor.Update.t)
    | Theorem(CellEditor.Update.t);

  let update = (~settings: Settings.t, action: t, model: Model.t) => {
    switch (action) {
    | UpdateTitle(new_title) when settings.instructor_mode =>
      Updated.return({
        ...model,
        title: new_title,
      })
    | UpdateTitle(_) =>
      print_endline("Instructor-only action");
      Updated.return_quiet(model);
    | UpdatePrompt(prompt) when settings.instructor_mode =>
      Updated.return({
        ...model,
        prompt,
      })
    | UpdatePrompt(_) =>
      print_endline("Instructor-only action");
      Updated.return_quiet(model);
    // | Prelude(action) when settings.instructor_mode =>
    //   let* new_cell =
    //     CellEditor.Update.update(~settings, action, model.cells.prelude);
    //   {
    //     ...model,
    //     cells: {
    //       ...model.cells,
    //       prelude: new_cell,
    //     },
    //   };
    // | Prelude(MainEditor(action))
    //     when CodeSelectable.Update.convert_action(action) != None =>
    //   let* new_cell =
    //     CellEditor.Update.update(
    //       ~settings,
    //       MainEditor(action),
    //       model.cells.prelude,
    //     );
    //   {
    //     ...model,
    //     cells: {
    //       ...model.cells,
    //       prelude: new_cell,
    //     },
    //   };
    // | Prelude(_) =>
    //   print_endline("Instructor-only action");
    //   Updated.return_quiet(model);
    // | Lemmas(action) =>
    //   let* new_cell =
    //     CellEditor.Update.update(~settings, action, model.cells.lemmas);
    //   {
    //     ...model,
    //     cells: {
    //       ...model.cells,
    //       lemmas: new_cell,
    //     },
    //   };
    | Theorem(action) when settings.instructor_mode =>
      let* new_cell =
        CellEditor.Update.update(~settings, action, model.cells.theorem);
      {
        ...model,
        cells: {
          // ...model.cells,
          theorem: new_cell,
        },
      };
    | Theorem(MainEditor(action))
        when CodeSelectable.Update.convert_action(action) != None =>
      let* new_cell =
        CellEditor.Update.update(
          ~settings,
          MainEditor(action),
          model.cells.theorem,
        );
      {
        ...model,
        cells: {
          // ...model.cells,
          theorem: new_cell,
        },
      };
    | Theorem(ResultAction(action)) =>
      let* new_cell =
        CellEditor.Update.update(
          ~settings,
          ResultAction(action),
          model.cells.theorem,
        );
      {
        ...model,
        cells: {
          // ...model.cells,
          theorem: new_cell,
        },
      };
    | Theorem(MainEditor(_)) =>
      print_endline("Instructor-only action");
      Updated.return_quiet(model);
    };
  };

  let can_undo = (action: t): bool => {
    switch (action) {
    | UpdateTitle(_) => true
    | UpdatePrompt(_) => true
    // | Prelude(action) => CellEditor.Update.can_undo(action)
    // | Lemmas(action) => CellEditor.Update.can_undo(action)
    | Theorem(action) => CellEditor.Update.can_undo(action)
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    // Work out the terms
    // let just_prelude_term =
    //   MakeTerm.from_zip_for_sem(
    //     model.cells.prelude.editor.editor.state.zipper,
    //   ).
    //     term;
    // let just_lemmas_term =
    //   MakeTerm.from_zip_for_sem(model.cells.lemmas.editor.editor.state.zipper).
    //     term;
    // let just_theorem_term =
    //   MakeTerm.from_zip_for_sem(
    //     model.cells.theorem.editor.editor.state.zipper,
    //   ).
    //     term;

    // let stitched_scratch =
    //   Exercise.append_exp(just_prelude_term, just_lemmas_term);
    // let stitched_theorem =
    //   Exercise.append_exp(
    //     stitched_scratch,
    //     Language.Exp.replace_all_ids(just_prelude_term),
    //   )
    //   |> Exercise.append_exp(_, just_theorem_term);

    // Worker Setup
    let worker_request: ref(list((string, Language.Exp.t))) = ref([]);
    let queue_worker = (pos, expr) => {
      worker_request := worker_request^ @ [(pos, expr)];
    };

    // Calculate each cell
    let cells: Model.cells =
      Model.{
        // prelude:
        //   model.cells.prelude
        //   |> CellEditor.Update.calculate(
        //        ~settings,
        //        ~is_edited,
        //        ~queue_worker=Some(queue_worker("prelude")),
        //        ~stitch=_ =>
        //        just_prelude_term
        //      ),
        // lemmas:
        //   model.cells.lemmas
        //   |> CellEditor.Update.calculate(
        //        ~settings,
        //        ~is_edited,
        //        ~queue_worker=Some(queue_worker("lemmas")),
        //        ~stitch=_ =>
        //        stitched_scratch
        //      ),
        theorem:
          model.cells.theorem
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~queue_worker=Some(queue_worker("theorem")),
               ~stitch=x =>
               x
             ) //  stitched_theorem
      };

    // Send to worker

    WorkerClient.request(
      worker_request^,
      ~handler=
        List.iter(((pos, result)) => {
          let result': Language.ProgramResult.t(Language.ProgramResult.inner) =
            switch (result) {
            | Ok((r, s)) =>
              ResultOk({
                result: r,
                state: s,
              })
            | Error(e) => ResultFail(e)
            };
          switch (pos) {
          // | "lemmas" =>
          //   schedule_action(Prelude(ResultAction(UpdateResult(result'))));
          //   schedule_action(Lemmas(ResultAction(UpdateResult(result'))));
          | "theorem" =>
            schedule_action(Theorem(ResultAction(UpdateResult(result'))))
          | _ => ()
          };
        }),
      ~timeout=_ => {
      List.iter(
        fun
        // | "lemmas" => {
        //     schedule_action(
        //       Prelude(ResultAction(UpdateResult(ResultFail(Timeout)))),
        //     );
        //     schedule_action(
        //       Lemmas(ResultAction(UpdateResult(ResultFail(Timeout)))),
        //     );
        //   }
        | "theorem" =>
          schedule_action(
            Theorem(ResultAction(UpdateResult(ResultFail(Timeout)))),
          )
        | _ => (),
        List.map(((pos, _)) => pos, worker_request^),
      )
    });

    {
      ...model,
      cells,
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TextBox
    // | Prelude(CellEditor.Selection.t)
    // | Lemmas(CellEditor.Selection.t)
    | Theorem(CellEditor.Selection.t);

  let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
    switch (selection) {
    | TextBox => Cursor.empty
    // | Prelude(s) =>
    //   let+ a =
    //     CellEditor.Selection.get_cursor_info(
    //       ~selection=s,
    //       model.cells.prelude,
    //     );
    //   Update.Prelude(a);
    // | Lemmas(s) =>
    //   let+ a =
    //     CellEditor.Selection.get_cursor_info(
    //       ~selection=s,
    //       model.cells.lemmas,
    //     );
    // Update.Lemmas(a);
    | Theorem(s) =>
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~selection=s,
          model.cells.theorem,
        );
      Update.Theorem(a);
    };
  };

  let handle_key_event =
      (~selection: t, ~event, model: Model.t): option(Update.t) => {
    switch (selection) {
    | TextBox => None
    // | Prelude(s) =>
    //   CellEditor.Selection.handle_key_event(
    //     ~selection=s,
    //     model.cells.prelude,
    //     ~event,
    //   )
    //   |> Option.map(x => Update.Prelude(x))
    // | Lemmas(s) =>
    //   CellEditor.Selection.handle_key_event(
    //     ~selection=s,
    //     model.cells.lemmas,
    //     ~event,
    //   )
    //   |> Option.map(x => Update.Lemmas(x))
    | Theorem(s) =>
      CellEditor.Selection.handle_key_event(
        ~selection=s,
        model.cells.theorem,
        ~event,
      )
      |> Option.map(x => Update.Theorem(x))
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    open OptUtil.Syntax;
    // let (let.or) = (v: option('b), f: unit => option('b)) => {
    //   switch (v) {
    //   | Some(x) => Some(x)
    //   | None => f()
    //   };
    // };

    // let.or () = {
    //   let* _ =
    //     TermData.root_tile(
    //       tile,
    //       model.cells.prelude.editor.editor.syntax.term_data,
    //     );
    //   Some((
    //     Update.Prelude(MainEditor(Perform(Move(Goal(TileId(tile)))))),
    //     Prelude(CellEditor.Selection.MainEditor),
    //   ));
    // };
    // let.or () = {
    //   let* _ =
    //     TermData.root_tile(
    //       tile,
    //       model.cells.lemmas.editor.editor.syntax.term_data,
    //     );
    //   Some((
    //     Update.Lemmas(MainEditor(Perform(Move(Goal(TileId(tile)))))),
    //     Lemmas(CellEditor.Selection.MainEditor),
    //   ));
    // };

    let* _ =
      TermData.root_tile(
        tile,
        model.cells.theorem.editor.editor.syntax.term_data,
      );
    Some((
      Update.Theorem(MainEditor(Perform(Move(Goal(TileId(tile)))))),
      Theorem(CellEditor.Selection.MainEditor),
    ));
  };
};

module View = {
  let view =
      (
        ~globals: Globals.t,
        ~take_focus: Selection.t => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selection: option(Selection.t),
        model: Model.t,
      ) => {
    let title_view =
      CellCommon.simple_cell_view([
        div(
          ~attrs=[Attr.class_("title-cell")],
          [
            globals.settings.instructor_mode
              ? div(
                  ~attrs=[Attr.class_("title-edit")],
                  [
                    input(
                      ~attrs=[
                        Attr.class_("title-input"),
                        Attr.value(model.title),
                        Attr.on_change((_, str) =>
                          inject(UpdateTitle(str))
                        ),
                        Attr.on_focus(_ => take_focus(TextBox)),
                      ],
                      (),
                    ),
                  ],
                )
              : div(
                  ~attrs=[Attr.class_("title-text")],
                  [text(model.title)],
                ),
          ],
        ),
      ]);

    let prompt_view =
      CellCommon.simple_cell_view([
        globals.settings.instructor_mode
          ? textarea(
              ~attrs=[
                Attr.class_("prompt-input"),
                Attr.on_change((_, str) => inject(UpdatePrompt(str))),
                Attr.on_focus(_ => take_focus(TextBox)),
              ],
              [text(model.prompt)],
            )
          : div(~attrs=[Attr.class_("prompt-text")], [text(model.prompt)]),
      ]);

    // let prelude_view =
    //   CellEditor.View.view(
    //     ~globals,
    //     ~signal=
    //       fun
    //       | MakeActive(a) => take_focus(Prelude(a)),
    //     ~selected=
    //       switch (selection) {
    //       | Some(Prelude(s)) => Some(s)
    //       | _ => None
    //       },
    //     ~inject=a => inject(Prelude(a)),
    //     ~result_kind=`NoResults,
    //     ~caption=CellCommon.caption("Prelude (Read-Only)"),
    //     model.cells.prelude,
    //   );

    // let lemmas_view =
    //   CellEditor.View.view(
    //     ~globals,
    //     ~signal=
    //       fun
    //       | MakeActive(a) => take_focus(Lemmas(a)),
    //     ~selected=
    //       switch (selection) {
    //       | Some(Lemmas(s)) => Some(s)
    //       | _ => None
    //       },
    //     ~inject=a => inject(Lemmas(a)),
    //     ~result_kind=`NoTheorems,
    //     ~caption=CellCommon.caption("Lemmas / Scratch Space"),
    //     model.cells.lemmas,
    //   );

    let theorem_view =
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => take_focus(Theorem(a)),
        ~selected=
          switch (selection) {
          | Some(Theorem(s)) => Some(s)
          | _ => None
          },
        ~inject=a => inject(Theorem(a)),
        ~result_kind=`JustTheorems,
        ~caption=CellCommon.caption("Theorem (Prove-Only)"),
        model.cells.theorem,
      );

    let score_view =
      Grading.score_view(
        Theorems.Model.get_score(model.cells.theorem.result.theorems)
        |> Option.value(~default=(Float.nan, Float.nan)),
      );

    [
      score_view,
      title_view,
      prompt_view,
      // prelude_view, lemmas_view,
      theorem_view,
    ];
  };
};
