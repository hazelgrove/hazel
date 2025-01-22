open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open Node;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: DerivationTree.spec,
    editors: DerivationTree.p(Editor.t),
    cells: DerivationTree.stitched(CellEditor.Model.t),
  };

  let of_spec = (~settings as _, ~instructor_mode as _: bool, spec) => {
    let editors =
      DerivationTree.mapi(spec, pos =>
        Editor.Model.mk(~root=DerivationTree.root_of_pos(pos))
      );
    let term_item_to_cell =
        (item: DerivationTree.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      DerivationTree.stitch_term(editors)
      |> DerivationTree.map_stitched(_ => term_item_to_cell);
    {spec, editors, cells};
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = DerivationTree.persistent_exercise_mode;

  let persist = (exercise: t, ~instructor_mode as _: bool): persistent => {
    DerivationTree.map(exercise.editors, editor =>
      editor.state.zipper |> PersistentZipper.persist
    );
  };

  let unpersist = (~instructor_mode, persistent: persistent, spec) => {
    ignore(spec);
    let spec =
      DerivationTree.mapi(persistent, pos =>
        PersistentZipper.unpersist(~root=DerivationTree.root_of_pos(pos))
      );
    of_spec(~instructor_mode, spec);
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(DerivationTree.pos, CellEditor.Update.t)
    | MapEditor(DerivationTree.eds => DerivationTree.eds)
    | ResetExercise;

  let update =
      (~settings: Settings.t, ~schedule_action as _, action, model: Model.t)
      : Updated.t(Model.t) => {
    switch (action) {
    | Editor(pos, MainEditor(action)) =>
      // Redirect to editors
      let editor =
        DerivationTree.main_editor_of_state(~selection=pos, model.editors);
      let* new_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        editor
        |> CodeEditable.Model.mk
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        editors:
          DerivationTree.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor.editor,
          ),
      };
    | Editor(pos, ResultAction(UpdateResult(_)) as action) =>
      let cell = DerivationTree.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {
        ...model,
        cells: DerivationTree.put_stitched(pos, model.cells, new_cell),
      };
    | Editor(_, ResultAction(_)) => Updated.return_quiet(model) // TODO: I think this case should never happen
    | MapEditor(f) =>
      let x = {...model, editors: f(model.editors)};
      // print_endline(
      //   "Spec: "
      //   ++ (
      //     x.spec
      //     |> DerivationTree.editor_positions
      //     |> List.map(DerivationTree.show_pos)
      //     |> String.concat(", ")
      //   )
      //   ++ "\n Editors: "
      //   ++ (
      //     x.editors
      //     |> DerivationTree.editor_positions
      //     |> List.map(DerivationTree.show_pos)
      //     |> String.concat(", ")
      //   )
      //   ++ "\n Editors sort: "
      //   ++ (
      //     x.editors
      //     |> DerivationTree.editors
      //     |> List.map((ed: Editor.t) => Sort.show(ed.root))
      //     |> String.concat(", ")
      //   )
      // ++ "\n Stitched: "
      // ++ (
      //   x.cells.trees
      //   |> List.map(
      //        Tree.mapi((pos, content) =>
      //          Tree.show_pos(pos)
      //          ++ (
      //            switch (content) {
      //            | Some(_) => ""
      //            | None => " (empty)"
      //            }
      //          )
      //        ),
      //      )
      //   |> List.map(Tree.flatten)
      //   |> List.map(String.concat(", "))
      //   |> String.concat("\n")
      // )
      // ++ "\n Stitched: "
      // ++ (
      //   x.editors.trees
      //   |> List.map(
      //        Tree.mapi((pos, content) =>
      //          Tree.show_pos(pos)
      //          ++ (
      //            switch (content) {
      //            | DerivationTree.Abbr.Just(_) => "(x)"
      //            | DerivationTree.Abbr.Abbr(i) =>
      //              "("
      //              ++ (
      //                switch (i) {
      //                | Some(i) => string_of_int(i)
      //                | None => "?"
      //                }
      //              )
      //              ++ ")"
      //            }
      //          )
      //        ),
      //      )
      //   |> List.map(Tree.flatten)
      //   |> List.map(String.concat(", "))
      //   |> String.concat("\n")
      // ),
      // ); // TODO(zhiyao): facilitate recalculation
      {
        ...x,
        cells:
          DerivationTree.stitch_term(x.editors)
          |> DerivationTree.map_stitched((_, item: DerivationTree.TermItem.t) =>
               CellEditor.Model.mk(item.editor)
             ),
      }
      |> Updated.return(~recalculate=true);
    | ResetExercise =>
      let new_editors =
        DerivationTree.mapi(model.spec, pos =>
          Editor.Model.mk(~root=DerivationTree.root_of_pos(pos))
        );
      {...model, editors: new_editors} |> Updated.return;
    };
  };

  let calculate =
      (~settings, ~is_edited, ~schedule_action, model: Model.t): Model.t => {
    let stitched_elabs = DerivationTree.stitch_term(model.editors);
    let worker_request = ref([]);
    let queue_worker = (pos, expr) => {
      worker_request :=
        worker_request^ @ [(pos |> DerivationTree.key_for_statics, expr)];
    };
    let cells: DerivationTree.stitched(CellEditor.Model.t) =
      DerivationTree.map_stitched(
        (pos, {term, editor}: DerivationTree.TermItem.t) => {
          (
            try({
              let cell = DerivationTree.get_stitched(pos, model.cells);
              {
                editor: {
                  editor,
                  statics: cell.editor.statics,
                },
                result: cell.result,
              };
            }) {
            | Not_found =>
              ""
              |> DerivationTree.zipper_of_code(~root=Drv(Exp))
              |> Editor.Model.mk(~root=Drv(Exp))
              |> CellEditor.Model.mk
            }
          )
          |> CellEditor.Update.calculate(
               ~settings,
               ~is_edited,
               ~queue_worker=Some(queue_worker(pos)),
               ~stitch=_ =>
               term
             )
        },
        stitched_elabs,
      );

    WorkerClient.request(
      worker_request^,
      ~handler=
        List.iter(((pos, result)) => {
          let pos' = DerivationTree.pos_of_key(pos);
          let result':
            Haz3lcore.ProgramResult.t(Haz3lcore.ProgramResult.inner) =
            switch (result) {
            | Ok((r, s)) => ResultOk({result: r, state: s})
            | Error(e) => ResultFail(e)
            };
          schedule_action(
            Editor(pos', ResultAction(UpdateResult(result'))),
          );
        }),
      ~timeout=_ => {
        let _ =
          DerivationTree.map_stitched(
            (pos, _) =>
              schedule_action(
                Editor(
                  pos,
                  ResultAction(UpdateResult(ResultFail(Timeout))),
                ),
              ),
            model.cells,
          );
        ();
      },
    );
    /* The following section pulls statics back from cells into the editors
       There are many ad-hoc things about this code, including the fact that
       one of the editors is shown in two cells, so we arbitrarily choose which
       statics to take */
    let editors: DerivationTree.p('a) = {
      let calculate = Editor.Update.calculate(~settings, ~is_edited);
      {
        ...model.editors,
        prelude:
          calculate(cells.prelude.editor.statics, model.editors.prelude),
        setup: calculate(cells.setup.editor.statics, model.editors.setup),
        trees: {
          List.map2(Util.Tree.combine, cells.trees, model.editors.trees)
          |> List.map(
               Util.Tree.map(
                 fun
                 | (
                     Some(di: CellEditor.Model.t),
                     DerivationTree.Abbr.Just(DerivationTree.{jdmt, rule}),
                   ) => {
                     DerivationTree.Abbr.Just(
                       DerivationTree.{
                         jdmt: calculate(di.editor.statics, jdmt),
                         rule,
                       },
                     );
                   }
                 | (None, DerivationTree.Abbr.Abbr(d)) =>
                   DerivationTree.Abbr.Abbr(d)
                 | (None, _) => failwith("derivation inconsistency1")
                 | (Some(_), _) => failwith("derivation inconsistency2"),
               ),
             );
        },
      };
    };
    {spec: model.spec, editors, cells};
  };
};

module NinjaKeysRule = {
  open Js_of_ocaml;
  open Util;
  let pos = ref(DerivationTree.Trees(0, Value));

  let init = () =>
    ""
    |> DerivationTree.zipper_of_code(~root=Drv(Exp))
    |> Editor.Model.mk(~root=Drv(Exp));

  let update_rule: Haz3lcore.RuleImage.t => Update.t =
    rule =>
      Update.MapEditor(
        DerivationTree.switch_rule(~pos=pos^, ~rule=Some(rule)),
      );

  /*
   Configuration of the rule choice palette using the https://github.com/ssleptsov/ninja-keys web component.
   */

  let from_rule =
      (schedule_action: Update.t => unit, rule: Haz3lcore.RuleImage.t)
      : {
          .
          "handler": Js.readonly_prop(unit => unit),
          "id": Js.readonly_prop(string),
          "title": Js.readonly_prop(string),
          "section": Js.readonly_prop(Js.optdef(string)),
          "keywords": Js.readonly_prop(string),
        } => {
    [%js
     {
       val id = Haz3lcore.RuleImage.show(rule);
       val title = Haz3lcore.RuleImage.show(rule);
       val section =
         Js.Optdef.option(
           Some(
             Haz3lcore.RuleImage.show_kind(
               Haz3lcore.RuleImage.of_kind(rule),
             ),
           ),
         );
       val handler = () => update_rule(rule) |> schedule_action;
       val keywords =
         Haz3lcore.RuleImage.keywords(rule) |> String.concat(" ")
     }];
  };

  let options = (schedule_action: Update.t => unit) =>
    Array.of_list(
      List.map(from_rule(schedule_action), Haz3lcore.RuleImage.all),
    );

  let elem = () => JsUtil.get_elem_by_id("ninja-keys-rules");

  let initialize = opts => Js.Unsafe.set(elem(), "data", Js.array(opts));

  let open_command_palette = (): unit =>
    Js.Unsafe.meth_call(elem(), "open", [||]);
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = (DerivationTree.pos, CellEditor.Selection.t);

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    let (pos, s) = selection;
    let pos = DerivationTree.farthest_pos(pos, model.editors);
    let cell_editor = DerivationTree.get_stitched(pos, model.cells);
    let+ a = CellEditor.Selection.get_cursor_info(~selection=s, cell_editor);
    Update.Editor(pos, a);
  };

  let handle_key_event = (~selection, ~event, model: Model.t) => {
    let (pos, s) = selection;
    let pos = DerivationTree.farthest_pos(pos, model.editors);
    let cell_editor = DerivationTree.get_stitched(pos, model.cells);
    CellEditor.Selection.handle_key_event(~selection=s, ~event, cell_editor)
    |> Option.map(a => Update.Editor(pos, a));
  };

  let jump_to_tile =
      (~settings as _: Settings.t, tile, model: Model.t)
      : option((Update.t, t)) => {
    DerivationTree.positioned_editors(model.editors)
    |> List.find_opt(((_, e: Editor.t)) =>
         TileMap.find_opt(tile, e.syntax.tiles) != None
       )
    |> Option.map(((pos, _)) =>
         (
           Update.Editor(pos, MainEditor(Perform(Jump(TileId(tile))))),
           (pos, CellEditor.Selection.MainEditor),
         )
       );
  };
};

let stitched_results =
  DerivationTree.map_stitched((_, cell_editor: CellEditor.Model.t) =>
    switch (cell_editor.result.result) {
    | Evaluation({result: OldValue(ResultOk((exp, _))), _})
    | Evaluation({result: NewValue(ResultOk((exp, _))), _}) => Some(exp)
    | Stepper(s) => StepperView.Model.get_elaboration(s)
    | _ => None // TODO(zhiyao): handle other cases
    }
  );

let grading_report = (model: Model.t) =>
  DrvGrading.GradingReport.mk(
    model.editors,
    ~stitched_results=stitched_results(model.cells),
  );

// ====== Exercise ======

module View = {
  module FakeCode = {
    let token_wrapper = (cls, s) =>
      span(~attrs=[Attr.class_(cls)], [text(s)]);
    let span_exp = token_wrapper("token default Exp poly");
    let span_var = token_wrapper("token default Exp mono");
    let span_pat = token_wrapper("token default Pat mono");
    let span_secondary = token_wrapper("secondary");
    let span_explicit_hole = token_wrapper("token explicit-hole Exp mono");
    let code_wrapper = code =>
      div(
        ~attrs=[Attr.class_("code fakecode")],
        [span(~attrs=[Attr.class_("code-text")], code)],
      );
  };

  type view_info = (DerivationTree.pos, DrvGrading.VerifiedTree.info, ed)
  and ed =
    | Just(option(RuleImage.t), CellEditor.Model.t)
    | Abbr(option(int));

  type event =
    | MakeActive(Selection.t);

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => 'b,
        ~inject: Update.t => 'b,
        ~selection: option(Selection.t),
        model: Model.t,
      ) => {
    let eds = model.editors;
    let {prelude, setup, trees}: DerivationTree.stitched('a) = model.cells;

    let grading_report = grading_report(model);

    let title_view = CellCommon.title_cell(eds.title);

    let prompt_view =
      CellCommon.narrative_cell(
        div(~attrs=[Attr.class_("cell-prompt")], [text(eds.prompt)]),
      );

    // let make_pos = (pos: DerivationTree.pos, index): DerivationTree.pos =>
    //   switch (pos) {
    //   | Trees(i, pos) =>
    //     Trees(i, Tree.pos_concat(Children(index, Value), pos))
    //   | _ => Prelude
    //   };

    let add_premise_btn_view = (~pos: DerivationTree.pos, ~index: int) =>
      div(
        ~attrs=[
          Attr.class_("add-premise-btn"),
          Attr.on_click(_ =>
            inject(
              MapEditor(
                DerivationTree.add_premise(~pos, ~index),
                // |> (m => {...m, pos: make_pos(pos, index)}),
              ),
            )
          ),
          // Attr.on_drop(drag_event => {
          //   let grabbed_pos =
          //     drag_event##.dataTransfer##getData(Js.string("pos"));
          //   print_endline(Js.to_string(grabbed_pos));
          //   Ui_effect.Ignore;
          // switch (grabbed_pos) {
          // | None => Ui_effect.Ignore
          // | Some(pos) =>
          //   inject(
          //     UpdateAction.MapExercise(
          //       map_model(add_premise(~pos, ~index)),
          //     ),
          //   )
          // }
          // }),
        ],
        [],
      );

    let del_premise_btn_view = (~pos: DerivationTree.pos) =>
      Widgets.button_named(
        Icons.trash,
        _ => inject(MapEditor(DerivationTree.del_premise(~pos))),
        ~tooltip=
          switch (pos) {
          | Trees(_, Value) => "Delete Abbr"
          | _ => "Delete Premise"
          },
      );

    let pop_premise_btn_view = (~pos: DerivationTree.pos) =>
      Widgets.button_named(
        Icons.export,
        _ => inject(MapEditor(DerivationTree.pop_premise(~pos))),
        ~tooltip="Pop out to Abbr",
      );

    let push_premise_btn_view = (~pos: DerivationTree.pos) =>
      Widgets.button_named(
        Icons.import,
        _ => inject(MapEditor(DerivationTree.push_premise(~pos))),
        ~tooltip="Push back Abbr",
      );

    let rule_to_label =
      fun
      | Some(rule) => RuleImage.repr(rule)
      | None => "?";

    let abbr_to_label = index =>
      FakeCode.code_wrapper([
        switch (index) {
        | Some(index) => FakeCode.span_var("d" ++ string_of_int(index))
        | None => FakeCode.span_explicit_hole("?")
        },
      ]);

    let dropdown_option_abbr_view =
        (~pos: DerivationTree.pos, ~index: option(int)) =>
      switch (index) {
      | Some(index) =>
        Widgets.button_named(
          abbr_to_label(Some(index)),
          _ =>
            inject(
              MapEditor(
                DerivationTree.switch_abbr(~pos, ~index=Some(index)),
              ),
            ),
          ~tooltip="Use Abbr d" ++ string_of_int(index),
        )
      | None => Node.none
      };

    let dropdown_switch_rule_view = (~pos: DerivationTree.pos) =>
      Widgets.button_named(
        Icons.command_palette_sparkle,
        _ => {
          NinjaKeysRule.pos := pos;
          NinjaKeysRule.open_command_palette();
          Effect.Ignore;
        },
        // Attr.draggable(true),
        // Attr.on_dragstart(drag_event => {
        //   print_endline("drag_start");
        //   drag_event##.dataTransfer##setData(
        //     Js.string("pos"),
        //     Js.string(show_pos(pos)),
        //   );
        //   Ui_effect.Ignore;
        // }),
        // Attr.on_mousemove(_ => label_on_mouseover(~pos))
        ~tooltip="Switch Rule",
      );

    let dropdown_switch_just_view = (~pos: DerivationTree.pos) =>
      Widgets.button_named(
        Icons.forward,
        _ =>
          inject(MapEditor(DerivationTree.switch_rule(~pos, ~rule=None))),
        ~tooltip="Cancel Abbr",
      );

    let class_of_result = ({res, _}: DrvGrading.VerifiedTree.info) =>
      switch (res) {
      | Incorrect(_) => "incorrect"
      | Correct => "correct"
      | Pending(_) => "pending"
      };

    let pos_is_value =
      fun
      | DerivationTree.Trees(_, Value) => true
      | _ => false;

    let dropdown_view = (~pos, ~res, ~index): t =>
      div(
        ~attrs=[
          Attr.class_("dropdown"),
          Attr.class_(class_of_result(res)),
        ],
        (
          DerivationTree.all_abbrs(pos)
          |> List.filter(abbr => abbr != index)
          |> List.filter(_ => !pos_is_value(pos))
          |> List.map(dropdown_option_abbr_view(~pos, ~index=_))
        )
        @ (
          switch (index) {
          | Some(_) => [dropdown_switch_just_view(~pos)]
          | None => []
          }
        )
        @ (
          switch (index) {
          | Some(_) when !pos_is_value(pos) => [push_premise_btn_view(~pos)]
          | Some(_) => []
          | None
              when
                !pos_is_value(pos)
                || pos == Trees(List.length(eds.trees) - 1, Value) => [
              pop_premise_btn_view(~pos),
            ]
          | None => []
          }
        )
        @ (
          !globals.settings.instructor_mode
          && pos == Trees(List.length(eds.trees) - 1, Value)
            ? [] : [del_premise_btn_view(~pos)]
        )
        @ [dropdown_switch_rule_view(~pos)],
      );

    let label_view = (~res, ~label) =>
      div(
        ~attrs=[
          Attr.class_("deduction-label"),
          Attr.class_(class_of_result(res)),
        ],
        [text(label)],
      );

    let result_btn_view = (~res: DrvGrading.VerifiedTree.info) => {
      let status =
        switch (res.res) {
        | Correct => "Pass"
        | Incorrect(_) => "Fail"
        | Pending(_) => "Indet"
        };
      div(~attrs=[Attr.classes(["test-result", status])], []);
    };

    let label_view = (~pos, ~res, ~label, ~index) =>
      div(
        ~attrs=[Attr.class_("deduction-label-wrapper")],
        [label_view(~res, ~label), dropdown_view(~pos, ~res, ~index)],
      );

    let premises_view = (~children_node, ~pos, ~res, ~rule) => {
      let label = rule_to_label(rule);
      div(
        ~attrs=[
          Attr.class_("deduction-prems-label"),
          Attr.class_(class_of_result(res)),
        ],
        [
          div(
            ~attrs=[Attr.class_("deduction-prems")],
            (
              children_node
              |> List.mapi((i, t) =>
                   div(
                     ~attrs=[Attr.class_("deduction-just-wrapper")],
                     [add_premise_btn_view(~pos, ~index=i), t],
                   )
                 )
            )
            @ [
              div(
                ~attrs=[Attr.class_("deduction-just-wrapper")],
                [
                  add_premise_btn_view(
                    ~pos,
                    ~index=List.length(children_node),
                  ),
                ],
              ),
            ],
          ),
        ]
        @ [
          label_view(~pos, ~res, ~label, ~index=None),
          result_btn_view(~res),
        ],
      );
    };

    let editor_view =
        (
          ~caption: option(string)=?,
          ~subcaption: option(string)=?,
          ~result_kind=EvalResult.View.NoResults,
          ~sort: Sort.t,
          this_pos: DerivationTree.pos,
          cell: CellEditor.Model.t,
        ) => {
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive((this_pos, a))),
        ~selected=
          switch (selection) {
          | Some((pos, s)) when pos == this_pos => Some(s)
          | _ => None
          },
        ~inject=a => inject(Editor(this_pos, a)),
        ~result_kind,
        ~caption=
          switch (caption) {
          | Some(c) => CellCommon.caption(c, ~rest=?subcaption)
          | None => None
          }, // TODO(zhiyao): refactor caption
        ~sort,
        cell,
      );
    };

    let prelude_view =
      editor_view(
        Prelude,
        prelude,
        ~subcaption=globals.settings.instructor_mode ? "" : " (Read-Only)",
        ~caption="Prelude",
        ~sort=Exp,
      );

    let setup_view = editor_view(Setup, setup, ~caption="Setup", ~sort=Exp);

    // let editor_view =
    //     (
    //       this_pos,
    //       ~editor,
    //       ~di: Exp.t,
    //       ~caption,
    //       ~footer,
    //       ~sort,
    //     ) =>
    //   Cell.editor_view(
    //     ~selected=(Proof(pos): Exercise.pos) == this_pos,
    //     ~override_statics=di.statics,
    //     ~inject,
    //     ~ui_state,
    //     ~mousedown_updates=[SwitchEditor(this_pos)],
    //     ~settings,
    //     ~highlights,
    //     ~caption,
    //     ~target_id=Exercise.show_pos(this_pos),
    //     ~test_results=ModelResult.test_results(di.result),
    //     ~footer,
    //     ~sort,
    //     editor,
    //   );

    let conclusion_view = (~pos, ~editor) =>
      div(
        ~attrs=[Attr.class_("deduction-concl")],
        [editor_view(pos, editor, ~sort=Drv(Exp))],
      );

    let deduction_view = (~children_node, ~pos, ~res, ~rule, ~editor) =>
      div(
        ~attrs=
          [Attr.class_("deduction-just")]
          @ (
            switch (selection) {
            | Some((pos', _)) when pos == pos' => [Attr.class_("staged")]
            | _ => []
            }
          ),
        [
          premises_view(~children_node, ~pos, ~res, ~rule),
          conclusion_view(~pos, ~editor),
        ],
      );

    // TODO: Refactor this
    let abbreviation_view = (~pos, ~res, ~index) =>
      div(
        ~attrs=[Attr.class_("deduction-abbr")],
        [
          div(
            ~attrs=[
              Attr.class_("deduction-prems"),
              Attr.class_(class_of_result(res)),
            ],
            [label_view(~pos, ~res, ~label="•", ~index)],
          ),
          div(
            ~attrs=[Attr.class_("deduction-concl")],
            [abbr_to_label(index)],
          ),
        ],
      );

    let deduction_view = ((pos, res, ed): view_info, children_node: list(t)) =>
      switch (ed) {
      | Just(rule, editor) =>
        deduction_view(~children_node, ~pos, ~res, ~rule, ~editor)
      | Abbr(index) => abbreviation_view(~pos, ~res, ~index)
      };

    let abbr_wrapper = (i, t) => {
      open FakeCode;
      let upper_code =
        [
          span_exp("let"),
          span_secondary(" "),
          span_pat("d" ++ string_of_int(i)),
          span_secondary(" "),
          span_exp("="),
        ]
        |> code_wrapper;
      let lower_code = [span_exp("in")] |> code_wrapper;
      if (i == List.length(eds.trees) - 1) {
        t;
      } else {
        div(
          ~attrs=[Attr.class_("abbr-wrapper")],
          [upper_code, t, lower_code],
        );
      };
    };

    let add_abbr_btn_view = (~index) =>
      div(
        ~attrs=[
          Attr.class_("add-abbr-btn"),
          Attr.on_click(_ =>
            inject(
              MapEditor(
                DerivationTree.add_abbr(~index),
                // |> (m => {...m, pos: Proof(Trees(index, Value))}),
              ),
            )
          ),
        ],
        [
          (
            if (index == List.length(eds.trees)) {
              [];
            } else {
              [FakeCode.span_exp("let"), FakeCode.span_secondary(" ")];
            }
          )
          @ [FakeCode.span_pat("...")]
          |> FakeCode.code_wrapper,
        ],
      );

    // type view_info = (Exercise.pos, VerifiedTree.res, ed)
    // and ed =
    //   | Just(Derivation.Rule.t, Editor.t, Exercise.DynamicsItem.t)
    //   | Abbr(index);

    let info_tree =
      List.map2(Tree.combine, eds.trees, trees)
      |> List.map(
           Tree.map(
             fun
             | (
                 DerivationTree.Abbr.Just(DerivationTree.{rule, _}),
                 Some(di),
               ) => (
                 Just(rule, di): ed
               )
             | (Abbr(i), _) => Abbr(i)
             | _ => raise(Failure("DerivationTree.mk: ed<>di inconsistent")),
           ),
         )
      |> List.map2(Tree.combine, grading_report.proof_report.verified_tree)
      |> List.mapi(i =>
           Tree.mapi((pos, (res, ed)) =>
             (DerivationTree.Trees(i, pos), res, ed)
           )
         );

    let derivation_view = (i, info_single) =>
      div(
        ~attrs=[Attr.class_("cell-derivation")],
        [add_abbr_btn_view(~index=i)]
        @ [info_single |> Tree.fold_deep(deduction_view) |> abbr_wrapper(i)],
      );

    let derivations_view =
      div(
        ~attrs=[Attr.classes(["cell-item derivation-panel"])],
        (info_tree |> List.mapi(derivation_view))
        @ (
          if (globals.settings.instructor_mode) {
            [
              div(
                ~attrs=[Attr.class_("cell-derivation")],
                [add_abbr_btn_view(~index=List.length(eds.trees))],
              ),
            ];
          } else {
            [];
          }
        ),
      );

    let option_view = (name, n) =>
      option(
        ~attrs=n == name ? [Attr.create("selected", "selected")] : [],
        [text(n)],
      );

    let version_view =
      div(
        ~attrs=[Attr.class_("version-name"), Attr.title("Toggle Version")],
        [
          div(~attrs=[Attr.class_("version-label")], [text("Version: ")]),
          text(Unicode.nbsp),
          text(RuleImage.show_version(eds.ruleset)),
          text(Unicode.nbsp),
          select(
            ~attrs=[
              Attr.class_("version-select"),
              Attr.on_change((_, name) =>
                inject(
                  MapEditor(
                    m => {...m, ruleset: RuleImage.version_of_string(name)},
                  ),
                )
              ),
            ],
            List.map(
              option_view(RuleImage.show_version(eds.ruleset)),
              RuleImage.all_of_version |> List.map(RuleImage.show_version),
            ),
          ),
        ],
      );

    [
      title_view,
      prompt_view,
      version_view,
      prelude_view,
      setup_view,
      derivations_view,
    ];
  };
};
