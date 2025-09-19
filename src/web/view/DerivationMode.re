open Haz3lcore;
open Virtual_dom.Vdom;
open Util;
open Language;
open Node;

let stitched_results =
  DerivationTree.map_stitched((_, cell_editor: CellEditor.Model.t) =>
    cell_editor.result.result
    |> Calc.save
    |> Calc.get_saved_opt
    |> (
      fun
      | Some(ProgramResult.ResultOk(r)) => Some(r.result)
      | Some(ResultFail(_))
      | Some(ResultPending)
      | None => None
    )
  );

let verified_tree =
    (
      editors: DerivationTree.p(Editor.t),
      cells: DerivationTree.stitched(CellEditor.Model.t),
    ) =>
  DrvGrading.VerifiedTree.mk(
    editors,
    ~stitched_results=stitched_results(cells),
  );

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: DerivationTree.spec,
    editors: DerivationTree.p(Editor.t),
    cells: DerivationTree.stitched(CellEditor.Model.t),
    verified_tree: DrvGrading.VerifiedTree.t,
    pos: DerivationTree.pos,
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
    let verified_tree = verified_tree(editors, cells);
    let pos = DerivationTree.Prelude;
    {
      pos,
      spec,
      editors,
      cells,
      verified_tree,
    };
  };

  let is_editable =
      (~instructor_mode, pos: DerivationTree.pos, model: t): bool => {
    switch (pos) {
    | Prelude => instructor_mode
    | Setup => true
    | Trees(i, Value) when i + 1 == List.length(model.editors.trees) => instructor_mode
    | Trees(_) => true
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = DerivationTree.persistent_state;

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

  let get_derivation_info = (model: t) => {
    let trees = model.verified_tree;
    let eds = model.editors;
    switch (model.pos) {
    | Trees(i, pos) =>
      try({
        let tree = List.nth(trees, i);
        let res = Tree.nth(tree, pos);
        let tree = List.nth(eds.trees, i);
        let ed = Tree.nth(tree, pos);
        switch (ed, res) {
        | (Just({rule: Some(rule), _}), {rule: None, _}) =>
          Language.(
            switch (RuleImage.to_rule(eds.corpus, rule)) {
            | Some(rule) =>
              Some({
                ...res,
                rule:
                  Some(
                    {
                      print_endline("Uncaught Rule: " ++ Rule.show(rule));
                      let spec = RuleSpec.of_spec(rule);
                      {
                        // TODO(zhiyao): may not bring it back now
                        // let (spec, tests) =
                        //   RuleVerify.fill_eq_tests(spec, tests);
                        // let tests = RuleVerify.test_remove_eq_test(tests);
                        rule,
                        spec,
                      };
                    },
                  ),
              })
            | _ => Some(res)
            }
          )
        | _ => Some(res)
        };
      }) {
      | _ => None
      }
    | _ => None
    };
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(DerivationTree.pos, CellEditor.Update.t)
    | MapEditor(DerivationTree.eds => DerivationTree.eds)
    | Refresh
    | ResetExercise;

  let can_undo = (action: t) => {
    switch (action) {
    | Editor(_, action) => CellEditor.Update.can_undo(action)
    | MapEditor(_) => true
    | Refresh => false
    | ResetExercise => false
    };
  };

  let update_editor_action =
      (
        action: CodeEditable.Update.t,
        pos: DerivationTree.pos,
        model: Model.t,
        settings,
      ) => {
    let editor =
      DerivationTree.main_editor_of_state(~selection=pos, model.editors);
    let* new_editor =
      // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
      editor
      |> CodeEditable.Model.mk
      |> CodeEditable.Update.update(~settings, action);
    {
      ...model,
      pos,
      editors:
        DerivationTree.put_main_editor(
          ~selection=pos,
          model.editors,
          new_editor.editor,
        ),
    };
  };

  let update =
      (~settings: Settings.t, ~schedule_action as _, action, model: Model.t)
      : Updated.t(Model.t) => {
    let instructor_mode = settings.instructor_mode;
    switch (action) {
    | Editor(pos, MainEditor(action))
        when Model.is_editable(pos, ~instructor_mode, model) =>
      let editor =
        DerivationTree.main_editor_of_state(~selection=pos, model.editors);
      let* new_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        editor
        |> CodeEditable.Model.mk
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        pos,
        editors:
          DerivationTree.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor.editor,
          ),
      };
    | Editor(pos, MainEditor(action)) =>
      switch (CodeSelectable.Update.convert_action(action)) {
      | Some(action) =>
        let editor =
          DerivationTree.main_editor_of_state(~selection=pos, model.editors);
        let* new_editor =
          // Hack[Matt]: put Editor.t into a CodeSelectable.t to use its update function
          editor
          |> CodeSelectable.Model.mk
          |> CodeSelectable.Update.update(~settings, action);
        {
          ...model,
          pos,
          editors:
            DerivationTree.put_main_editor(
              ~selection=pos,
              model.editors,
              new_editor.editor,
            ),
        };
      | None => Updated.return_quiet(model)
      }
    | Editor(pos, ResultAction(UpdateResult(_)) as action) =>
      let cell = DerivationTree.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {
        ...model,
        cells: DerivationTree.put_stitched(pos, model.cells, new_cell),
      };
    | Editor(_, ResultAction(_)) => Updated.return_quiet(model)
    | MapEditor(f) =>
      let editors = model.editors |> f;
      let pos = DerivationTree.farthest_pos(model.pos, editors);
      {
        ...model,
        pos,
        editors,
        cells:
          DerivationTree.stitch_term(editors)
          |> DerivationTree.map_stitched((_, item: DerivationTree.TermItem.t) =>
               CellEditor.Model.mk(item.editor)
             ),
      }
      |> Updated.return;
    | Refresh => Updated.return(model)
    | ResetExercise =>
      let new_editors =
        DerivationTree.mapi(model.spec, pos =>
          Editor.Model.mk(~root=DerivationTree.root_of_pos(pos))
        );
      {
        ...model,
        editors: new_editors,
      }
      |> Updated.return;
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
                  dynamics: cell.editor.dynamics,
                },
                result: cell.result,
              };
            }) {
            | Invalid_argument(_)
            | Not_found =>
              let root = DerivationTree.root_of_pos(pos);
              ""
              |> DerivationTree.zipper_of_code(~root)
              |> Editor.Model.mk(~root)
              |> CellEditor.Model.mk;
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
          let result': ProgramResult.t(ProgramResult.inner) =
            switch (result) {
            | Ok((r, s)) =>
              ResultOk({
                result: r,
                state: s,
              })
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
    let editors: DerivationTree.eds = {
      let calculate = Editor.Update.calculate(~settings, ~is_edited);
      {
        ...model.editors,
        prelude:
          calculate(
            cells.prelude.editor.statics,
            cells.prelude.editor.dynamics,
            model.editors.prelude,
          ),
        setup:
          calculate(
            cells.setup.editor.statics,
            cells.setup.editor.dynamics,
            model.editors.setup,
          ),
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
                         jdmt:
                           calculate(
                             di.editor.statics,
                             di.editor.dynamics,
                             jdmt,
                           ),
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
    let verified_tree = verified_tree(editors, cells);
    {
      ...model,
      editors,
      cells,
      verified_tree,
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CellEditor.Selection.t;

  let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
    let cell_editor = DerivationTree.get_stitched(model.pos, model.cells);
    let+ a = CellEditor.Selection.get_cursor_info(~selection, cell_editor);
    Update.Editor(model.pos, a);
  };

  let handle_key_event = (~selection: t, ~event, model: Model.t) => {
    let cell_editor = DerivationTree.get_stitched(model.pos, model.cells);
    CellEditor.Selection.handle_key_event(~selection, ~event, cell_editor)
    |> Option.map(a => Update.Editor(model.pos, a));
  };

  let jump_to_tile =
      (~settings as _: Settings.t, id: Id.t, model: Model.t)
      : option((Update.t, t)) => {
    DerivationTree.positioned_editors(model.editors)
    |> List.find_opt(((_, e: Editor.t)) =>
         TermData.root_tile(id, e.syntax.term_data) != None
       )
    |> Option.map(((pos, _)) =>
         (
           Update.Editor(
             pos,
             MainEditor(Perform(Move(Goal(TileId(id))))),
           ),
           CellEditor.Selection.MainEditor,
         )
       );
  };
};

// ====== Exercise ======

module NinjaKeys = {
  open Language;
  open Js_of_ocaml;
  open Util;

  let schedule_action = ref((_: Update.t) => ());
  let current_hover_rule = ref(Rule.Assumption);

  let ( let* ) = Js.Opt.case(_, () => Js._false);

  // Wrap a function (to be called by Js setInterval) in a loop call.
  // Clear the interval if the function returns true.
  let loop = (f: unit => bool, interval: float) => {
    let id_ref = ref(Option.None);
    id_ref :=
      Some(
        Dom_html.window##setInterval(
          Js.wrap_callback(_ =>
            if (f()) {
              switch (id_ref^) {
              | Some(id) => Dom_html.window##clearInterval(id)
              | None => ()
              };
            }
          ),
          Js.float(interval),
        ),
      );
  };

  let selector = "div.hover-rule-spec";
  let selector_origin = "#page > " ++ selector;
  let selector_copied = "body > " ++ selector;
  let opt_get_origin = () =>
    Dom_html.document##querySelector(Js.string(selector_origin));
  let opt_get_copied = () =>
    Dom_html.document##querySelector(Js.string(selector_copied));

  let try_remove_copied = _ev => {
    let* copied = opt_get_copied();
    let _ =
      Dom_html.document##.body##removeChild((copied :> Js.t(Dom.node)));
    Js._true;
  };

  let elem = JsUtil.get_elem_by_id("ninja-keys-rules");
  let shadow_root = Js.Unsafe.get(_, "shadowRoot");

  module Open =
         (M: {
            let corpus: RuleImage.corpus;
            let pos: DerivationTree.pos;
          }) => {
    let copy_hover_rule_spec = (target_elem: Js.t(Dom_html.element), ev) => {
      let action = Js.Unsafe.get(target_elem, "action");
      let id = Js.to_string(action##.id);
      let rule_image = RuleImage.t_of_sexp(Sexplib.Sexp.of_string(id));
      let rule = Option.get(RuleImage.to_rule(M.corpus, rule_image));
      if (current_hover_rule^ != rule) {
        current_hover_rule := rule;
        schedule_action^(Refresh);
      };
      let* origin = opt_get_origin();
      let _ = try_remove_copied(ev);
      let _ =
        Dom_html.document##.body##appendChild(origin##cloneNode(Js._true));
      let* copied = opt_get_copied();
      let left = ev##.clientX;
      let bottom = Dom_html.window##.innerHeight - ev##.clientY;
      copied##.style##.left := Js.string(Printf.sprintf("%dpx", left));
      copied##.style##.bottom := Js.string(Printf.sprintf("%dpx", bottom));
      Js._true;
    };

    let bind_event_handler = (action: Js.t(Dom_html.element)) => {
      action##.onmousemove := Dom.handler(copy_hover_rule_spec(action));
      action##.onmouseout := Dom.handler(try_remove_copied);
      (); // TODO(zhiyao): I don't know why if it's removed, it doesn't work
    };

    let bind_event_handler_all = () => {
      let elem_root = shadow_root(elem);
      let actions = elem_root##querySelectorAll(Js.string("ninja-action"));
      let _ = actions##forEach(Js.wrap_callback(bind_event_handler));
      actions##.length != 0;
    };

    let bind_event_handler_search = () => {
      let elem_root = shadow_root(elem);
      let ninja_header = elem_root##querySelector(Js.string("ninja-header"));
      let shadow_root = shadow_root(ninja_header);
      let search: Js.t(Dom_html.inputElement) =
        shadow_root##querySelector(Js.string("#search"));
      search##.oninput :=
        Dom.handler(_ev => Js.bool(bind_event_handler_all()));
    };

    let from_rule =
        (rule: RuleImage.t)
        : {
            .
            "handler": Js.readonly_prop(unit => unit),
            "id": Js.readonly_prop(string),
            "title": Js.readonly_prop(string),
            "section": Js.readonly_prop(Js.optdef(string)),
            "keywords": Js.readonly_prop(string),
          } => {
      open RuleImage;
      [%js
       {
         val id = sexp_of_t(rule) |> Sexplib.Sexp.to_string;
         val title = show(rule);
         val section = Js.Optdef.option(Some(show_kind(of_kind(rule))));
         val handler =
           () =>
             schedule_action^(
               MapEditor(
                 DerivationTree.switch_rule(~pos=M.pos, ~rule=Some(rule)),
               ),
             );
         val keywords = keywords(rule) |> String.concat(" ")
       }
      ];
    };

    let set_data = () => {
      Js.Unsafe.set(
        elem,
        "data",
        M.corpus
        |> RuleImage.all_rules_of_version
        |> List.map(from_rule)
        |> Array.of_list
        |> Js.array,
      );
    };
  };

  let open_command_palette = (~corpus, ~pos): unit => {
    module Open =
      Open({
        let corpus = corpus;
        let pos = pos;
      });
    open Open;
    set_data();
    loop(bind_event_handler_all, 100.);
    bind_event_handler_search();
    Js.Unsafe.meth_call(elem, "open", [||]);
  };
};

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

module View = {
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

    let title_view = CellCommon.title_cell(eds.title);

    let prompt_view =
      CellCommon.narrative_cell(
        div(~attrs=[Attr.class_("cell-prompt")], [text(eds.prompt)]),
      );

    let add_premise_btn_view = (~pos: DerivationTree.pos, ~index: int) =>
      div(
        ~attrs=[
          Attr.class_("add-premise-btn"),
          Attr.on_click(_ =>
            inject(MapEditor(DerivationTree.add_premise(~pos, ~index)))
          ),
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
          NinjaKeys.open_command_palette(~corpus=eds.corpus, ~pos);
          Effect.Ignore;
        },
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
      | PartialCorrect(_) => "partial-correct"
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
        | PartialCorrect(_) => "Partial"
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
          if (globals.settings.core.dynamics) {
            result_btn_view(~res);
          } else {
            none;
          },
        ],
      );
    };

    let editor_view =
        (
          ~caption: option(string)=?,
          ~subcaption: option(string)=?,
          ~result_kind=`NoResults,
          this_pos: DerivationTree.pos,
          cell: CellEditor.Model.t,
        ) => {
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive(a)),
        ~selected=
          switch (selection) {
          | Some(s) when model.pos == this_pos => Some(s)
          | _ => None
          },
        ~inject=a => inject(Editor(this_pos, a)),
        ~result_kind,
        ~caption=
          switch (caption) {
          | Some(c) => CellCommon.caption(c, ~rest=?subcaption)
          | None => None
          },
        cell,
      );
    };

    let prelude_view =
      editor_view(
        Prelude,
        prelude,
        ~subcaption=globals.settings.instructor_mode ? "" : " (Read-Only)",
        ~caption="Prelude",
      );

    let setup_view =
      div(
        ~attrs=[Attr.class_("cell-setup")],
        [editor_view(Setup, setup, ~caption="Setup")],
      );

    let conclusion_view = (~pos, ~editor) =>
      div(
        ~attrs=[Attr.class_("deduction-concl")],
        [editor_view(pos, editor)],
      );

    let deduction_view = (~children_node, ~pos, ~res, ~rule, ~editor) =>
      div(
        ~attrs=
          [Attr.class_("deduction-just")]
          @ (
            if (pos == model.pos) {
              [Attr.class_("staged")];
            } else {
              [];
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
            inject(MapEditor(DerivationTree.add_abbr(~index)))
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
      |> List.map2(Tree.combine, model.verified_tree)
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
          div(~attrs=[Attr.class_("version-label")], [text("Corpus: ")]),
          text(Unicode.nbsp),
          text(RuleImage.show_corpus(eds.corpus)),
          text(Unicode.nbsp),
          select(
            ~attrs=[
              Attr.class_("version-select"),
              Attr.on_change((_, name) => {
                let corpus = RuleImage.corpus_of_string(name);
                inject(
                  MapEditor(
                    m =>
                      {
                        ...m,
                        corpus,
                      },
                  ),
                );
              }),
            ],
            List.map(
              option_view(RuleImage.show_corpus(eds.corpus)),
              RuleImage.all_of_corpus |> List.map(RuleImage.show_corpus),
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
