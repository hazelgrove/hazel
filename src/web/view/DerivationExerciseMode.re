open Haz3lcore;
open Virtual_dom.Vdom;
open Util_web;
open Language;
open Node;

let stitched_results =
  DerivationExercise.map_stitched((_, cell_editor: CellEditor.Model.t) =>
    cell_editor.result.result
    |> Calc.save
    |> Calc.get_saved_opt
    |> (
      fun
      | Some(ProgramResult.ResultOk(r)) => Some(r.result)
      | Some(ResultFail(_))
      | Some(ResultPending(_))
      | None => None
    )
  );

let verified_tree =
    (
      editors: DerivationExercise.p(Editor.t),
      cells: DerivationExercise.stitched(CellEditor.Model.t),
    ) =>
  DrvGrading.VerifiedTree.mk(
    editors,
    ~stitched_results=stitched_results(cells),
  );

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type editing_flags = {
    editing_title: bool,
    editing_module_name: bool,
    editing_prompt: bool,
  };

  let editing_flags_false = {
    editing_title: false,
    editing_module_name: false,
    editing_prompt: false,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    spec: DerivationExercise.spec,
    editors: DerivationExercise.p(Editor.t),
    cells: DerivationExercise.stitched(CellEditor.Model.t),
    verified_tree: DrvGrading.VerifiedTree.t,
    pos: DerivationExercise.pos,
    editing_flags,
  };

  let of_spec = (~settings as _, ~instructor_mode as _: bool, spec) => {
    let editors =
      DerivationExercise.mapi(spec, pos =>
        Editor.Model.mk(~root=DerivationExercise.root_of_pos(pos))
      );
    let term_item_to_cell =
        (item: DerivationExercise.TermItem.t): CellEditor.Model.t => {
      CellEditor.Model.mk(item.editor);
    };
    let cells =
      DerivationExercise.stitch_term(editors)
      |> DerivationExercise.map_stitched(_ => term_item_to_cell);
    let verified_tree = verified_tree(editors, cells);
    let pos = DerivationExercise.Prelude;
    {
      pos,
      spec,
      editors,
      cells,
      verified_tree,
      editing_flags: editing_flags_false,
    };
  };

  let is_editable =
      (
        ~instructor_mode,
        ~scratch_mode=false,
        pos: DerivationExercise.pos,
        model: t,
      )
      : bool =>
    /* In the unified scratch/derivation mode, all positions are freely
       editable (there is no instructor/student distinction, and no goal
       conclusion that should be locked). */
    if (scratch_mode) {
      true;
    } else {
      switch (pos) {
      | Prelude => instructor_mode
      | Setup => true
      | Trees(i, Value) when i + 1 == List.length(model.editors.trees) => instructor_mode
      | Trees(_) => true
      };
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = DerivationExercise.persistent_state;

  let persist = (exercise: t, ~instructor_mode as _: bool): persistent => {
    DerivationExercise.map(exercise.editors, editor =>
      editor.state.zipper |> PersistentZipper.persist
    );
  };

  let unpersist = (~instructor_mode, persistent: persistent, spec) => {
    ignore(spec);
    let spec =
      DerivationExercise.mapi(persistent, pos =>
        PersistentZipper.unpersist(~root=DerivationExercise.root_of_pos(pos))
      );
    of_spec(~instructor_mode, spec);
  };

  let get_derivation_info_at = (pos: DerivationExercise.pos, model: t) => {
    let trees = model.verified_tree;
    let eds = model.editors;
    switch (pos) {
    | Trees(i, pos) =>
      try({
        let tree = List.nth(trees, i);
        let res = Tree.nth(tree, pos);
        let tree = List.nth(eds.trees, i);
        let ed = Tree.nth(tree, pos);
        switch (ed, res) {
        | (Just({rule: Some(rule), _}), {rule: None, _}) =>
          Language.(
            switch (RuleImage.to_rule(eds.rule_set, rule)) {
            | Some(rule) =>
              Some({
                ...res,
                rule:
                  Some({
                    rule,
                    spec: RuleSpec.of_spec(rule),
                  }),
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

  /* Editors whose problems should appear in the Problems sidebar, each
     paired with a display label. Only cells that are actually rendered are
     listed: the Prelude is shown in exercise mode but not in scratch /
     documentation Drv slides, and abbreviation tree nodes carry no editor
     (`None` in `cells.trees`, dropped below). Read-only cells (e.g. the
     student-mode Prelude or the goal conclusion) are still shown and
     jumpable, so they stay. All tree judgement editors are bundled into a
     single "Derivation" group (multi-source, so per-row line numbers are
     suppressed since L# would refer to different editors' geometries).
     Trees are walked in postorder so within-tree order matches the visual
     top-to-bottom layout (premises above the conclusion); trees themselves
     are in display order. */
  let get_problem_editors =
      (~scratch_mode: bool, model: t)
      : list((option(string), list(CodeEditable.Model.t))) => {
    let rec postorder = (Tree.Node(v, c)) =>
      List.concat_map(postorder, c) @ [v];
    let tree_editors =
      model.cells.trees
      |> List.concat_map(tree =>
           tree
           |> postorder
           |> List.filter_map(cell_opt =>
                Option.map(
                  (cell: CellEditor.Model.t) => cell.editor,
                  cell_opt,
                )
              )
         );
    (scratch_mode ? [] : [(Some("Prelude"), [model.cells.prelude.editor])])
    @ [(Some("Setup"), [model.cells.setup.editor])]
    @ [(Some("Derivation"), tree_editors)];
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type instructor =
    | EditingTitle
    | EditingModuleName
    | EditingPrompt
    | UpdateTitle(string)
    | UpdateModuleName(string)
    | UpdatePrompt(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Editor(DerivationExercise.pos, CellEditor.Update.t)
    | MapEditor(DerivationExercise.eds => DerivationExercise.eds)
    | Instructor(instructor)
    | Refresh
    | ResetExercise;

  let instructor_update =
      (action: instructor, model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | EditingTitle =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_title: !model.editing_flags.editing_title,
        },
      })
    | EditingModuleName =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_module_name: !model.editing_flags.editing_module_name,
        },
      })
    | EditingPrompt =>
      Updated.return_quiet({
        ...model,
        editing_flags: {
          ...model.editing_flags,
          editing_prompt: !model.editing_flags.editing_prompt,
        },
      })
    | UpdateTitle(title) =>
      Updated.return_quiet(
        {
          ...model,
          editors: DerivationExercise.update_title(model.editors, title),
        },
        ~is_edit=true,
      )
    | UpdateModuleName(module_name) =>
      Updated.return(
        {
          ...model,
          editors:
            DerivationExercise.update_module_name(model.editors, module_name),
        },
        ~is_edit=true,
      )
    | UpdatePrompt(prompt) =>
      Updated.return(
        {
          ...model,
          editors: DerivationExercise.update_prompt(model.editors, prompt),
        },
        ~is_edit=true,
      )
    };
  };

  let instructor_update =
      (~settings: Settings.t, action: instructor, model: Model.t)
      : Updated.t(Model.t) =>
    if (settings.instructor_mode) {
      instructor_update(action, model);
    } else {
      Updated.return_quiet(model);
    };

  let update =
      (
        ~settings: Settings.t,
        ~schedule_action as _,
        ~scratch_mode=false,
        action,
        model: Model.t,
      )
      : Updated.t(Model.t) => {
    let instructor_mode = settings.instructor_mode;
    switch (action) {
    | Editor(pos, MainEditor(action))
        when Model.is_editable(pos, ~instructor_mode, ~scratch_mode, model) =>
      let editor =
        DerivationExercise.main_editor_of_state(
          ~selection=pos,
          model.editors,
        );
      let* new_editor =
        // Hack[Matt]: put Editor.t into a CodeEditor.t to use its update function
        editor
        |> CodeEditable.Model.mk
        |> CodeEditable.Update.update(~settings, action);
      {
        ...model,
        pos,
        editors:
          DerivationExercise.put_main_editor(
            ~selection=pos,
            model.editors,
            new_editor.editor,
          ),
      };
    | Editor(pos, MainEditor(action)) =>
      switch (CodeSelectable.Update.convert_action(action)) {
      | Some(action) =>
        let editor =
          DerivationExercise.main_editor_of_state(
            ~selection=pos,
            model.editors,
          );
        let* new_editor =
          // Hack[Matt]: put Editor.t into a CodeSelectable.t to use its update function
          editor
          |> CodeSelectable.Model.mk
          |> CodeSelectable.Update.update(~settings, action);
        {
          ...model,
          pos,
          editors:
            DerivationExercise.put_main_editor(
              ~selection=pos,
              model.editors,
              new_editor.editor,
            ),
        };
      | None => Updated.return_quiet(model)
      }
    | Editor(pos, ResultAction(UpdateResult(_)) as action) =>
      let cell = DerivationExercise.get_stitched(pos, model.cells);
      let* new_cell = CellEditor.Update.update(~settings, action, cell);
      {
        ...model,
        cells: DerivationExercise.put_stitched(pos, model.cells, new_cell),
      };
    | Editor(_, ResultAction(_)) => Updated.return_quiet(model)
    | MapEditor(f) =>
      let editors = model.editors |> f;
      let pos = DerivationExercise.farthest_pos(model.pos, editors);
      {
        ...model,
        pos,
        editors,
        cells:
          DerivationExercise.stitch_term(editors)
          |> DerivationExercise.map_stitched(
               (_, item: DerivationExercise.TermItem.t) =>
               CellEditor.Model.mk(item.editor)
             ),
      }
      |> Updated.return;
    | Instructor(action) => instructor_update(~settings, action, model)
    | Refresh => Updated.return(~historic=false, model)
    | ResetExercise =>
      let new_editors =
        DerivationExercise.mapi(model.spec, pos =>
          Editor.Model.mk(~root=DerivationExercise.root_of_pos(pos))
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
    let stitched_elabs = DerivationExercise.stitch_term(model.editors);
    let worker_request = ref([]);
    let queue_worker = (pos, expr) => {
      worker_request :=
        worker_request^ @ [(pos |> DerivationExercise.key_for_statics, expr)];
    };
    let cells: DerivationExercise.stitched(CellEditor.Model.t) =
      DerivationExercise.map_stitched(
        (pos, {term, editor}: DerivationExercise.TermItem.t) => {
          (
            try({
              let cell = DerivationExercise.get_stitched(pos, model.cells);
              {
                editor: {
                  editor,
                  statics: cell.editor.statics,
                  dynamics: cell.editor.dynamics,
                  context_menu: cell.editor.context_menu,
                },
                result: cell.result,
              };
            }) {
            | Invalid_argument(_)
            | Not_found =>
              let root = DerivationExercise.root_of_pos(pos);
              ""
              |> DerivationExercise.zipper_of_code(~root)
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

    EvalRequest.request(
      worker_request^,
      ~pos_of_key=DerivationExercise.pos_of_key,
      ~dispatch=
        (pos, action) =>
          schedule_action(Editor(pos, ResultAction(action))),
      ~on_timeout=
        _ =>
          ignore(
            DerivationExercise.map_stitched(
              (pos, _) =>
                schedule_action(
                  Editor(
                    pos,
                    ResultAction(UpdateResult(ResultFail(Timeout))),
                  ),
                ),
              model.cells,
            ),
          ),
    );
    /* The following section pulls statics back from cells into the editors
       There are many ad-hoc things about this code, including the fact that
       one of the editors is shown in two cells, so we arbitrarily choose which
       statics to take */
    let editors: DerivationExercise.eds = {
      let calculate =
        Editor.Update.calculate(~settings, ~autoprobe_mode=false, ~is_edited);
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
          List.map2(Util_web.Tree.combine, cells.trees, model.editors.trees)
          |> List.map(
               Util_web.Tree.map(
                 fun
                 | (
                     Some(di: CellEditor.Model.t),
                     DerivationExercise.Abbr.Just(
                       DerivationExercise.{jdmt, rule},
                     ),
                   ) => {
                     DerivationExercise.Abbr.Just(
                       DerivationExercise.{
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
                 | (None, DerivationExercise.Abbr.Abbr(d)) =>
                   DerivationExercise.Abbr.Abbr(d)
                 /* The cells/editors trees are built in lockstep, so a cell
                    should be [Some(_)] iff the matching editor is [Just(_)]. */
                 | (None, _) =>
                   failwith(
                     "DerivationExerciseMode.calculate: editor present but no cell",
                   )
                 | (Some(_), _) =>
                   failwith(
                     "DerivationExerciseMode.calculate: cell present but no editor",
                   ),
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
  /* The `pos` in `InCell` identifies which cell the user is actually
     focused on. We cannot rely on `model.pos` here because that field is
     only updated on edit actions (not on click/focus), which causes
     cursor-info and derivation-info to reflect the previously edited cell
     rather than the currently focused one. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | TextBox
    | InCell(DerivationExercise.pos, CellEditor.Selection.t);

  let pos_of: t => option(DerivationExercise.pos) =
    fun
    | TextBox => None
    | InCell(pos, _) => Some(pos);

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection: t, model: Model.t)
      : cursor(Update.t) => {
    switch (selection) {
    | TextBox => empty
    | InCell(pos, s) =>
      let cell_editor = DerivationExercise.get_stitched(pos, model.cells);
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~inject=a => inject(Editor(pos, a)),
          ~selection=s,
          cell_editor,
        );
      Update.Editor(pos, a);
    };
  };

  let jump_to_tile =
      (~settings as _: Settings.t, id: Id.t, model: Model.t)
      : option((Update.t, t)) => {
    DerivationExercise.positioned_editors(model.editors)
    |> List.find_opt(((_, e: Editor.t)) =>
         TermData.root_piece(id, e.syntax.term_data) != None
       )
    |> Option.map(((pos, _)) =>
         (
           Update.Editor(
             pos,
             MainEditor(Perform(Move(Goal(TileId(id))))),
           ),
           InCell(pos, CellEditor.Selection.MainEditor),
         )
       );
  };

  /* Use the selection's live pos to check whether the cursor is in a
     derivation tree cell. */
  let get_derivation_info = (~selection: t, model: Model.t) =>
    switch (pos_of(selection)) {
    | Some(pos) => Model.get_derivation_info_at(pos, model)
    | None => None
    };
};

// ====== Exercise ======

module NinjaKeys = {
  open Language;
  open Js_of_ocaml;
  open Util;

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

  /* Lazy: module init must not touch the DOM (the test binary links
     this module under node). */
  let elem = Lazy.from_fun(() => JsUtil.get_elem_by_id("ninja-keys-rules"));
  let shadow_root = Js.Unsafe.get(_, "shadowRoot");

  module Open =
         (
           M: {
             let rule_set: RuleImage.rule_set;
             let pos: DerivationExercise.pos;
             let schedule_action: Update.t => Ui_effect.t(unit);
           },
         ) => {
    let copy_hover_rule_spec = (target_elem: Js.t(Dom_html.element), ev) => {
      let action = Js.Unsafe.get(target_elem, "action");
      let id = Js.to_string(action##.id);
      let rule_image = RuleImage.t_of_sexp(Sexplib.Sexp.of_string(id));
      let rule = Option.get(RuleImage.to_rule(M.rule_set, rule_image));
      if (current_hover_rule^ != rule) {
        current_hover_rule := rule;
        Ui_effect.Expert.handle(M.schedule_action(Refresh));
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
      /* FIXME: removing this trailing unit breaks the JS interop — investigate
         why the function's return type drifts without it. */
      ();
    };

    let bind_event_handler_all = () => {
      let elem_root = shadow_root(elem);
      if (elem_root != None) {
        let actions = elem_root##querySelectorAll(Js.string("ninja-action"));
        let _ = actions##forEach(Js.wrap_callback(bind_event_handler));
        actions##.length != 0;
      } else {
        true;
      };
    };

    let bind_event_handler_search = () => {
      let elem_root = shadow_root(elem);
      if (elem_root != None) {
        let ninja_header =
          elem_root##querySelector(Js.string("ninja-header"));
        let shadow_root = shadow_root(ninja_header);
        let search: Js.t(Dom_html.inputElement) =
          shadow_root##querySelector(Js.string("#search"));
        search##.oninput :=
          Dom.handler(_ev => Js.bool(bind_event_handler_all()));
      };
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
             Ui_effect.Expert.handle(
               M.schedule_action(
                 MapEditor(
                   DerivationExercise.switch_rule(
                     ~pos=M.pos,
                     ~rule=Some(rule),
                   ),
                 ),
               ),
             );
         val keywords = keywords(rule) |> String.concat(" ")
       }
      ];
    };

    let set_data = () => {
      Js.Unsafe.set(
        Lazy.force(elem),
        "data",
        M.rule_set
        |> RuleImage.all_rules_of_rule_set
        |> List.map(from_rule)
        |> Array.of_list
        |> Js.array,
      );
    };
  };

  let open_command_palette = (~rule_set, ~pos, ~inject): unit => {
    module Open =
      Open({
        let rule_set = rule_set;
        let pos = pos;
        let schedule_action = (a: Update.t) => inject(a);
      });
    open Open;
    set_data();
    loop(bind_event_handler_all, 100.);
    bind_event_handler_search();
    Js.Unsafe.meth_call(Lazy.force(elem), "open", [||]);
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
  type view_info = (DerivationExercise.pos, DrvGrading.VerifiedTree.info, ed)
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
        ~inject_explainthis: ExplainThisUpdate.update => 'b,
        ~selection: option(Selection.t),
        ~scratch_mode: bool=false,
        model: Model.t,
      ) => {
    let eds = model.editors;

    let add_premise_btn_view = (~pos: DerivationExercise.pos, ~index: int) =>
      div(
        ~attrs=[
          Attr.class_("add-premise-btn"),
          Attr.on_click(_ =>
            inject(MapEditor(DerivationExercise.add_premise(~pos, ~index)))
          ),
        ],
        [],
      );

    let del_premise_btn_view = (~pos: DerivationExercise.pos) =>
      Widgets.button_named(
        Icons.trash,
        _ => inject(MapEditor(DerivationExercise.del_premise(~pos))),
        ~tooltip=
          switch (pos) {
          | Trees(_, Value) => "Delete Abbr"
          | _ => "Delete Premise"
          },
      );

    let pop_premise_btn_view = (~pos: DerivationExercise.pos) =>
      Widgets.button_named(
        Icons.export,
        _ => inject(MapEditor(DerivationExercise.pop_premise(~pos))),
        ~tooltip="Pop out to Abbr",
      );

    let push_premise_btn_view = (~pos: DerivationExercise.pos) =>
      Widgets.button_named(
        Icons.import,
        _ => inject(MapEditor(DerivationExercise.push_premise(~pos))),
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
        (~pos: DerivationExercise.pos, ~index: option(int)) =>
      switch (index) {
      | Some(index) =>
        Widgets.button_named(
          abbr_to_label(Some(index)),
          _ =>
            inject(
              MapEditor(
                DerivationExercise.switch_abbr(~pos, ~index=Some(index)),
              ),
            ),
          ~tooltip="Use Abbr d" ++ string_of_int(index),
        )
      | None => Node.none
      };

    let dropdown_switch_rule_view = (~pos: DerivationExercise.pos) =>
      Widgets.button_named(
        Icons.command_palette_sparkle,
        _ => {
          NinjaKeys.open_command_palette(
            ~rule_set=eds.rule_set,
            ~pos,
            ~inject,
          );
          Effect.Ignore;
        },
        ~tooltip="Switch Rule",
      );

    let dropdown_switch_just_view = (~pos: DerivationExercise.pos) =>
      Widgets.button_named(
        Icons.forward,
        _ =>
          inject(
            MapEditor(DerivationExercise.switch_rule(~pos, ~rule=None)),
          ),
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
      | DerivationExercise.Trees(_, Value) => true
      | _ => false;

    let dropdown_view = (~pos, ~res, ~index): t =>
      div(
        ~attrs=[
          Attr.class_("dropdown"),
          Attr.class_(class_of_result(res)),
        ],
        (
          DerivationExercise.all_abbrs(pos)
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
          | None when !pos_is_value(pos) => [pop_premise_btn_view(~pos)]
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
          this_pos: DerivationExercise.pos,
          cell: CellEditor.Model.t,
        ) => {
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(a) => signal(MakeActive(InCell(this_pos, a))),
        ~selected=
          switch (selection) {
          | Some(InCell(pos, s)) when pos == this_pos => Some(s)
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

    let conclusion_view = (~pos, ~editor) =>
      div(
        ~attrs=[Attr.class_("deduction-concl")],
        [editor_view(pos, editor)],
      );

    let deduction_view = (~children_node, ~pos, ~res, ~rule, ~editor) =>
      div(
        ~attrs=[Attr.class_("deduction-just")],
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
            inject(MapEditor(DerivationExercise.add_abbr(~index)))
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
      List.map2(Tree.combine, eds.trees, model.cells.trees)
      |> List.map(
           Tree.map(
             fun
             | (
                 DerivationExercise.Abbr.Just(DerivationExercise.{rule, _}),
                 Some(di),
               ) => (
                 Just(rule, di): ed
               )
             | (Abbr(i), _) => Abbr(i)
             | _ =>
               raise(Failure("DerivationExercise.mk: ed<>di inconsistent")),
           ),
         )
      |> List.map2(Tree.combine, model.verified_tree)
      |> List.mapi(i =>
           Tree.mapi((pos, (res, ed)) =>
             (DerivationExercise.Trees(i, pos), res, ed)
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
        ~attrs=[Attr.classes(["cell", "unlocked"])],
        [
          CellCommon.caption("Derivation"),
          div(
            ~attrs=[Attr.classes(["cell-item", "derivation-panel"])],
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
          ),
        ],
      );

    let editing_flags = model.editing_flags;
    let on_focus_textbox = _ => signal(MakeActive(TextBox));

    let title_view =
      InstructorEditViews.title_view(
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_title,
        ~title=eds.title,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingTitle)),
        ~update_title=t => inject(Instructor(UpdateTitle(t))),
      );

    let module_name_view =
      InstructorEditViews.module_name_view(
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_module_name,
        ~module_name=eds.module_name,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingModuleName)),
        ~update_module_name=m => inject(Instructor(UpdateModuleName(m))),
      );

    let prompt_view =
      InstructorEditViews.prompt_view(
        ~globals,
        ~inject_explainthis,
        ~instructor_mode=globals.settings.instructor_mode,
        ~is_editing=editing_flags.editing_prompt,
        ~prompt=eds.prompt,
        ~on_focus_textbox,
        ~toggle_editing=_ => inject(Instructor(EditingPrompt)),
        ~update_prompt=p => inject(Instructor(UpdatePrompt(p))),
      );

    let option_view = (name, n) =>
      option(
        ~attrs=n == name ? [Attr.create("selected", "selected")] : [],
        [text(n)],
      );

    let rule_set_view = {
      let can_edit = globals.settings.instructor_mode || scratch_mode;
      let control =
        if (can_edit) {
          select(
            ~attrs=[
              Attr.class_("rule-set-select"),
              Attr.title("Toggle Rule Set"),
              Attr.on_change((_, name) => {
                let rule_set = RuleImage.rule_set_of_string(name);
                inject(
                  MapEditor(
                    m =>
                      {
                        ...m,
                        rule_set,
                      },
                  ),
                );
              }),
            ],
            List.map(
              option_view(RuleImage.show_rule_set(eds.rule_set)),
              RuleImage.all_of_rule_set |> List.map(RuleImage.show_rule_set),
            ),
          );
        } else {
          text(RuleImage.show_rule_set(eds.rule_set));
        };
      /* Use .unlocked so the cell picks up the same left-border accent as
         the Setup/Derivation cells above/below it. */
      div(
        ~attrs=[Attr.classes(["cell", "unlocked"])],
        [
          CellCommon.caption("Rule Set"),
          CellCommon.simple_cell_item([control]),
        ],
      );
    };

    let prelude_view =
      editor_view(
        Prelude,
        model.cells.prelude,
        ~subcaption=globals.settings.instructor_mode ? "" : " (Read-Only)",
        ~caption="Prelude",
      );

    let setup_view = editor_view(Setup, model.cells.setup, ~caption="Setup");

    if (scratch_mode) {
      [rule_set_view, setup_view, derivations_view];
    } else {
      let score_view =
        Grading.score_view(
          GradeExercise.score_of_verified_tree(
            model.spec,
            model.verified_tree,
          ),
        );
      [
        score_view,
        title_view,
        module_name_view,
        prompt_view,
        rule_set_view,
        prelude_view,
        setup_view,
        derivations_view,
      ];
    };
  };
};
