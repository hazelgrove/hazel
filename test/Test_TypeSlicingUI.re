open Alcotest;
open Language;
open Haz3lcore;

module CI = Web.CursorInspector;

let parse_exp = Test_Statics_Slicing_Prelude.parse_exp;
let parse_typ = Test_Statics_Slicing_Prelude.parse_typ;

let code_model = (src: string): Web.CodeEditable.Model.t => {
  let z =
    switch (Parser.to_zipper(~root=Exp, src)) {
    | Some(z) => z
    | None => fail("could not parse code source")
    };
  let editor = Editor.Model.mk(z, ~root=Exp);
  let model = Web.CodeWithStatics.Model.mk(editor);
  let statics =
    CachedStatics.init(
      ~settings=CoreSettings.on,
      ~stitch=x => x,
      ~is_dynamic_term=false,
      ~root=Exp,
      z,
    );
  {
    ...model,
    statics,
  };
};

let folded_query_editor = (typ: Typ.t): Web.CodeEditable.Model.t => {
  let (_, editor) = CI.type_editor_of_type(typ);
  let seg = editor.editor.state.zipper |> Zipper.unselect_and_zip;
  let folded =
    switch (ProjectorPerform.init(ProjectorCore.Kind.Fold, seg)) {
    | Some(piece) => [piece]
    | None => fail("could not fold query type")
    };
  {
    ...editor,
    editor: Editor.Model.mk(Zipper.unzip(folded), ~root=Sort.Typ),
  };
};

let query_row = (focus: Id.t, query: string): CI.Model.row => {
  let typ = parse_typ(query);
  let editor = folded_query_editor(typ);
  {
    active: true,
    cursor_id: CI.Model.OptionalId.SomeId(focus),
    typ_id: CI.Model.OptionalId.SomeId(Typ.rep_id(typ)),
    editor: CI.Model.EditorSlot.SomeEditor(editor),
  };
};

let slicing_model = (focus: Id.t, query: string): CI.Model.t => {
  syn: query_row(focus, query),
  ana: CI.Model.empty_row,
  menu: CI.Model.NoMenu,
  anchor: CI.Model.OptionalId.SomeId(focus),
  anchor_caret: CI.Model.NoCaret,
  focus_target: CI.Model.Main,
};

let render_folded = (model: Web.CodeEditable.Model.t): string =>
  model.editor.state.zipper
  |> Zipper.unselect_and_zip
  |> Printer.of_segment(
       ~holes="?",
       ~projector_to_segment=_ => [Piece.mk_grout(Convex)],
       _,
     );

let info_exp_id = (model: Web.CodeEditable.Model.t, pred): Id.t =>
  model.statics.info_map
  |> Id.Map.bindings
  |> List.find_map(((id, info)) =>
       switch (info) {
       | Info.InfoExp({user_term, _}) when pred(user_term) => Some(id)
       | _ => None
       }
     )
  |> Option.get;

let int_id = (n: int, model: Web.CodeEditable.Model.t): Id.t =>
  info_exp_id(model, e =>
    switch (Exp.term_of(e)) {
    | Atom(Int(value)) => Bigint.to_string(value) == string_of_int(n)
    | _ => false
    }
  );

let first_binop_id = (model: Web.CodeEditable.Model.t): Id.t =>
  info_exp_id(model, e =>
    switch (Exp.term_of(e)) {
    | BinOp(_) => true
    | _ => false
    }
  );

let fold_slice = (~focus, ~query, src: string): string => {
  let code = code_model(src);
  let focus = focus(code);
  let cursor_inspector = slicing_model(focus, query);
  switch (CI.TypeSlicing.query_of_row(cursor_inspector.syn)) {
  | Some(_) => ()
  | None => fail("query_of_row failed")
  };
  let root_exp =
    switch (CI.ProgramFolds.root_exp(code)) {
    | Some(root_exp) => root_exp
    | None => fail("root_exp failed")
    };
  let omitted =
    CI.TypeSlicing.omitted_ids_for_model(
      ~root_exp,
      ~info_map=code.statics.info_map,
      ~fallback_ci=None,
      cursor_inspector,
    );
  if (Id.Set.is_empty(omitted)) {
    fail("omitted ids empty");
  };
  let result =
    CI.ProgramFolds.apply_type_slice(
      ~info_map=code.statics.info_map,
      ~fallback_ci=None,
      ~cursor_inspector,
      code,
    );
  render_folded(result.model);
};

let fold_after_refresh = (~from_focus, ~to_focus, ~query, src: string): string => {
  let code = code_model(src);
  let cursor_inspector = slicing_model(from_focus(code), query);
  let to_focus = to_focus(code);
  let ci =
    switch (Id.Map.find_opt(to_focus, code.statics.info_map)) {
    | Some(ci) => ci
    | None => fail("target focus info missing")
    };
  let cursor_inspector = CI.Model.refresh_for_info(ci, cursor_inspector);
  let result =
    CI.ProgramFolds.apply_type_slice(
      ~info_map=code.statics.info_map,
      ~fallback_ci=None,
      ~cursor_inspector,
      code,
    );
  render_folded(result.model);
};

let case = (~name, ~src, ~focus, ~query, ~expected) =>
  test_case(name, `Quick, () => {
    check(string, expected, expected, fold_slice(~focus, ~query, src))
  });

let select_term = (id: Id.t, code: Web.CodeEditable.Model.t): Zipper.t =>
  switch (
    Select.term(
      ~defs_exclude_bodies=false,
      ~case_rules=false,
      code.editor.syntax.term_data,
      id,
      code.editor.state.zipper,
    )
  ) {
  | Some(z) => z
  | None => fail("could not select term")
  };

let tests = (
  "TypeSlicing.UI",
  [
    test_case(
      "selection root anchors the selected term",
      `Quick,
      () => {
        let code = code_model("fun x : String -> 1 + 2");
        let binop = first_binop_id(code);
        let z = select_term(binop, code);
        let anchor =
          switch (
            Web.CursorInspector.selection_root_info(
              ~info_map=code.statics.info_map,
              z,
            )
          ) {
          | Some(ci) => Info.id_of(ci)
          | None => fail("selection_root_info returned None")
          };
        check(bool, "anchor is binop", true, Id.equal(anchor, binop));
        let code = {
          ...code,
          editor: Editor.Model.mk(z, ~root=Exp),
        };
        let unfolded_query_row = {
          let typ = parse_typ("Int");
          let (_, editor) = CI.type_editor_of_type(typ);
          CI.Model.{
            active: true,
            cursor_id: CI.Model.OptionalId.SomeId(anchor),
            typ_id: CI.Model.OptionalId.SomeId(Typ.rep_id(typ)),
            editor: CI.Model.EditorSlot.SomeEditor(editor),
          };
        };
        let toggle_inspector = {
          ...slicing_model(anchor, "Int"),
          syn: unfolded_query_row,
        };
        let first =
          CI.ProgramFolds.apply_type_slice(
            ~info_map=code.statics.info_map,
            ~fallback_ci=None,
            ~cursor_inspector=toggle_inspector,
            code,
          );
        check(
          string,
          "toggle keeps selected binop shape",
          "fun ? -> ? + ?",
          render_folded(first.model),
        );
        let folded_statics =
          CachedStatics.init(
            ~settings=CoreSettings.on,
            ~stitch=x => x,
            ~is_dynamic_term=false,
            ~root=Exp,
            first.model.editor.state.zipper,
          );
        let folded_code = {
          ...first.model,
          statics: folded_statics,
        };
        let gap_inspector = slicing_model(anchor, "Int");
        let unfolded = CI.ProgramFolds.remove_all(folded_code);
        let second =
          CI.ProgramFolds.apply_type_slice(
            ~info_map=unfolded.model.statics.info_map,
            ~fallback_ci=None,
            ~cursor_inspector=gap_inspector,
            unfolded.model,
          );
        check(
          string,
          "gap reslice keeps selected binop shape",
          "fun ? -> ? + ?",
          render_folded(second.model),
        );
      },
    ),
    case(
      ~name="focused gap keeps focused literal",
      ~src="1 + 2 + 3",
      ~focus=int_id(2),
      ~query="Int",
      ~expected="? + 2 + ?",
    ),
    case(
      ~name="focused gap keeps focused body shape",
      ~src="fun x : String -> 1 + 2",
      ~focus=first_binop_id,
      ~query="Int",
      ~expected="fun ? -> ? + ?",
    ),
    test_case("anchor stays fixed while cursor moves", `Quick, () => {
      check(
        string,
        "1 + ? + ?",
        "1 + ? + ?",
        fold_after_refresh(
          ~from_focus=int_id(1),
          ~to_focus=int_id(2),
          ~query="Int",
          "1 + 2 + 3",
        ),
      )
    }),
    test_case("anchor pins slice in a fun body", `Quick, () => {
      check(
        string,
        "fun ? -> ? + ?",
        "fun ? -> ? + ?",
        fold_after_refresh(
          ~from_focus=first_binop_id,
          ~to_focus=int_id(1),
          ~query="Int",
          "fun x : String -> 1 + 2",
        ),
      )
    }),
  ],
);
