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

let tests = (
  "TypeSlicing.UI",
  [
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
    test_case("refresh keeps active query while moving focus", `Quick, () => {
      check(
        string,
        "? + 2 + ?",
        "? + 2 + ?",
        fold_after_refresh(
          ~from_focus=int_id(1),
          ~to_focus=int_id(2),
          ~query="Int",
          "1 + 2 + 3",
        ),
      )
    }),
  ],
);
