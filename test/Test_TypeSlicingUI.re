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

let query_row = (~typed=false, focus: Id.t, query: string): CI.Model.row => {
  let typ = parse_typ(query);
  let editor =
    typed ? snd(CI.type_editor_of_type(typ)) : folded_query_editor(typ);
  {
    active: true,
    cursor_id: CI.Model.OptionalId.SomeId(focus),
    typ_id: CI.Model.OptionalId.SomeId(Typ.rep_id(typ)),
    editor: CI.Model.EditorSlot.SomeEditor(editor),
  };
};

let slicing_model = (~typed=false, focus: Id.t, query: string): CI.Model.t => {
  syn: query_row(~typed, focus, query),
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

let render_typ = (typ: Typ.t): string =>
  ExpToSegment.typ_to_segment(
    ~settings=Web.ExplainThis.slice_view_settings,
    typ,
  )
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

let ctor_arg_ap_id = (name: string, model: Web.CodeEditable.Model.t): Id.t =>
  info_exp_id(model, e =>
    switch (Exp.term_of(e)) {
    | Ap(_, fn, arg) =>
      switch (Exp.term_of(fn), Exp.term_of(arg)) {
      | (Constructor(_, _), Constructor(ctor, _)) => ctor == name
      | _ => false
      }
    | _ => false
    }
  );

let first_tuple_id = (model: Web.CodeEditable.Model.t): Id.t =>
  info_exp_id(model, e =>
    switch (Exp.term_of(e)) {
    | Tuple(_) => true
    | _ => false
    }
  );

let rec is_product_type = (typ: Typ.t): bool =>
  switch (Typ.term_of(typ)) {
  | Prod(_) => true
  | Parens(typ) => is_product_type(typ)
  | _ => false
  };

let info_typ_id = (model: Web.CodeEditable.Model.t, pred): Id.t =>
  model.statics.info_map
  |> Id.Map.bindings
  |> List.find_map(((id, info)) =>
       switch (info) {
       | Info.InfoTyp({user_term, _}) when pred(user_term) => Some(id)
       | _ => None
       }
     )
  |> Option.get;

let first_product_type_id = (model: Web.CodeEditable.Model.t): Id.t =>
  info_typ_id(model, is_product_type);

let fold_slice = (~typed=false, ~focus, ~query, src: string): string => {
  let code = code_model(src);
  let focus = focus(code);
  let cursor_inspector = slicing_model(~typed, focus, query);
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
      ~term_data=code.editor.syntax.term_data,
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

let typed_case = (~name, ~src, ~focus, ~query, ~expected) =>
  test_case(name, `Quick, () => {
    check(
      string,
      expected,
      expected,
      fold_slice(~typed=true, ~focus, ~query, src),
    )
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
          "gap reslice folds selected binop",
          "fun ? -> ?",
          render_folded(second.model),
        );
      },
    ),
    case(
      ~name="focused gap folds focused literal",
      ~src="1 + 2 + 3",
      ~focus=int_id(2),
      ~query="Int",
      ~expected="? + ? + ?",
    ),
    case(
      ~name="focused gap folds focused body",
      ~src="fun x : String -> 1 + 2",
      ~focus=first_binop_id,
      ~query="Int",
      ~expected="fun ? -> ?",
    ),
    typed_case(
      ~name="typed query on focused branch keeps context",
      ~src=
        "type Option = typfun A -> None + Some(A) in type Digit = Zero + One in let parse_digit = fun s : String -> case s | \"0\" => Some(Zero) | \"1\" => Some(One) | _ => None end in parse_digit(\"5\")",
      ~focus=ctor_arg_ap_id("One"),
      ~query="None + Some(Digit)",
      ~expected=
        "type Option = typfun A -> ? + Some(A) in type ? = ? in let parse_digit = fun ? -> case ? | ? => ? | ? => Some(?) | ? => ? end in parse_digit(?)",
    ),
    case(
      ~name="gap query on focused branch keeps context",
      ~src=
        "type Option = typfun A -> None + Some(A) in type Digit = Zero + One in let parse_digit = fun s : String -> case s | \"0\" => Some(Zero) | \"1\" => Some(One) | _ => None end in parse_digit(\"5\")",
      ~focus=ctor_arg_ap_id("One"),
      ~query="None + Some(Digit)",
      ~expected=
        "type ? = ? in type ? = ? in let ? = fun ? -> case ? | ? => ? | ? => ? | ? => ? end in ?",
    ),
    case(
      ~name="product query folds tuple binding",
      ~src="let x : (Int, Int) = (1, 2) in ?",
      ~focus=first_tuple_id,
      ~query="(Int, Int)",
      ~expected="let ? = ? in ?",
    ),
    test_case(
      "product query folds selected tuple binding",
      `Quick,
      () => {
        let code = code_model("let x : (Int, Int) = (1, 2) in ?");
        let tuple = first_tuple_id(code);
        let code = {
          ...code,
          editor: Editor.Model.mk(select_term(tuple, code), ~root=Exp),
        };
        let cursor_inspector = slicing_model(tuple, "(Int, Int)");
        let result =
          CI.ProgramFolds.apply_type_slice(
            ~info_map=code.statics.info_map,
            ~fallback_ci=None,
            ~cursor_inspector,
            code,
          );
        check(
          string,
          "selected tuple binding slice",
          "let ? = ? in ?",
          render_folded(result.model),
        );
      },
    ),
    test_case(
      "type editor statics parse as a type",
      `Quick,
      () => {
        let (_, editor) = CI.type_editor_of_type(parse_typ("Int -> Int"));
        let has_arrow_info =
          editor.statics.info_map
          |> Id.Map.bindings
          |> List.exists(((_, info)) =>
               switch (info) {
               | Info.InfoTyp({user_term, _}) =>
                 switch (Typ.term_of(user_term)) {
                 | Arrow(_, _) => true
                 | _ => false
                 }
               | _ => false
               }
             );
        check(bool, "arrow type has InfoTyp", true, has_arrow_info);
        let has_broken_exp =
          editor.statics.info_map
          |> Id.Map.bindings
          |> List.exists(((_, info)) =>
               switch (info) {
               | Info.InfoExp({user_term, _}) =>
                 switch (Exp.term_of(user_term)) {
                 | MultiHole(_) => true
                 | _ => false
                 }
               | _ => false
               }
             );
        check(bool, "no broken expression info", false, has_broken_exp);
      },
    ),
    test_case(
      "query type editor sees program aliases",
      `Quick,
      () => {
        let code = code_model("type Digit = A + B in (? : Digit)");
        let asc =
          info_exp_id(code, e =>
            switch (Exp.term_of(e)) {
            | Asc(_, _) => true
            | _ => false
            }
          );
        let ci =
          switch (Id.Map.find_opt(asc, code.statics.info_map)) {
          | Some(ci) => ci
          | None => fail("ascription info missing")
          };
        let row = CI.Model.refresh_row(Synthesizing, ci, CI.Model.empty_row);
        switch (row.editor) {
        | CI.Model.EditorSlot.SomeEditor(editor) =>
          let src = render_folded(editor);
          let contains = (needle, hay) => {
            let n = String.length(needle);
            let h = String.length(hay);
            let rec go = i =>
              i + n <= h && (String.sub(hay, i, n) == needle || go(i + 1));
            go(0);
          };
          check(
            bool,
            "query editor shows the alias name",
            true,
            contains("Digit", src),
          );
          check(
            bool,
            "alias in query editor is bound",
            true,
            editor.statics.error_ids == [],
          );
        | CI.Model.EditorSlot.NoEditor => fail("query editor missing")
        };
      },
    ),
    test_case(
      "folding arrow type editor is stable",
      `Quick,
      () => {
        let code = code_model("fun x : Int -> 1");
        let fn =
          info_exp_id(code, e =>
            switch (Exp.term_of(e)) {
            | Fun(_, _, _, _) => true
            | _ => false
            }
          );
        let ci =
          switch (Id.Map.find_opt(fn, code.statics.info_map)) {
          | Some(ci) => ci
          | None => fail("fun info missing")
          };
        let settings = {
          ...Web.Settings.Model.init,
          core: {
            ...Web.Settings.Model.init.core,
            flip_animations: false,
          },
        };
        let model =
          CI.Update.update(
            ~settings,
            ~cursor_info=Some(ci),
            Toggle(Synthesizing),
            CI.Model.init,
          ).
            model;
        let model =
          CI.Update.update(
            ~settings,
            ~cursor_info=Some(ci),
            TypeEditor(
              Synthesizing,
              Web.CodeEditable.Update.Perform(
                Action.Project(
                  Action.SetIndicated(Specific(ProjectorCore.Kind.Fold)),
                ),
              ),
            ),
            model,
          ).
            model;
        switch (model.syn.editor) {
        | CI.Model.EditorSlot.SomeEditor(editor) =>
          switch (CI.TypeSlicing.whole_fold_query(editor)) {
          | Some(_) => ()
          | None => fail("arrow type editor did not fold to a projector")
          }
        | CI.Model.EditorSlot.NoEditor => fail("folded arrow editor missing")
        };
      },
    ),
    test_case(
      "folding product type editor is stable",
      `Quick,
      () => {
        let (_, product_editor) =
          CI.type_editor_of_type(parse_typ("(Int, Int)"));
        switch (CI.TypeSlicing.typ_of_editor(product_editor)) {
        | Some(typ) when is_product_type(typ) => ()
        | _ => fail("product query editor did not parse as a type product")
        };
        let code = code_model("let x : (Int, Int) = (1, 2) in ?");
        let tuple = first_tuple_id(code);
        let ci =
          switch (Id.Map.find_opt(tuple, code.statics.info_map)) {
          | Some(ci) => ci
          | None => fail("tuple info missing")
          };
        let settings = {
          ...Web.Settings.Model.init,
          core: {
            ...Web.Settings.Model.init.core,
            flip_animations: false,
          },
        };
        let model =
          CI.Update.update(
            ~settings,
            ~cursor_info=Some(ci),
            Toggle(Synthesizing),
            CI.Model.init,
          ).
            model;
        let model =
          CI.Update.update(
            ~settings,
            ~cursor_info=Some(ci),
            TypeEditor(
              Synthesizing,
              Web.CodeEditable.Update.Perform(
                Action.Project(
                  Action.SetIndicated(Specific(ProjectorCore.Kind.Fold)),
                ),
              ),
            ),
            model,
          ).
            model;
        switch (CI.TypeSlicing.query_of_row(model.syn)) {
        | Some(_) => ()
        | None => fail("folded product query missing")
        };
        switch (model.syn.editor) {
        | CI.Model.EditorSlot.SomeEditor(editor) =>
          switch (
            Indicated.ci_of(
              editor.editor.state.zipper,
              editor.statics.info_map,
            )
          ) {
          | Some(InfoTyp(_)) => ()
          | _ => fail("folded product query info is not a type")
          }
        | CI.Model.EditorSlot.NoEditor =>
          fail("folded product editor missing")
        };
        let result =
          CI.ProgramFolds.apply_type_slice(
            ~info_map=code.statics.info_map,
            ~fallback_ci=None,
            ~cursor_inspector=model,
            code,
          );
        let _statics =
          CachedStatics.init(
            ~settings=CoreSettings.on,
            ~stitch=x => x,
            ~is_dynamic_term=false,
            ~root=Exp,
            result.model.editor.state.zipper,
          );
        check(
          string,
          "folded product type query slice",
          "let ? = ? in ?",
          render_folded(result.model),
        );
      },
    ),
    test_case(
      "folding product annotation is stable",
      `Quick,
      () => {
        let code = code_model("let x : (Int, Int) = (1, 2) in ?");
        let product_type = first_product_type_id(code);
        let result =
          CI.ProgramFolds.apply_folds(
            ~omitted=Id.Set.add(product_type, Id.Set.empty),
            code,
          );
        let _statics =
          CachedStatics.init(
            ~settings=CoreSettings.on,
            ~stitch=x => x,
            ~is_dynamic_term=false,
            ~root=Exp,
            result.model.editor.state.zipper,
          );
        let folded = render_folded(result.model);
        check(
          bool,
          "folded product annotation",
          true,
          folded == "let x : ? = (1, 2) in ?"
          || folded == "let x : (?) = (1, 2) in ?",
        );
      },
    ),
    test_case(
      "ana subpart fold applies with both rows active",
      `Quick,
      () => {
        let code = code_model("let x : (Int, Int) = (1, ?) in ?");
        let tuple = first_tuple_id(code);
        let ci =
          switch (Id.Map.find_opt(tuple, code.statics.info_map)) {
          | Some(ci) => ci
          | None => fail("tuple info missing")
          };
        let settings = {
          ...Web.Settings.Model.init,
          core: {
            ...Web.Settings.Model.init.core,
            flip_animations: false,
          },
        };
        let update = (action, model) =>
          CI.Update.update(~settings, ~cursor_info=Some(ci), action, model).
            model;
        let model =
          CI.Model.init
          |> update(Toggle(Synthesizing))
          |> update(Toggle(Analyzing));
        let ana_editor =
          switch (model.ana.editor) {
          | CI.Model.EditorSlot.SomeEditor(editor) => editor
          | CI.Model.EditorSlot.NoEditor => fail("ana editor missing")
          };
        let rec first_int_tile = (seg: Segment.t): option(Id.t) =>
          List.fold_left(
            (acc, piece: Piece.t) =>
              switch (acc, piece) {
              | (Some(_), _) => acc
              | (None, Tile({label: ["Int"], id, _})) => Some(id)
              | (None, Tile({children, _})) =>
                List.fold_left(
                  (acc, child) =>
                    switch (acc) {
                    | Some(_) => acc
                    | None => first_int_tile(child)
                    },
                  None,
                  children,
                )
              | (None, _) => None
              },
            None,
            seg,
          );
        let int_tile =
          switch (
            first_int_tile(
              ana_editor.editor.state.zipper |> Zipper.unselect_and_zip,
            )
          ) {
          | Some(id) => id
          | None => fail("no Int tile in ana editor")
          };
        let jump =
          switch (
            Web.CodeEditable.Selection.jump_to_tile(int_tile, ana_editor)
          ) {
          | Some(action) => action
          | None => fail("could not jump to Int tile")
          };
        let model = model |> update(TypeEditor(Analyzing, jump));
        let model =
          model
          |> update(
               TypeEditor(
                 Analyzing,
                 Web.CodeEditable.Update.Perform(
                   Action.Project(
                     Action.SetIndicated(Specific(ProjectorCore.Kind.Fold)),
                   ),
                 ),
               ),
             );
        let ana_query =
          switch (CI.TypeSlicing.query_of_row(model.ana)) {
          | Some(q) => q
          | None => fail("ana query missing")
          };
        let rec strip_parens = (typ: Typ.t): Typ.t =>
          switch (Typ.term_of(typ)) {
          | Parens(inner) => strip_parens(inner)
          | _ => typ
          };
        let query_shape =
          switch (Typ.term_of(strip_parens(ana_query))) {
          | Prod([first, second]) =>
            Statics.Slice.is_gap(first)
            && (
              switch (Typ.term_of(second)) {
              | Atom(Int) => true
              | _ => false
              }
            )
          | _ => false
          };
        check(bool, "ana query is (?, Int)", true, query_shape);
        let result =
          CI.ProgramFolds.apply_type_slice(
            ~info_map=code.statics.info_map,
            ~fallback_ci=None,
            ~cursor_inspector=model,
            code,
          );
        check(
          string,
          "combined slice respects ana subpart fold",
          "let ? : (?, Int) = (1, ?) in ?",
          render_folded(result.model),
        );
      },
    ),
    test_case(
      "explainthis slice examples",
      `Quick,
      () => {
        let typ = parse_typ("((Int, Int), Int, Int)");
        let render = t =>
          ExpToSegment.typ_to_segment(
            ~settings=Web.ExplainThis.slice_view_settings,
            t,
          )
          |> Printer.of_segment(
               ~holes="?",
               ~projector_to_segment=_ => [Piece.mk_grout(Convex)],
               _,
             );
        check(string, "full query", "((Int, Int), Int, Int)", render(typ));
        let examples =
          Web.ExplainThis.TypeSlicing.examples(typ)
          |> List.map(
               ({omitted, query}: Web.ExplainThis.TypeSlicing.example) =>
               (render(omitted), render(query))
             );
        check(
          list(pair(string, string)),
          "examples fold first subterm per depth, folds render as ?",
          [
            ("(Int, Int)", "(?, Int, Int)"),
            ("Int", "((?, Int), Int, Int)"),
          ],
          examples,
        );
        let folded_query =
          switch (
            Web.ExplainThis.TypeSlicing.examples(parse_typ("(Int, Int)"))
          ) {
          | [{query, _}, ..._] => query
          | [] => fail("no examples for (Int, Int)")
          };
        check(
          string,
          "folded current query",
          "(?, Int)",
          render(folded_query),
        );
        check(
          list(pair(string, string)),
          "folded query yields one example, skipping the fold",
          [("Int", "(?, ?)")],
          Web.ExplainThis.TypeSlicing.examples(folded_query)
          |> List.map(
               ({omitted, query}: Web.ExplainThis.TypeSlicing.example) =>
               (render(omitted), render(query))
             ),
        );
      },
    ),
    test_case(
      "explain error queries fold to first inconsistency",
      `Quick,
      () => {
        let ctx = Test_Statics_Slicing_Prelude.base_ctx();
        let queries = (syn, ana) =>
          switch (
            CI.ErrorSlicing.queries(ctx, parse_typ(syn), parse_typ(ana))
          ) {
          | Some((qs, qa)) => (render_typ(qs), render_typ(qa))
          | None => fail("expected a type inconsistency")
          };
        check(
          pair(string, string),
          "head clash folds children",
          ("? -> ?", "(?, ?)"),
          queries("Int -> Int", "(Int, Int)"),
        );
        check(
          pair(string, string),
          "nested clash keeps path, folds rest",
          ("(?, ?) -> ?", "(? -> ?) -> ?"),
          queries("(Int, Int) -> (Int, Int)", "(Int -> Int) -> (Int -> Int)"),
        );
        check(
          bool,
          "consistent types yield no queries",
          true,
          CI.ErrorSlicing.queries(
            ctx,
            parse_typ("Int -> ?"),
            parse_typ("Int -> Int"),
          )
          == None,
        );
      },
    ),
    test_case(
      "explain error activates both slicing rows",
      `Quick,
      () => {
        let code = code_model("let x : (Int, Int) = fun y -> y in ?");
        let fn =
          info_exp_id(code, e =>
            switch (Exp.term_of(e)) {
            | Fun(_, _, _, _) => true
            | _ => false
            }
          );
        let ci =
          switch (Id.Map.find_opt(fn, code.statics.info_map)) {
          | Some(ci) => ci
          | None => fail("fun info missing")
          };
        let settings = {
          ...Web.Settings.Model.init,
          core: {
            ...Web.Settings.Model.init.core,
            flip_animations: false,
          },
        };
        let model =
          CI.Update.update(
            ~settings,
            ~cursor_info=Some(ci),
            ExplainError,
            CI.Model.init,
          ).
            model;
        check(bool, "syn row active", true, model.syn.active);
        check(bool, "ana row active", true, model.ana.active);
        let row_query = (row: CI.Model.row) =>
          switch (CI.TypeSlicing.query_of_row(row)) {
          | Some(q) => render_typ(q)
          | None => fail("query missing")
          };
        check(string, "syn query", "? -> ?", row_query(model.syn));
        check(string, "ana query", "(?, ?)", row_query(model.ana));
        let result =
          CI.ProgramFolds.apply_type_slice(
            ~info_map=code.statics.info_map,
            ~fallback_ci=None,
            ~cursor_inspector=model,
            code,
          );
        check(
          string,
          "explain error slice",
          "let ? : (?, ?) = fun ? -> ? in ?",
          render_folded(result.model),
        );
      },
    ),
    test_case("anchor stays fixed while cursor moves", `Quick, () => {
      check(
        string,
        "? + ? + ?",
        "? + ? + ?",
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
        "fun ? -> ?",
        "fun ? -> ?",
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
