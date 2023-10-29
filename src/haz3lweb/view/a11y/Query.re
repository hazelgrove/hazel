open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | CursorPos
  | CursorInfo
  | CursorMove
  | ContextInfo;

let to_string = (query: t): string =>
  switch (query) {
  | CursorPos => "CursorPos"
  | CursorInfo => "CursorInfo"
  | CursorMove => "CursorMove"
  | ContextInfo => "ContextInfo"
  };

let get_position = (id: Id.t, measured: Measured.t) =>
  switch (Measured.find_by_id(id, measured)) {
  | Some(m) => Some(m.last)
  | None => None
  };

let info_exp_str = (exp: Info.exp): string =>
  "Expression. " ++ (exp.cls |> Term.Cls.show) ++ ". " ++ (exp.ty |> Typ.show);

let info_pat_str = (pat: Info.pat): string =>
  "Pattern. " ++ (pat.cls |> Term.Cls.show) ++ ". " ++ (pat.ty |> Typ.show);

let info_typ_str = (typ: Info.typ): string =>
  "Type. " ++ (typ.cls |> Term.Cls.show);

let info_tpat_str = (tpat: Info.tpat): string =>
  "Type Pattern. " ++ (tpat.cls |> Term.Cls.show);

let info_str = (info: Info.t): string =>
  switch (info) {
  | InfoExp(exp) => info_exp_str(exp)
  | InfoPat(pat) => info_pat_str(pat)
  | InfoTyp(typ) => info_typ_str(typ)
  | InfoTPat(tpat) => info_tpat_str(tpat)
  };

let query_reply = (query: t, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let unselected = Zipper.unselect_and_zip(zipper);
  let (term, _) = MakeTerm.go(unselected);
  let info_map = Statics.mk_map(term);
  switch (query) {
  | CursorPos =>
    let index = Indicated.index(zipper);
    let position =
      switch (index) {
      | Some(id) => get_position(id, editor.state.meta.measured)
      | None => None
      };
    switch (position) {
    | Some({row, col}) =>
      "It is on row "
      ++ string_of_int(row)
      ++ " column "
      ++ string_of_int(col)
      ++ "."
    | None => "Error occurs when querying cursor position."
    };
  | CursorInfo =>
    switch (zipper.backpack, Indicated.index(zipper)) {
    | ([_, ..._], _) => "No information while backpack in use"
    | (_, None) => "No cursor in program"
    | (_, Some(id)) =>
      switch (Id.Map.find_opt(id, info_map)) {
      | None => "Whitespace or Comment"
      | Some(ci) => info_str(ci)
      }
    }
  | CursorMove =>
    // when the last action is moving up or down, we will read the full line, otherwise the character the cursor at
    let action =
      switch (editor.history) {
      | (_, []) => None
      | (_, [(action, _), ..._]) => Some(action)
      };

    let is_line_needed =
      switch (action) {
      | Some(Move(Extreme(Up)))
      | Some(Move(Extreme(Down)))
      | Some(Move(Local(Up)))
      | Some(Move(Local(Down))) => true
      | _ => false
      };

    let program = Printer.to_string_editor(editor);
    let rows = String.split_on_char('\n', program);
    switch (Editor.caret_point(editor)) {
    | {row, col} =>
      switch (List.nth_opt(rows, row)) {
      | Some(str) =>
        is_line_needed
          ? str
          : (
            switch (String.sub(str, min(col, String.length(str) - 1), 1)) {
            | s => s
            | exception (Invalid_argument(_)) => ""
            }
          )
      | None => ""
      }
    };
  | ContextInfo =>
    let context_entry = (entry: Haz3lcore.Ctx.entry) =>
      switch (entry) {
      | VarEntry({name, typ, _})
      | ConstructorEntry({name, typ, _}) =>
        name ++ " of type " ++ (typ |> Typ.show)
      | TVarEntry({name, kind, _}) =>
        name ++ " of kind " ++ (kind |> Kind.show)
      };
    switch (zipper.backpack, Indicated.index(zipper)) {
    | ([_, ..._], _) => ""
    | (_, None) => ""
    | (_, Some(id)) =>
      switch (Id.Map.find_opt(id, info_map)) {
      | None => ""
      | Some(ci) =>
        Haz3lcore.Info.ctx_of(ci)
        |> Haz3lcore.Ctx.filter_duplicates
        |> List.rev
        |> List.map(context_entry)
        |> String.concat("\n")
      }
    };
  };
};
