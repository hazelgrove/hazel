open Util;
open Language;
open OptUtil.Syntax;

let binding_name =
    (binding_id: Id.t, info_map: Id.Map.t(Info.t)): option(string) => {
  switch (Id.Map.find_opt(binding_id, info_map)) {
  | Some(InfoPat({user_term: {term: Var(name), _}, _})) => Some(name)
  | _ => None
  };
};

let binding_owner =
    (binding_id: Id.t, info_map: Id.Map.t(Info.t)): option(Info.t) => {
  let rec walk = ancestors => {
    switch (ancestors) {
    | [] => None
    | [ancestor_id, ...rest] =>
      switch (Id.Map.find_opt(ancestor_id, info_map)) {
      | Some(InfoPat(_)) => walk(rest)
      | Some(InfoExp(_) as owner) => Some(owner)
      | _ => None
      }
    };
  };

  switch (Id.Map.find_opt(binding_id, info_map)) {
  | Some(InfoPat(_) as binding_info) =>
    walk(Info.ancestors_of(binding_info))
  | _ => None
  };
};

/* Renames variables in let bindings, including tuples.
 * Renaming function names and parameters is not supported yet. */
let reference_ids =
    (binding_id: Id.t, info_map: Id.Map.t(Info.t)): option(list(Id.t)) => {
  let* name = binding_name(binding_id, info_map);
  let* owner = binding_owner(binding_id, info_map);

  switch (owner) {
  | InfoExp({user_term, _}) =>
    switch (Exp.term_of(user_term)) {
    | Let(pattern, _, body)
        when Option.is_none(FunctionSugar.detect(pattern)) =>
      switch (Id.Map.find_opt(Exp.rep_id(body), info_map)) {
      | Some(InfoExp({co_ctx, _})) =>
        let entries =
          VarMap.lookup(co_ctx, name) |> Option.value(~default=[]);
        Some(List.map((entry: CoCtx.entry) => entry.id, entries));
      | _ => None
      }
    | _ => None
    }
  | _ => None
  };
};

let rename_piece =
    (ids: list(Id.t), new_name: string, piece: Piece.t): Segment.t => {
  switch (piece) {
  | Tile(tile) when List.mem(tile.id, ids) =>
    switch (tile.label, tile.children) {
    | ([_], []) => [
        Tile({
          ...tile,
          label: [new_name],
        }),
      ]
    | _ => [piece]
    }
  | _ => [piece]
  };
};

let rename_in_zipper =
    (ids: list(Id.t), new_name: string, z: Zipper.t): Zipper.t => {
  let z = Zipper.unselect(z);

  let renamed = ZipperBase.MapPiece.go(rename_piece(ids, new_name), z);

  switch (renamed.caret) {
  | Outer => renamed
  | Inner(index) =>
    switch (Zipper.Caret.nhbr_max_idx(Right, renamed)) {
    | Some(max_index) =>
      Zipper.Caret.set(Inner(min(index, max_index)), renamed)
    | None => Zipper.Caret.set(Outer, renamed)
    }
  };
};

let valid_name = (name: string): bool =>
  Token.is_var(name) && !Token.is_keyword(name);

let name_conflicts =
    (binding_id: Id.t, new_name: string, info_map: Id.Map.t(Info.t))
    : option(bool) => {
  let* old_name = binding_name(binding_id, info_map);
  let* owner = binding_owner(binding_id, info_map);

  if (String.equal(old_name, new_name)) {
    Some(false);
  } else {
    switch (owner) {
    | InfoExp({user_term, _}) =>
      Some(
        GeneralTreeUtils.name_occurs_within(
          ~root_id=Exp.rep_id(user_term),
          ~info_map,
          new_name,
        ),
      )
    | _ => None
    };
  };
};

let go =
    (
      binding_id: Id.t,
      new_name: string,
      info_map: Id.Map.t(Info.t),
      z: Zipper.t,
    )
    : option(Zipper.t) =>
  if (!valid_name(new_name)) {
    None;
  } else {
    let* old_name = binding_name(binding_id, info_map);

    if (String.equal(old_name, new_name)) {
      Some(z);
    } else {
      let* conflicts = name_conflicts(binding_id, new_name, info_map);

      if (conflicts) {
        None;
      } else {
        let* refs = reference_ids(binding_id, info_map);
        let ids = [binding_id, ...refs];
        Some(rename_in_zipper(ids, new_name, z));
      };
    };
  };

let target =
    (z: Zipper.t, info_map: Id.Map.t(Info.t)): option((Id.t, string)) => {
  let* info = Indicated.ci_of(z, info_map);

  let* binding_id =
    switch (info) {
    | InfoPat({user_term: {term: Var(_), _} as pattern, _}) =>
      Some(Pat.rep_id(pattern))
    | InfoExp({user_term: {term: Var(_), _}, _}) =>
      Info.get_binding_site(info)
    | _ => None
    };

  let* name = binding_name(binding_id, info_map);
  let* _refs = reference_ids(binding_id, info_map);
  Some((binding_id, name));
};
