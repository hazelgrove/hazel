open Util;

/* Which expression ids the UI should mark as pending incremental
 * (re)evaluation: the top-level "leaf" expressions — not inside a function,
 * and not themselves let/seq chains — plus helpers for clearing ids as
 * their results stream in from the worker. */

let is_chain = (exp: Exp.t) =>
  switch (Exp.term_of(exp)) {
  | Let(_)
  | Seq(_) => true
  | _ => false
  };

let is_function = (info: Info.t): bool =>
  switch (info) {
  | Info.InfoExp({user_term, _}) =>
    switch (Exp.term_of(user_term)) {
    | Fun(_)
    | TypFun(_) => true
    | _ => false
    }
  | _ => false
  };

let ids_inside_functions = (info_map: StaticsBase.Map.t): Id.Set.t => {
  let memo = ref(Id.Map.empty);
  let rec is_inside = (id: Id.t): bool =>
    switch (Id.Map.find_opt(id, memo^)) {
    | Some(result) => result
    | None =>
      let result =
        switch (Id.Map.find_opt(id, info_map)) {
        | None => false
        | Some(info) =>
          switch (Info.parent_id_of(info)) {
          | None => false
          | Some(parent_id) =>
            switch (Id.Map.find_opt(parent_id, info_map)) {
            | Some(parent_info) when is_function(parent_info) => true
            | Some(_) => is_inside(parent_id)
            | None => false
            }
          }
        };
      memo := Id.Map.add(id, result, memo^);
      result;
    };
  Id.Map.fold(
    (id, _, acc) => is_inside(id) ? Id.Set.add(id, acc) : acc,
    info_map,
    Id.Set.empty,
  );
};

let is_top_level_leaf =
    (
      ~inside_function_ids: Id.Set.t,
      info_map: StaticsBase.Map.t,
      id: Id.t,
      info: Info.t,
    ) =>
  switch (info) {
  | Info.InfoExp({user_term, _}) when !Id.Set.mem(id, inside_function_ids) =>
    switch (Info.parent_id_of(info)) {
    | None => !is_chain(user_term)
    | Some(parent_id) =>
      switch (Id.Map.find_opt(parent_id, info_map)) {
      | Some(Info.InfoExp({user_term: parent, _})) =>
        switch (Exp.term_of(parent)) {
        | Let(_, def, body) =>
          Id.equal(id, Exp.rep_id(def))
          || Id.equal(id, Exp.rep_id(body))
          && !is_chain(user_term)
        | Seq(d1, d2) =>
          Id.equal(id, Exp.rep_id(d1))
          || Id.equal(id, Exp.rep_id(d2))
          && !is_chain(user_term)
        | _ => false
        }
      | _ => false
      }
    }
  | _ => false
  };

let pending_ids = (info_map: StaticsBase.Map.t): list(Id.t) => {
  let inside_function_ids = ids_inside_functions(info_map);
  Id.Map.fold(
    (id, info, acc) =>
      is_top_level_leaf(~inside_function_ids, info_map, id, info)
        ? [id, ...acc] : acc,
    info_map,
    [],
  );
};

let remove_streamed_ids =
    (stream: IncrEval.outbox(EvaluatorState.t), pending_ids) => {
  let completed_ids = IncrEval.visible_id_set(stream.completed);
  List.filter(id => !Id.Set.mem(id, completed_ids), pending_ids);
};
