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

let is_inside_function = (info_map: StaticsBase.Map.t, info: Info.t) =>
  Info.ancestors_of(info)
  |> List.exists(ancestor_id =>
       switch (Id.Map.find_opt(ancestor_id, info_map)) {
       | Some(Info.InfoExp({user_term, _})) =>
         switch (Exp.term_of(user_term)) {
         | Fun(_)
         | TypFun(_) => true
         | _ => false
         }
       | _ => false
       }
     );

let is_top_level_leaf = (info_map: StaticsBase.Map.t, id: Id.t, info: Info.t) =>
  switch (info) {
  | Info.InfoExp({user_term, _}) when !is_inside_function(info_map, info) =>
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

let pending_ids = (info_map: StaticsBase.Map.t): list(Id.t) =>
  Id.Map.fold(
    (id, info, acc) =>
      is_top_level_leaf(info_map, id, info) ? [id, ...acc] : acc,
    info_map,
    [],
  );

let remove_streamed_ids =
    (stream: IncrEval.outbox(EvaluatorState.t), pending_ids) => {
  let completed_ids = IncrEval.visible_ids(stream.completed);
  List.filter(id => !List.exists(Id.equal(id), completed_ids), pending_ids);
};
