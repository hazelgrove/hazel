open Util;

/* Which expression ids the UI should mark as pending incremental
 * (re)evaluation: the top-level "leaf" expressions — not inside a function,
 * and not themselves chains (definition forms or seqs) — plus helpers for
 * clearing ids as their results stream in from the worker. */

/* Seq, or a definition form (DefForm: let, type, use, theorem, module,
   stepper filters): the worklist descends through these to their parts. */
let is_chain = (exp: Exp.t) =>
  switch (Exp.term_of(exp)) {
  | Seq(_) => true
  | _ => DefForm.is_def_form(exp)
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
        switch (Exp.term_of(parent), DefForm.parts(parent)) {
        | (Seq(d1, d2), _) =>
          Id.equal(id, Exp.rep_id(d1))
          || Id.equal(id, Exp.rep_id(d2))
          && !is_chain(user_term)
        /* a definition's evaluated value is a leaf; its body continues the
           chain. Type, theorem and filter headers aren't evaluated. */
        | (_, Some({value: Some(def), _}))
            when Id.equal(id, Exp.rep_id(def)) =>
          true
        | (_, Some({body, _})) =>
          Id.equal(id, Exp.rep_id(body)) && !is_chain(user_term)
        | (_, None) => false
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
