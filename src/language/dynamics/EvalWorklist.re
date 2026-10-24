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
  /* tail-recursive walk up the parent chain, stopping at a memo hit,
     function, root, or cycle (some info maps' chains revisit an id) */
  let resolve = (id0: Id.t): bool => {
    let rec walk = (chain: list(Id.t), seen: Id.Set.t, id: Id.t): bool =>
      switch (Id.Map.find_opt(id, memo^)) {
      | Some(result) => result
      | None =>
        if (Id.Set.mem(id, seen)) {
          false; /* cycle: treat as not inside a function */
        } else {
          let chain = [id, ...chain];
          let seen = Id.Set.add(id, seen);
          switch (Id.Map.find_opt(id, info_map)) {
          | None => finish(chain, false)
          | Some(info) =>
            switch (Info.parent_id_of(info)) {
            | None => finish(chain, false)
            | Some(parent_id) =>
              switch (Id.Map.find_opt(parent_id, info_map)) {
              | Some(parent_info) when is_function(parent_info) =>
                finish(chain, true)
              | Some(_) => walk(chain, seen, parent_id)
              | None => finish(chain, false)
              }
            }
          };
        }
      }
    and finish = (chain: list(Id.t), result: bool): bool => {
      List.iter(id => memo := Id.Map.add(id, result, memo^), chain);
      result;
    };
    let result = walk([], Id.Set.empty, id0);
    /* cycle exits skip finish for the visited prefix: memoize id0 too */
    memo := Id.Map.add(id0, result, memo^);
    result;
  };
  Id.Map.fold(
    (id, _, acc) => resolve(id) ? Id.Set.add(id, acc) : acc,
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

/* mirrors the web-layer "Eval Progress" setting: when off, nothing
   renders the pending-eval highlight, so callers skip the worklist */
let compute_enabled = ref(true);

let pending_ids_uncached = (info_map: StaticsBase.Map.t): list(Id.t) => {
  let inside_function_ids = ids_inside_functions(info_map);
  Id.Map.fold(
    (id, info, acc) =>
      is_top_level_leaf(~inside_function_ids, info_map, id, info)
        ? [id, ...acc] : acc,
    info_map,
    [],
  );
};

/* single-slot memo keyed by map identity; the one info_map it pins is
   retained by the current statics anyway */
let pending_memo: Slot.t(StaticsBase.Map.t, list(Id.t)) = Slot.mk();
let pending_ids = (info_map: StaticsBase.Map.t): list(Id.t) =>
  Slot.get(pending_memo, info_map, () => pending_ids_uncached(info_map));

/* pending ids are top-level leaves, which record their own entries: an
   id settles exactly when its entry streams, so key membership suffices */
let remove_streamed_ids =
    (stream: IncrEval.outbox(EvaluatorState.t), pending_ids) =>
  List.filter(id => !Id.Map.mem(id, stream.completed.entries), pending_ids);
