open Util;

type scope = {
  id: Id.t,
  binders: list(Id.t),
};

let binders = (p: Pat.t): list(Id.t) =>
  List.map(~f=(b: Binding.t) => b.id, Pat.bindings(p));

/* A recursive let's definition is in scope of its own binders, which the
   elaborated let shows by binding them around the definition. A let in
   function sugar, `let f(x) = ...`, binds the function name; its arguments
   are bound by the function the sugar stands for, which is in the map. */
let scopes_of = (info: Info.exp): list(scope) => {
  let scope = (id, p) =>
    switch (binders(p)) {
    | [] => None
    | binders =>
      Some({
        id,
        binders,
      })
    };
  switch (info.user_term.term) {
  | Let(p, _, body) =>
    let p =
      switch (FunctionSugar.detect(p)) {
      | Some((f_name, _, _)) => f_name
      | None => p
      };
    let recursive =
      switch (info.elab_term.term) {
      | Let(_, {term: FixF(_), _}, _) => true
      | _ => false
      };
    Option.to_list(
      scope(recursive ? Exp.rep_id(info.user_term) : Exp.rep_id(body), p),
    );
  | Fun(p, body, _, _) => Option.to_list(scope(Exp.rep_id(body), p))
  | Match(_, rules) =>
    List.filter_map(~f=((p, e)) => scope(Exp.rep_id(e), p), rules)
  | _ => []
  };
};

let of_map = (m: StaticsBase.Map.t): list(Blocks.t) => {
  let exps =
    Id.Map.fold(
      (id, info, acc) =>
        switch (info) {
        | Info.InfoExp(e) when Id.equal(id, Info.id_of(info)) => [e, ...acc]
        | _ => acc
        },
      m,
      [],
    );
  let scopes = List.concat_map(~f=scopes_of, exps);
  let scope_ids = Id.Set.of_list(List.map(~f=s => s.id, scopes));
  /* the scopes among `ids`; an ancestor can be listed twice (function
     sugar traverses its let twice) */
  let depth = (ids: list(Id.t)): int =>
    Id.Set.cardinal(Id.Set.inter(scope_ids, Id.Set.of_list(ids)));
  let depth_of = (id: Id.t): option(int) =>
    Option.map(
      ~f=info => depth(Info.ancestors_of(info)),
      StaticsBase.Map.lookup(id, m),
    );
  let depths: Id.Map.t(int) =
    List.fold(scopes, ~init=Id.Map.empty, ~f=(acc, s: scope) =>
      switch (depth_of(s.id)) {
      | Some(d) => Id.Map.add(s.id, d, acc)
      | None => acc
      }
    );
  let scope_of_binder: Id.Map.t(Id.t) =
    List.fold(scopes, ~init=Id.Map.empty, ~f=(acc, s: scope) =>
      List.fold(s.binders, ~init=acc, ~f=(acc, b) =>
        Id.Map.add(b, s.id, acc)
      )
    );
  let paint_of_binder = (b: Id.t): option(Blocks.paint) => {
    open OptUtil.Syntax;
    let* scope = Id.Map.find_opt(b, scope_of_binder);
    let+ d = Id.Map.find_opt(scope, depths);
    Blocks.Scope(d);
  };
  let block = (id, level, paint): Blocks.t => {
    id,
    level,
    paint,
  };
  let scope_blocks =
    Id.Map.fold(
      (id, d, acc) => [block(id, d, Blocks.Scope(d)), ...acc],
      depths,
      [],
    );
  let binder_blocks =
    List.concat_map(
      ~f=
        (s: scope) =>
          List.filter_map(
            ~f=
              b => {
                open OptUtil.Syntax;
                let* level = depth_of(b);
                let+ paint = paint_of_binder(b);
                block(b, level, paint);
              },
            s.binders,
          ),
      scopes,
    );
  let use_blocks =
    List.filter_map(
      ~f=
        (e: Info.exp) =>
          switch (e.user_term.term) {
          | Var(_) =>
            open OptUtil.Syntax;
            let* binder = Info.get_binding_site(InfoExp(e));
            let+ paint = paint_of_binder(binder);
            let id = Exp.rep_id(e.user_term);
            block(id, depth([id, ...e.ancestors]), paint);
          | _ => None
          },
      exps,
    );
  List.stable_sort(
    scope_blocks @ binder_blocks @ use_blocks,
    ~compare=(a: Blocks.t, b: Blocks.t) =>
    Int.compare(a.level, b.level)
  );
};
