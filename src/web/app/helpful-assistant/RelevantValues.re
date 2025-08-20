open Language;
open Util;

/* Assembles relevant value headers per the Static Contextualization paper */

[@deriving (show({with_path: false}), sexp, yojson)]
type filtered_entry = {
  name: string,
  typ: Typ.t,
  matched_type: Typ.t,
  depth: int,
};

/* Returns the ratio of type nodes which are the Unknown
   constructor. Must recurse and gather results from composite nodes */
let unknown_ratio = (ty: Typ.t): float => {
  let total = float_of_int(Typ.num_nodes(ty));
  let unknowns = float_of_int(Typ.count_unknowns(ty));
  (total -. unknowns) /. total;
};

let score_type = (ty: Typ.t): float => {
  let unk_ratio = unknown_ratio(ty);
  Typ.is_atom(ty) ? 0.8 : unk_ratio;
};

let filter_ctx = (ctx: Ctx.t, ty_expect: Typ.t): list(filtered_entry) =>
  List.filter_map(
    fun
    | Ctx.VarEntry({typ, name, _})
        when Typ.is_consistent(ctx, ty_expect, typ) =>
      Some({
        name,
        typ,
        depth: 0,
        matched_type: typ,
      })
    | Ctx.VarEntry({
        typ: {term: {typ: Arrow(_, return_ty), _}, _} as typ,
        name,
        _,
      })
        when Typ.is_consistent(ctx, ty_expect, return_ty) =>
      Some({
        name,
        typ,
        matched_type: return_ty,
        depth: 1,
      })
    | Ctx.VarEntry({
        typ:
          {
            term:
              {
                typ: Arrow(_, {term: {typ: Arrow(_, return_ty), _}, _}),
                _,
              },
            _,
          } as typ,
        name,
        _,
      })
        when Typ.is_consistent(ctx, ty_expect, return_ty) =>
      Some({
        name,
        typ,
        matched_type: return_ty,
        depth: 2,
      })
    | _ => None,
    ctx.entries,
  );

let primary_goal = (ctx: Ctx.t, ana: Typ.t): Typ.t =>
  switch (ana) {
  | {term: {typ: Var(name), _}, _} when Ctx.lookup_alias(ctx, name) != None =>
    let ty_expanded = Ctx.lookup_alias(ctx, name) |> Option.get;
    ty_expanded |> Typ.normalize(ctx);
  | _ => ana |> Typ.normalize(ctx)
  };

let secondary_targets = (goal: Typ.t): list(Typ.t) =>
  switch (Typ.term_of(goal)) {
  | Arrow(_source, target) =>
    let elems =
      switch (Typ.term_of(target)) {
      | Prod(ts) => ts
      | _ => []
      };
    [target] @ elems;
  | _ => []
  };

let entries = (ctx: Ctx.t, ana: Typ.t): list(filtered_entry) => {
  let primary_goal = primary_goal(ctx, ana);
  let secondary_targets = secondary_targets(primary_goal);
  List.concat_map(filter_ctx(ctx), secondary_targets @ [primary_goal])
  |> Util.ListUtil.dedup
  |> List.sort((t1, t2) =>
       compare(score_type(t2.matched_type), score_type(t1.matched_type))
     )
  |> List.filter(entry => Typ.contains_sum_or_var(entry.typ));
};

let format_def = ({name, typ, _}: filtered_entry) =>
  Printf.sprintf(
    "let %s: %s =  in",
    name,
    Haz3lcore.ErrorPrint.Print.typ(typ),
  );

/* Returns a list of relevant values headers formatted as let definitions */
let get = (ctx: Ctx.t, ana: Typ.t): string =>
  "# Consider using these variables relevant to the expected type: #\n"
  ++ (
    entries(ctx, ana)
    |> ListUtil.take_up_to_n(8)
    |> List.map(format_def)
    |> String.concat("\n")
  );
